#' Get LTER Metabase credentials
#'
#' This function reads credentials for an LTER Metabase into the mbcred
#' list variable.
#'
#' @param pathname Complete path (including filename) to your metabase
#' credentials
#' @return mbcred - a list containing metabase credentials
#' @export
load_metabase_cred <- function(pathname){
  # Source the file
  source(pathname)
}

#' Get dataset destination credentials
#'
#' This function reads credentials for any data repositories and s3 buckets
#' in a jerald destination credentials file.
#'
#' @param path Path to the 'destination_keys.R' file containing API keys
#' @return 1 if successful
#' @export
load_destination_cred <- function(path){
  # Source the file
  source(paste0(path, 'jerald_destination_keys.R'))
}

#' Update a dataset at the EDI repository
#'
#' This function updates an existing dataset (or "data package") at the EDI
#' research data repository using metadata derived from an LTER Metabase. 
#' The user must supply credentials for the metabase and EDI (see 
#' `load_metabase_cred` and `load_destination_dred` functions), and
#' appropriate database and EDI environment names.
#' 
#' The basic process is to
#' 
#' 1. Pull metadata for the dataset from the metabase using `MetaEgress`
#' 2. Query for the current dataset revision in the EDI environment
#' 3. Write an EML document for the next revision to go to EDI
#' 4. Push EML data entities from the working directory to an s3 bucket
#' 5. Push the EML document to EDI, which triggers PASTA to pull data from
#'    the s3 bucket and update the data package.
#'
#' @param datasetid ID number of the dataset to find in metabase and 
#' update in EDI
#' @param mb.cred list of credentials for the metabase postgres cluster
#' @param mb.name name of the metabase database in the postgres cluster
#' @param edi.cred list of credentials to use for EDI
#' @param edi.env name of the EDI environment to update (staging, production,
#' or development)
#' @param publish boolean value - publish if TRUE, end before s3 upload if 
#' FALSE
#' @param bucket.name name of the s3 bucket to push data entities to
#' @export
update_dataset_edi <- function(datasetid,
                               mb.name,
                               mb.cred,
                               edi.cred,
                               edi.env='staging',
                               publish=FALSE,
                               bucket.name=Sys.getenv('AWS_S3_BUCKETNAME')){
  
  # Collect metadata into EML list from JRN metabase (using MetaEgress)
  eml.list <- eml_from_mb(datasetid, mb.name, mb.cred)
  # Revision number in metabase
  rev.mb <- unlist(strsplit(eml.list$packageID, ".", fixed=TRUE))[3]
  
  # Update the revision numbers using EDI
  eml.list.new <- update_eml_revnum_edi(eml.list, edi.env=edi.env)
  rev.next <- unlist(strsplit(eml.list.new$packageID, ".", fixed=TRUE))[3]
  if (rev.next==1){
    stop("To create a new package in EDI ", edi.env, ", use the 
         `create_dataset_edi` function")
  }
  # Warn if the revisions on metabase and EDI don't match
  if (rev.mb != (rev.next-1)){
    warning("The metabase revision (", rev.mb, "), does not match the EDI ",
            edi.env, "revision (", rev.next-1, ").")
  }
  
  # validate and serialize (write) EML document
  message('Writing EML...')
  EML::eml_validate(eml.list.new)
  EML::write_eml(eml.list.new, file = paste0(eml.list.new$packageId, ".xml"))
  message('Done.\n')
  
  if (!publish){
    stop("Stopping - to continue to publication pass argument `publish=TRUE`")
  }
  
  # Collect the data entities from the eml list & push to s3 bucket
  ents <- get_eml_entities(eml.list.new)
  ents_to_s3(ents, bucket.name)
  
  # Update package on EDI
  edi_update_package(eml.list.new$packageId, edi.cred, edi.env=edi.env)
}


#' Create a dataset at the EDI repository
#'
#' This function creates a new dataset (or "data package") at the EDI
#' research data repository using metadata derived from an LTER Metabase. 
#' The user must supply credentials for the metabase and EDI (see 
#' `load_metabase_cred` and `load_destination_dred` functions), and
#' appropriate database and EDI environment names.
#' 
#' The basic process is to
#' 
#' 1. Pull metadata for the dataset from the metabase using `MetaEgress`
#' 2. Query for the current dataset revision in the EDI environment
#' 3. Write an EML document for the next revision to go to EDI
#' 4. Push EML data entities from the working directory to an s3 bucket
#' 5. Push the EML document to EDI, which triggers PASTA to pull data from
#'    the s3 bucket and update the data package.
#'
#' @param datasetid ID number of the dataset to find in metabase and 
#' update in EDI
#' @param mb.cred list of credentials for the metabase postgres cluster
#' @param mb.name name of the metabase database in the postgres cluster
#' @param edi.cred list of credentials to use for EDI
#' @param edi.env name of the EDI environment to update (staging, production,
#' or development)
#' @param publish boolean value - publish if TRUE, end before s3 upload if 
#' FALSE
#' @param bucket.name name of the s3 bucket to push data entities to
#' @export
create_dataset_edi <- function(datasetid,
                               mb.name,
                               mb.cred,
                               edi.cred,
                               edi.env='staging',
                               publish=FALSE,
                               bucket.name=Sys.getenv('AWS_S3_BUCKETNAME')){
  
  # Collect metadata into EML list from JRN metabase (using MetaEgress)
  eml.list <- eml_from_mb(datasetid, mb.name, mb.cred)
  # Revision number in metabase
  rev.mb <- unlist(strsplit(eml.list$packageID, ".", fixed=TRUE))[3]
  
  # Update the revision numbers using EDI
  eml.list.new <- update_eml_revnum_edi(eml.list, edi.env=edi.env)
  rev.next <- unlist(strsplit(eml.list.new$packageID, ".", fixed=TRUE))[3]
  if (rev.next>1){
    stop("This package already exists at EDI ", edi.env, ". Use the
         `update_dataset_edi` function")
  }
  # Warn if the revisions on metabase and EDI don't match
  if (rev.mb != (rev.next-1)){
    warning("The metabase revision (", rev.mb, "), does not match the EDI ",
            edi.env, "revision (", rev.next-1, ").")
  }
  
  # validate and serialize (write) EML document
  message('Writing EML...')
  EML::eml_validate(eml.list.new)
  EML::write_eml(eml.list.new, file = paste0(eml.list.new$packageId, ".xml"))
  message('Done.\n')
  
  if (!publish){
    stop("Stopping - to continue to publication pass argument `publish=TRUE`")
  }
  
  # Collect the data entities from the eml list & push to s3 bucket
  ents <- get_eml_entities(eml.list.new)
  ents_to_s3(ents, bucket.name)
  
  # Update package on EDI
  edi_create_package(eml.list.new$packageId, edi.cred, edi.env=edi.env)
}

#' Create a dataset directory from a jerald template
#'
#' Create a new directory for a dataset with some template scripts,
#' metadata files, useful subdirectories, and a readme file. Template
#' files are in `jerald/inst/template/`.
#'
#' @param datasetid ID number of the dataset to find in metabase and 
#' update in EDI
#' @export
#' 
template_dataset_dir <- function(datasetid, get.edi=FALSE){
  
  # Create dataset directory name
  user.shortname <- readline(paste0('Enter a short name to append to the ',
                             'dataset directory: '))
  dir.name <- paste(datasetid, user.shortname, sep='_')
  
  # Choose parent directory
  cap = 'Select a parent folder for the dataset directory'
  #user.destdir <- rstudioapi::selectDirectory(caption=cap)
  user.destdir <- tcltk::tk_choose.dir(caption=cap)
  
  # Create it
  new.dir <- file.path(path.expand(user.destdir), dir.name)
  message('Creating directory ', new.dir)
  if (!dir.exists(new.dir)){
    dir.create(new.dir)
  } else if (dir.exists(new.dir)){
    stop('Dataset directory already exists. Choose another name.')
  }
  
  # Create subdirectories
  for (sub in c('eml','source_data', 'metadata_docs')){
    subdir = file.path(new.dir, sub)
    if (!dir.exists(subdir)){
      dir.create(subdir)
    }
  }
  
  # List of jerald template names
  jerald.templates <- c('README.md', 'abstract.datasetid.md',
                      'methods.datasetid.md', 'build_dataset.datasetid.R',
                      'build_eml.datasetid.R')
  # Empty list to collect dataset templates
  dataset.templates <- c()
  # Get jerald templates, rename, and copy to new.dir
  for (t in jerald.templates){
    j.temp <- system.file('template', t, package='jerald')
    new.temp <- sub('datasetid', as.character(datasetid), t)
    new.temp <- file.path(new.dir, new.temp)
    file.copy(j.temp, new.temp)
    dataset.templates <- c(dataset.templates, new.temp)
  }

  # Edit the dataset template files to add the datasetid
  for (t in dataset.templates){
    message('Creating template: ', t)
    x <- readLines(t)
    y <- gsub("210000000", as.character(datasetid), x, fixed=TRUE)
    cat(y, file=t, sep="\n")
  }
  message('Done.\n')
}

#' Migrate an EAL dataset directory to a jerald directory
#'
#' Create a new directory for a dataset with some template scripts,
#' metadata files, useful subdirectories, and a readme file. Template
#' files are in `jerald/inst/template/`.
#'
#' @param eal.dir Dataset directory, in EAL format, to migrate from
#' @param jerald.dir Dataset directory, in jerald format, to migrate to
#' @export
#' 
migrate_eal_dir <- function(eal.dir, jerald.dir){
  
  message('WARNING - this function may remove data - be careful!')
  user.continue <- readline('Do you want to continue? (Y/n): ')
  if (user.continue!='Y'){
    stop('Aborting...')
  } else {
    message('continuing...')
  }
  
  # Expand paths
  eal.dir <- path.expand(eal.dir)
  jerald.dir <- path.expand(jerald.dir)
  
  # Make sure both directories exist
  if (!dir.exists(eal.dir)){
    stop(eal.dir, ' does not exist.')
  }
  if (!dir.exists(jerald.dir)){
    stop(jerald.dir, ' does not exist.')
  } else {
    # If the jerald directory exists and has no EAL archive,
    # create one. If it has an EAL archive, abort.
    eal.archive <- file.path(jerald.dir, 'EAL_archive')
    if (!dir.exists(eal.archive)){
      dir.create(eal.archive)
    } else if (dir.exists(eal.archive)){
      stop(eal.archive, '\n already exists! Aborting.')
    }
  }
  
  # List EAL directory contents
  eal.files <- list.files(eal.dir, full.names=TRUE, include.dirs = TRUE)
  # Copy all files to EAL_archive
  message('Copying all files from EAL directory to jerald EAL_archive...')
  file.copy(eal.files, eal.archive, recursive=TRUE, copy.date=TRUE,
            copy.mode=TRUE)
  message('Done.\n')
  
  eal.entities <- file.path(eal.archive, 'data_entities')
  if (dir.exists(eal.entities)){
    eal.dataents <- list.files(eal.entities, full.names=TRUE)
    message('Copy EAL data entities to parent/...')
    file.copy(eal.dataents, jerald.dir, copy.date=TRUE, copy.mode=TRUE)
    message('Done.\n')
  }
  # Copy build script to top level
  eal.buildscript <- list.files(eal.archive, pattern="(build_)*\\.(R)",
                                full.names=TRUE)
  message('Copy EAL build script to parent/...')
  file.copy(eal.buildscript, file.path(jerald.dir, 'build_EALarchive.R'),
            copy.date=TRUE, copy.mode=TRUE)
  message('Done.\n')
  
  # Copy metadata files to metadata_docs/
  eal.metadatafiles <- list.files(eal.archive,
                                  pattern="\\.(prj|dsd|his|PRJ|DSD|HIS)",
                                  full.names=TRUE)
  message('Copy prj, dsd, and his files to metadata_docs/...')
  file.copy(eal.metadatafiles, file.path(jerald.dir, 'metadata_docs'),
            copy.date=TRUE, copy.mode=TRUE)
  message('Done.\n')
  
  # Remove the old directory?
  user.remove <- readline(paste0('Remove the EAL dataset directory? ',
                                 '(check results first!) (Y/n): '))
  if (user.remove=='Y'){
    message('Removing ', eal.dir, ' ...')
    unlink(eal.dir, recursive = TRUE)
    message('Done.')
  }
  
}
  