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
#' @return edicred - a list containing EDI credentials
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
#' `load_metabase_cred` and `load_destination_cred` functions), and
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
  warning(paste('This function (update_dataset_edi) is now deprecated and will', 
                'be removed from a future version of jerald. Use',
                'publish_dataset_edi instead'), immediate=T)
  
  # Collect metadata into EML list from Metabase (using MetaEgress)
  eml.list <- eml_egress(datasetid, mb.name, mb.cred)
  # Revision number in metabase
  rev.mb <- unlist(strsplit(eml.list$packageId, ".", fixed=TRUE))[3]
  rev.mb <- as.numeric(rev.mb)
  
  # Update the revision numbers using EDI
  eml.list.new <- increment_edi_revision(eml.list, edi.env=edi.env)
  rev.next <- unlist(strsplit(eml.list.new$packageId, ".", fixed=TRUE))[3]
  rev.next <- as.numeric(rev.next)
  if (rev.next==1){
    stop("To create a new package in EDI ", edi.env, ", use the 
         `create_dataset_edi` function")
  }
  # Warn if the revisions on metabase and EDI don't match
  #if (rev.mb!=(rev.next-1)){
  #  warning("The metabase revision (", rev.mb, "), does not match the EDI ",
  #          edi.env, "revision (", rev.next-1, ").")
  #}
  
  # Validate and serialize (write) EML document
  message('Validating EML...')
  out <- EML::eml_validate(eml.list.new)
  message(out)
  message('Writing EML...')
  emlfile <- paste0(eml.list.new$packageId, ".xml")
  EML::write_eml(eml.list.new, file=emlfile)
  message('Done.\n')
  
  if (!publish){
    message('Stopping execution: the dataset will not be published')
    message('Please check dataset identifiers, revision numbers, eml, etc.')
    message('To continue to publication pass argument `publish=TRUE`. \n')
    stop("Stopping", call.=FALSE)
  }
  
  # Collect the data entities from the eml list & push to s3 bucket
  ents <- get_eml_entities(eml.list.new)
  ents_to_s3(ents, bucket.name)
  
  # Update package on EDI
  edi_update_package(emlfile, edi.cred, edi.env=edi.env)
}


#' Create a dataset at the EDI repository
#'
#' This function creates a new dataset (or "data package") at the EDI
#' research data repository using metadata derived from an LTER Metabase. 
#' The user must supply credentials for the metabase and EDI (see 
#' `load_metabase_cred` and `load_destination_cred` functions), and
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
  warning(paste('This function (create_dataset_edi) is now deprecated and will', 
          'be removed from a future version of jerald. Use',
          'publish_dataset_edi instead'), immediate=T)
  
  # Collect metadata into EML list from Metabase (using MetaEgress)
  eml.list <- eml_egress(datasetid, mb.name, mb.cred)
  # Revision number in metabase
  rev.mb <- unlist(strsplit(eml.list$packageId, ".", fixed=TRUE))[3]
  rev.mb <- as.numeric(rev.mb)
  
  # Update the revision numbers using EDI
  eml.list.new <- increment_edi_revision(eml.list, edi.env=edi.env)
  rev.next <- unlist(strsplit(eml.list.new$packageId, ".", fixed=TRUE))[3]
  rev.next <- as.numeric(rev.next)
  if (rev.next>1){
    stop("This package already exists at EDI ", edi.env, ". Use the
         `update_dataset_edi` function")
  }
  # Warn if the revisions on metabase and EDI don't match
  #if (rev.mb != (rev.next-1)){
  #  warning("The metabase revision (", rev.mb, "), does not match the EDI ",
  #          edi.env, "revision (", rev.next-1, ").")
  #}
  
  # Validate and serialize (write) EML document
  message('Validating EML...')
  out <- EML::eml_validate(eml.list.new)
  message(out)
  message('Writing EML...')
  emlfile <- paste0(eml.list.new$packageId, ".xml")
  EML::write_eml(eml.list.new, file=emlfile)
  message('Done.\n')
  
  if (!publish){
    message('Stopping execution: the dataset will not be published')
    message('Please check dataset identifiers, revision numbers, eml, etc.')
    message('To continue to publication pass argument `publish=TRUE`. \n')
    stop("Stopping", call.=FALSE)
  }
  
  # Collect the data entities from the eml list & push to s3 bucket
  ents <- get_eml_entities(eml.list.new)
  ents_to_s3(ents, bucket.name)
  
  # Update package on EDI
  edi_create_package(emlfile, edi.cred, edi.env=edi.env)
}


#' Publish a dataset at the EDI repository
#'
#' This function publishes a dataset (or "data package") at the EDI
#' research data repository using metadata derived from an LTER Metabase. 
#' The user must supply credentials for the metabase and EDI (see 
#' `load_metabase_cred` and `load_destination_cred` functions), and
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
#' @param dry.run boolean value - write EML only, then stop (end before s3 
#' and EDI upload) if TRUE, continue to publish if FALSE
#' @param s3.upload boolean value - if TRUE upload to the s3 bucket, if FALSE
#' skip this (entities already there). Note that this does not currently do a 
#' check on whether entities are present or not. 
#' @param bucket.name name of the s3 bucket to push data entities to
#' @export
publish_dataset_edi <- function(datasetid,
                               mb.name,
                               mb.cred,
                               edi.cred,
                               edi.env='staging',
                               dry.run=TRUE,
                               s3.upload=TRUE,
                               multi.part=FALSE,
                               bucket.name=Sys.getenv('AWS_S3_BUCKETNAME')){
  
  # Collect metadata into EML list from Metabase (using MetaEgress)
  eml.list <- eml_egress(datasetid, mb.name, mb.cred)
  
  # Update the revision numbers using EDI
  eml.list.new <- increment_edi_revision(eml.list, edi.env=edi.env)
  rev.next <- parse_edi_pid(eml.list.new, 'revision')
  if (rev.next>1){
    pubflag <- 'update'
  } else {
    pubflag <- 'create'
  }
  
  # Validate and serialize (write) EML document
  emlfile <- paste0(eml.list.new$packageId, ".xml")
  eml_serialize(eml.list.new, emlfile)
  
  if (dry.run){
    message('Stopping because this is a dry run')
    message('Please check dataset identifiers, revision numbers, eml, etc.')
    message('To continue to publication pass argument `dry.run=FALSE`. \n')
    stop("Stopping (dry run)", call.=FALSE)
  }
  
  if (s3.upload){
    # Collect the data entities from the eml list & push to s3 bucket
    ents <- get_eml_entities(eml.list.new)
    ents_to_s3(ents, bucket.name, multi.part=multi.part)
  } else {
    message('Skipping S3 upload: make sure data entity files are online ')
    message('at the URL designated in <distribution>. \n')
  }
  # Create or update dataset
  if (pubflag=='update'){
    edi_update_package(emlfile, edi.cred, edi.env=edi.env)
  } else {
    edi_create_package(emlfile, edi.cred, edi.env=edi.env)
  }
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
  
  datasetid <- format(datasetid, scientific = FALSE)
  
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
  jerald.templates <- c('README.md', 'abstract.999.md',
                      'methods.999.md', 'build_dataset.999.R',
                      'build_eml.999.R')
  # Empty list to collect dataset templates
  dataset.templates <- c()
  # Get jerald templates, rename, and copy to new.dir
  for (t in jerald.templates){
    j.temp <- system.file('template', t, package='jerald')
    new.temp <- sub('999', as.character(datasetid), t)
    new.temp <- file.path(new.dir, new.temp)
    file.copy(j.temp, new.temp)
    dataset.templates <- c(dataset.templates, new.temp)
  }

  # Edit the dataset template files to add the datasetid
  for (t in dataset.templates){
    message('Creating template: ', t)
    x <- readLines(t)
    y <- gsub("ds999", paste0('ds', as.character(datasetid)), x, fixed=TRUE)
    y2 <- gsub("dataset.999", paste0('dataset.', as.character(datasetid)),
	       y, fixed=TRUE)    
    cat(y2, file=t, sep="\n")
  }
  message('Done.\n')
}

#' Migrate an EAL dataset directory to a jerald directory
#'
#' WARNING - this is fairly jornada-specific use at your own risk
#'
#' This will move the files from an EMLassemblyline-formatted dataset directory
#' to a jerald-formatted dataset directory (as described in
#' `jerald/inst/template/`). The jerald-formatted destination should be
#' created first using `jerald::template_dataset_dir()` using a name distinct
#' from the source directory.
#'
#' All files from the EAL source directory will be copied into a new
#' `EAL_archive/` directory in the jerald destination directory. Then, this
#' function copies any "build" R scripts and data entities into the top level
#' of the jerald directory, moves old metadata files (`.dsd`, etc) into 
#' `metadata_docs/`, and moves EML files to `eml/`.
#'
#' Optionally you may remove the source dataset directory at the end but
#' YOU SHOULD VERY CAREFULLY CHECK THE OUTPUT BEFORE ANSWERING YES! 
#'
#' @param eal.dir Source dataset directory, in EAL format, to migrate from
#' @param jerald.dir Destination dataset directory, in jerald format, to
#'                   migrate to
#' @export
#' 
migrate_eal_dir <- function(eal.dir, jerald.dir){
  
  message('\nWARNING - this function may remove data - be careful!!!')
  user.continue <- readline('Do you want to continue? (Y/n): ')
  if (tolower(user.continue)!='y'){
    stop('Aborting...')
  } else {
    message('continuing...\n')
  }
  
  # Expand paths
  eal.dir <- path.expand(eal.dir)
  jerald.dir <- path.expand(jerald.dir)
  
  # Make sure both directories exist
  if (!dir.exists(eal.dir)){
    stop(eal.dir, ' does not exist.')
  }
  if (!dir.exists(jerald.dir)){
    stop(jerald.dir, ' does not exist.\nCreate a jerald template directory',
        ' before migrating.')
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
  message('Copying all files from EAL_source to jerald_dest/EAL_archive/...')
  file.copy(eal.files, eal.archive, recursive=TRUE, copy.date=TRUE,
            copy.mode=TRUE)
  message('Done.\n')
  
  # Move any data entities or warn if not found
  eal.entities <- file.path(eal.archive, 'data_entities')
  if (dir.exists(eal.entities)){
    eal.dataents <- list.files(eal.entities, full.names=TRUE)
    message('Move EAL data entities to parent/...')
    file.copy(eal.dataents, jerald.dir, copy.date=TRUE, copy.mode=TRUE)
    file.remove(eal.dataents)
    message('Done.\n')
  }else{
    message('Data entities directory not found!')
  }

  # Copy build script to top level
  eal.buildscript <- list.files(eal.archive, pattern="(build_).*\\.R$",
                                full.names=TRUE)
  print(eal.buildscript)
  message('Move EAL build script to parent/...')
  file.copy(eal.buildscript, file.path(jerald.dir, 'build_EALarchive.R'),
            copy.date=TRUE, copy.mode=TRUE)
  file.remove(eal.buildscript)
  message('Done.\n')
  
  # Copy metadata files to metadata_docs/
  eal.metadatafiles <- list.files(eal.archive,
                                  pattern="\\.(prj|dsd|his|PRJ|DSD|HIS)",
                                  full.names=TRUE, recursive=TRUE)
  print(eal.metadatafiles)
  message('Move prj, dsd, and his files to metadata_docs/...')
  file.copy(eal.metadatafiles, file.path(jerald.dir, 'metadata_docs'),
            copy.date=TRUE, copy.mode=TRUE)
  file.remove(eal.metadatafiles)
  message('Done.\n')

  # Copy metadata templates to metadata_docs/
  eal.metadatatemp <- list.files(eal.archive,
                                 pattern="(metadata_template).*\\.(docx|xlsx)",
                                 full.names=TRUE, recursive=TRUE)
  print(eal.metadatatemp)
  message('Move metadata_template files to metadata_docs/...')
  file.copy(eal.metadatatemp, file.path(jerald.dir, 'metadata_docs'),
            copy.date=TRUE, copy.mode=TRUE)
  file.remove(eal.metadatatemp)
  message('Done.\n')

  # Copy other R files to parent
  eal.rscripts <- list.files(eal.archive,pattern="\\.R$",
                             full.names=TRUE)
  if(length(eal.rscripts > 0)){
    print(eal.rscripts)
    message('Additional R scripts are being moved to parent/')
    file.copy(eal.rscripts, file.path(jerald.dir),
              copy.date=TRUE, copy.mode=TRUE)
    file.remove(eal.rscripts)
    message('Done.\n')
  }

  # Copy EML files to eml (anything ending with .xml)
  eal.EML <- list.files(eal.archive, pattern="\\.xml$",
                        full.names=TRUE, recursive=TRUE)
  print(eal.EML)
  message('Move EML files to eml/...')
  file.copy(eal.EML, file.path(jerald.dir, 'EML'),
            copy.date=TRUE, copy.mode=TRUE)
  file.remove(eal.EML)
  message('Done.\n')
  
  
  # Remove the old directory?
  user.remove <- readline(paste0('Remove the EAL_source directory? ',
                                 '(check results first!) (Y/n): '))
  if (tolower(user.continue)!='y'){
    message('Removing ', eal.dir, ' ...')
    unlink(eal.dir, recursive = TRUE)
    message('Done.')
  }
}
