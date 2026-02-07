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

#' Connect to an LTER Metabase
#'
#' @param mbcred_path Path to a file containing credentials for an LTER Metabase instance
#' @return A connection to the LTER Metabase
#' @export
metabase_connect <- function(mbcred_path) {
    # Get metabase credentials (includes host, port, user, pwd, and dbname)
    load_metabase_cred(mbcred_path)

    # Get postgres driver and connect to the metabase
    driver <- RPostgres::Postgres()
    
    conn <- RPostgres::dbConnect(
        drv = driver,
        dbname = mbname,
        host = mbcred$host,
        port = mbcred$port,
        user = mbcred$user,
        password = mbcred$password)
        
    return(conn)
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
#' @param mb_cred list of credentials for the metabase postgres cluster
#' @param mb_name name of the metabase database in the postgres cluster
#' @param edi_cred list of credentials to use for EDI
#' @param edi_env name of the EDI environment to update (staging, production,
#' or development)
#' @param publish boolean value - publish if TRUE, end before s3 upload if 
#' FALSE
#' @param bucket_name name of the s3 bucket to push data entities to
#' @export
update_dataset_edi <- function(datasetid,
                               mb_name,
                               mb_cred,
                               edi_cred,
                               edi_env='staging',
                               publish=FALSE,
                               bucket_name=Sys.getenv('AWS_S3_BUCKETNAME')){
  warning(paste('This function (update_dataset_edi) is now deprecated and will', 
                'be removed from a future version of jerald. Use',
                'publish_dataset_edi instead'), immediate=T)
  
  # Collect metadata into EML list from Metabase (using MetaEgress)
  eml_list <- eml_egress(datasetid, mb_name, mb_cred)
  # Revision number in metabase
  rev.mb <- unlist(strsplit(eml_list$packageId, ".", fixed=TRUE))[3]
  rev.mb <- as.numeric(rev.mb)
  
  # Update the revision numbers using EDI
  eml_list_new <- increment_edi_revision(eml_list, edi_env=edi_env)
  rev.next <- unlist(strsplit(eml_list_new$packageId, ".", fixed=TRUE))[3]
  rev.next <- as.numeric(rev.next)
  if (rev.next==1){
    stop("To create a new package in EDI ", edi_env, ", use the 
         `create_dataset_edi` function")
  }
  # Warn if the revisions on metabase and EDI don't match
  #if (rev.mb!=(rev.next-1)){
  #  warning("The metabase revision (", rev.mb, "), does not match the EDI ",
  #          edi_env, "revision (", rev.next-1, ").")
  #}
  
  # Validate and serialize (write) EML document
  message('Validating EML...')
  out <- EML::eml_validate(eml_list_new)
  message(out)
  message('Writing EML...')
  emlfile <- paste0(eml_list_new$packageId, ".xml")
  EML::write_eml(eml_list_new, file=emlfile)
  message('Done.\n')
  
  if (!publish){
    message('Stopping execution: the dataset will not be published')
    message('Please check dataset identifiers, revision numbers, eml, etc.')
    message('To continue to publication pass argument `publish=TRUE`. \n')
    stop("Stopping", call.=FALSE)
  }
  
  # Collect the data entities from the eml list & push to s3 bucket
  ents <- get_eml_entities(eml_list_new)
  ents_to_s3(ents, bucket_name)
  
  # Update package on EDI
  edi_update_package(emlfile, edi_cred, edi_env=edi_env)
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
#' @param mb_cred list of credentials for the metabase postgres cluster
#' @param mb_name name of the metabase database in the postgres cluster
#' @param edi_cred list of credentials to use for EDI
#' @param edi_env name of the EDI environment to update (staging, production,
#' or development)
#' @param publish boolean value - publish if TRUE, end before s3 upload if 
#' FALSE
#' @param bucket_name name of the s3 bucket to push data entities to
#' @export
create_dataset_edi <- function(datasetid,
                               mb_name,
                               mb_cred,
                               edi_cred,
                               edi_env='staging',
                               publish=FALSE,
                               bucket_name=Sys.getenv('AWS_S3_BUCKETNAME')){
  warning(paste('This function (create_dataset_edi) is now deprecated and will', 
          'be removed from a future version of jerald. Use',
          'publish_dataset_edi instead'), immediate=T)
  
  # Collect metadata into EML list from Metabase (using MetaEgress)
  eml_list <- eml_egress(datasetid, mb_name, mb_cred)
  # Revision number in metabase
  rev.mb <- unlist(strsplit(eml_list$packageId, ".", fixed=TRUE))[3]
  rev.mb <- as.numeric(rev.mb)
  
  # Update the revision numbers using EDI
  eml_list_new <- increment_edi_revision(eml_list, edi_env=edi_env)
  rev.next <- unlist(strsplit(eml_list_new$packageId, ".", fixed=TRUE))[3]
  rev.next <- as.numeric(rev.next)
  if (rev.next>1){
    stop("This package already exists at EDI ", edi_env, ". Use the
         `update_dataset_edi` function")
  }
  # Warn if the revisions on metabase and EDI don't match
  #if (rev.mb != (rev.next-1)){
  #  warning("The metabase revision (", rev.mb, "), does not match the EDI ",
  #          edi_env, "revision (", rev.next-1, ").")
  #}
  
  # Validate and serialize (write) EML document
  message('Validating EML...')
  out <- EML::eml_validate(eml_list_new)
  message(out)
  message('Writing EML...')
  emlfile <- paste0(eml_list_new$packageId, ".xml")
  EML::write_eml(eml_list_new, file=emlfile)
  message('Done.\n')
  
  if (!publish){
    message('Stopping execution: the dataset will not be published')
    message('Please check dataset identifiers, revision numbers, eml, etc.')
    message('To continue to publication pass argument `publish=TRUE`. \n')
    stop("Stopping", call.=FALSE)
  }
  
  # Collect the data entities from the eml list & push to s3 bucket
  ents <- get_eml_entities(eml_list_new)
  ents_to_s3(ents, bucket_name)
  
  # Update package on EDI
  edi_create_package(emlfile, edi_cred, edi_env=edi_env)
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
#' @param mb_cred list of credentials for the metabase postgres cluster
#' @param mb_name name of the metabase database in the postgres cluster
#' @param edi_cred list of credentials to use for EDI
#' @param edi_env name of the EDI environment to update (staging, production,
#' or development)
#' @param dry_run boolean value - write EML only, then stop (end before s3 
#' and EDI upload) if TRUE, continue to publish if FALSE
#' @param s3_upload boolean value (T/F) if TRUE upload to the s3 bucket, if 
#' FALSE skip this (entities already there). Note that this does not currently
#' do a check on whether entities are present or not. 
#' @param multi_part boolean value (T/F) if TRUE upload to the s3 bucket using
#' multipart method
#' @param skip_checks boolean value (T/F) indicating whether or not to check
#' for congruence between data entity and attribute metadata 
#' (check_attribute_congruence function). May want to set as True if the data
#' are online and not in the working directory.
#' @param bucket_name name of the s3 bucket to push data entities to
#' @export
publish_dataset_edi <- function(datasetid,
                               mb_name,
                               mb_cred,
                               edi_cred,
                               edi_env='staging',
                               dry_run=TRUE,
                               s3_upload=TRUE,
                               multi_part=FALSE,
                               skip_checks=FALSE,
                               bucket_name=Sys.getenv('AWS_S3_BUCKETNAME')){
  
  # Collect metadata into EML list from Metabase (using MetaEgress)
  eml_list <- eml_egress(datasetid, mb_name, mb_cred,
                         skip_checks=skip_checks)
  
  # Update the revision numbers using EDI
  eml_list_new <- increment_edi_revision(eml_list, edi_env=edi_env)
  rev.next <- parse_edi_pid(eml_list_new, 'revision')
  if (rev.next>1){
    pubflag <- 'update'
  } else {
    pubflag <- 'create'
  }
  
  # Validate and serialize (write) EML document
  emlfile <- paste0(eml_list_new$packageId, ".xml")
  eml_serialize(eml_list_new, emlfile)
  
  if (dry_run){
    message('Stopping because this is a dry run')
    message('Please check dataset identifiers, revision numbers, eml, etc.')
    message('To continue to publication pass argument `dry_run=FALSE`. \n')
    stop("Stopping (dry run)", call.=FALSE)
  }
  
  if (s3_upload){
    # Collect the data entities from the eml list & push to s3 bucket
    ents <- get_eml_entities(eml_list_new)
    ents_to_s3(ents, bucket_name, multi_part=multi_part)
  } else {
    message('Skipping S3 upload: make sure data entity files are online ')
    message('at the URL designated in <distribution>. \n')
  }
  # Create or update dataset
  if (pubflag=='update'){
    edi_update_package(emlfile, edi_cred, edi_env=edi_env)
  } else {
    edi_create_package(emlfile, edi_cred, edi_env=edi_env)
  }
}


#' Wrapper to publish a dataset
#'
#' This is a basic wrapper for publishing datasets to repositories. It has
#' some path and error handling built in. It relies on credentials and 
#' other configurations being in one file. See the called functions
#' 
#'   * publish_dataset_edi()
#'   * thats it for now...
#' 
#' to understand what happens...
#'
#' @param datasetid ID number of the dataset to find in metabase and 
#' update in EDI
#' @param repository name of the repository to publish to
#' (edi.staging or edi.production)
#' @param cred_path path to jerald credentials file (repository and metabase
#' postgres cluster)
#' @param data_path path to the published data directory
#' @param dry_run boolean value - write EML only, then stop (end before s3 
#' and EDI upload) if TRUE, continue to publish if FALSE
#' @param s3_upload boolean value (T/F) if TRUE upload to the s3 bucket, if 
#' FALSE skip this (entities already there). Note that this does not currently
#' do a check on whether entities are present or not. 
#' @export
publish_dataset <- function(id, repository,
                            data_path, cred_path, 
                            dry_run=TRUE, s3_upload=TRUE){
  # Save the current working directory
  wd <- getwd()
  # Now switch to the target data directory
  setwd(data_path)
  options(scipen=999)   # turns off scientific notation
  # Read your jerald credentials
  source(paste(cred_path, 'jerald_cred.R', sep='/'))
  # Now create or update the dataset on EDI...
  # You must pass `dry_run=FALSE` to really publish the data. Make sure to check
  # dataset identifiers, revision numbers, eml, and other details first.
  result <- tryCatch(
    {
      # Try to publish the dataset using the credentials provided
      message('Begin publishing the dataset')
      suppressWarnings(
        if(repository=="edi.staging"){
          env <- "staging"
          publish_dataset_edi(id, mbname, mbcred, edicred, edi_env=env, dry_run=dry_run,
            s3_upload = s3_upload)
        } else if(repository=="edi.production"){
          env <- "production"
          publish_dataset_edi(id, mbname, mbcred, edicred, edi_env=env, dry_run=dry_run,
            s3_upload = s3_upload)
        } else {
          message("Valid repository not specified")
        }
      )
      # Return any warnings
    }, warning = function(w){
      message(paste("There was a warning publishing package ", id))
      message("Here's the original warning message:")
      message(conditionMessage(w))
      NULL
      # Return any errors
    }, error = function(e){
      message(paste("There was an error publishing package ", id))
      message("Here's the original error message:")
      message(conditionMessage(e))
      NA
      # Whatever happens, cleanup and return to original working directory
    }, finally = {
      # Clean up
      remove(list=c('mbcred', 'edicred', 'mbname'), envir = .GlobalEnv)
      setwd(wd)
    }
  )
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
template_dataset_dir <- function(datasetid){
  
  datasetid <- format(datasetid, scientific = FALSE)
  
  # Create dataset directory name
  user.shortname <- readline(paste0('Enter a short name for the dataset: '))
  dir_name <- paste(datasetid, user.shortname, sep='_')
  
  # Choose parent directory
  cap = 'Select a parent folder for the dataset directory'
  #user.destdir <- rstudioapi::selectDirectory(caption=cap)
  user.destdir <- tcltk::tk_choose.dir(caption=cap)
  
  # Create it
  new.dir <- file.path(path.expand(user.destdir), dir_name)
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
    y3 <- gsub("eml.999", paste0('eml.', as.character(datasetid)),
	       y2, fixed=TRUE)
    y4 <- gsub("id <- 999", paste0('id <- ', as.character(datasetid)),
	       y3, fixed=TRUE)
    cat(y4, file=t, sep="\n")
  }
  message('Done.\n')

  # Ask if user wants to template a metabase dataset
  user.template_metabase <- readline(paste0('Do you want to create an entry in metabase? (Y/n): '))
  # If so, run template_metabase
  if (tolower(user.template_metabase)=='y'){
    template_dataset_metabase(datasetid, user.shortname)
  }
}

template_dataset_metabase <- function(datasetid, shortname){
  message("You have selected to create an entry in Metabase for ", datasetid, ".")
  message(" but this feature is not implemented yet!")
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
#' @param eal_dir Source dataset directory, in EAL format, to migrate from
#' @param jerald_dir Destination dataset directory, in jerald format, to
#'                   migrate to
#' @export
#' 
migrate_eal_dir <- function(eal_dir, jerald_dir){
  
  message('\nWARNING - this function may remove data - be careful!!!')
  user.continue <- readline('Do you want to continue? (Y/n): ')
  if (tolower(user.continue)!='y'){
    stop('Aborting...')
  } else {
    message('continuing...\n')
  }
  
  # Expand paths
  eal_dir <- path.expand(eal_dir)
  jerald_dir <- path.expand(jerald_dir)
  
  # Make sure both directories exist
  if (!dir.exists(eal_dir)){
    stop(eal_dir, ' does not exist.')
  }
  if (!dir.exists(jerald_dir)){
    stop(jerald_dir, ' does not exist.\nCreate a jerald template directory',
        ' before migrating.')
  } else {
    # If the jerald directory exists and has no EAL archive,
    # create one. If it has an EAL archive, abort.
    eal_archive <- file.path(jerald_dir, 'EAL_archive')
    if (!dir.exists(eal_archive)){
      dir.create(eal_archive)
    } else if (dir.exists(eal_archive)){
      stop(eal_archive, '\n already exists! Aborting.')
    }
  }
  
  # List EAL directory contents
  eal_files <- list.files(eal_dir, full.names=TRUE, include.dirs = TRUE)
  # Copy all files to EAL_archive
  message('Copying all files from EAL_source to jerald_dest/EAL_archive/...')
  file.copy(eal_files, eal_archive, recursive=TRUE, copy.date=TRUE,
            copy.mode=TRUE)
  message('Done.\n')
  
  # Move any data entities or warn if not found
  eal_entities <- file.path(eal_archive, 'data_entities')
  if (dir.exists(eal_entities)){
    eal_dataents <- list.files(eal_entities, full.names=TRUE)
    message('Move EAL data entities to parent/...')
    file.copy(eal_dataents, jerald_dir, copy.date=TRUE, copy.mode=TRUE)
    file.remove(eal_dataents)
    message('Done.\n')
  }else{
    message('Data entities directory not found!')
  }

  # Copy build script to top level
  eal_buildscript <- list.files(eal_archive, pattern="(build_).*\\.R$",
                                full.names=TRUE)
  print(eal_buildscript)
  message('Move EAL build script to parent/...')
  file.copy(eal_buildscript, file.path(jerald_dir, 'build_EALarchive.R'),
            copy.date=TRUE, copy.mode=TRUE)
  file.remove(eal_buildscript)
  message('Done.\n')
  
  # Copy metadata files to metadata_docs/
  eal_metadatafiles <- list.files(eal_archive,
                                  pattern="\\.(prj|dsd|his|PRJ|DSD|HIS)",
                                  full.names=TRUE, recursive=TRUE)
  print(eal_metadatafiles)
  message('Move prj, dsd, and his files to metadata_docs/...')
  file.copy(eal_metadatafiles, file.path(jerald_dir, 'metadata_docs'),
            copy.date=TRUE, copy.mode=TRUE)
  file.remove(eal_metadatafiles)
  message('Done.\n')

  # Copy metadata templates to metadata_docs/
  eal_metadatatemp <- list.files(eal_archive,
                                 pattern="(metadata_template).*\\.(docx|xlsx)",
                                 full.names=TRUE, recursive=TRUE)
  print(eal_metadatatemp)
  message('Move metadata_template files to metadata_docs/...')
  file.copy(eal_metadatatemp, file.path(jerald_dir, 'metadata_docs'),
            copy.date=TRUE, copy.mode=TRUE)
  file.remove(eal_metadatatemp)
  message('Done.\n')

  # Copy other R files to parent
  eal_rscripts <- list.files(eal_archive,pattern="\\.R$",
                             full.names=TRUE)
  if(length(eal_rscripts > 0)){
    print(eal_rscripts)
    message('Additional R scripts are being moved to parent/')
    file.copy(eal_rscripts, file.path(jerald_dir),
              copy.date=TRUE, copy.mode=TRUE)
    file.remove(eal_rscripts)
    message('Done.\n')
  }

  # Copy EML files to eml (anything ending with .xml)
  eal_EML <- list.files(eal_archive, pattern="\\.xml$",
                        full.names=TRUE, recursive=TRUE)
  print(eal_EML)
  message('Move EML files to eml/...')
  file.copy(eal_EML, file.path(jerald_dir, 'EML'),
            copy.date=TRUE, copy.mode=TRUE)
  file.remove(eal_EML)
  message('Done.\n')
  
  
  # Remove the old directory?
  user.remove <- readline(paste0('Remove the EAL_source directory? ',
                                 '(check results first!) (Y/n): '))
  if (tolower(user.continue)!='y'){
    message('Removing ', eal_dir, ' ...')
    unlink(eal_dir, recursive = TRUE)
    message('Done.')
  }
}
