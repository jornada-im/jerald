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
#' @param bucket.name name of the s3 bucket to push data entities to
#' @export
dataset_update_edi <- function(datasetid,
                               mb.name,
                               mb.cred,
                               edi.cred,
                               edi.env='staging',
                               bucket.name=Sys.getenv('AWS_S3_BUCKETNAME')){
  
  # Collect metadata into EML list from JRN metabase (using MetaEgress)
  print(paste('Collecting metadata for', datasetid, 'from LTER Metabase',
              mb.name, '...'))
  eml.dsid <- eml_from_mb(datasetid, mb.name, mb.cred)
  print('Done.\n')
  
  # Update the revision numbers using EDI
  print(paste('Updating revision information for', datasetid, 'against EDI',
              edi.env, 'package...'))
  eml.dsid.new <- update_eml_revnum_edi(eml.dsid, edi.env=edi.env)
  if (eml.dsid.new$packageID==1){
    stop("This package does not exist at EDI yet ", edi.env, ". Use the
                `dataset_create_edi` function")
    }
  print('Done.\n')
  
  # validate and serialize (write) EML document
  print('Writing EML...')
  EML::eml_validate(eml.dsid.new)
  EML::write_eml(eml.dsid.new, file = paste0(eml.dsid.new$packageId, ".xml"))
  print('Done.\n')
  
  # Collect the data entities from the eml list & push to s3 bucket
  ents <- get_eml_entities(eml.dsid.new)
  #print
  ents_to_s3(ents, bucket.name)
  
  # Update package on EDI
  edi_update_package(eml.dsid.new$packageId, edi.cred, edi.env=edi.env)
}