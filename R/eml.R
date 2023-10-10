#' Create EML list from an LTER metabase
#'
#' This function queries an LTER metabase and formats the metadata into an
#' R list in the EML schema
#'
#' @param datasetid Number or numeric vector of dataset IDs to query
#' @param mb.name Name of the LTER metabase to query metadata from
#' @param mb.cred Credentials for mb.name (a list, see `load_metabase_cred`)
#' @return An EML-schema-formatted list of metadata
#' @export
eml_from_mb <- function(datasetid, mb.name, mb.cred){
  # Get metadata from metabase
  message('Collecting metadata for ', datasetid, ' from LTER Metabase ',
	  mb.name, '...')
  metadata <- do.call(MetaEgress::get_meta,
		      c(list(dbname = mb.name,
	                     dataset_ids = c(datasetid)),
			mb.cred)) # assigned in cred file
  # Create a list of entities formatted for the EML document
  message('Generating entity table for ', datasetid, '...')
  entities <- MetaEgress::create_entity_all(meta_list =  metadata,
					    file_dir = getwd(),
					    dataset_id = datasetid)
  # Create an EML schema list object
  message('Creating EML schema list...')
  eml.list <- MetaEgress::create_EML(meta_list = metadata,
	                             entity_list = entities,
	                             dataset_id = datasetid,
	                             expand_taxa = TRUE,
	                             skip_taxa = FALSE)
  message('Done.\n')
  return(eml.list)	
}

#' Update EML revision numbers using EDI repository
#'
#' This function queries the EDI repository to retrieve the current
#' revision number for a data package there, and then updates an EML 
#' list accordingly.
#'
#' @param eml.list R list of EML-schema-formatted metadata
#' @param edi.env Name of EDI repository environment ('staging', 'production',
#' or 'development')
#' @return An EML-schema-formatted list of metadata with revised package 
#' revision numbers.
#' @export
update_eml_revnum_edi <- function(eml.list, edi.env='staging'){
  # Get the scope, packageid, and revision number 
  id.eml <- eml.list$packageId
  scope <- unlist(strsplit(id.eml, ".", fixed=TRUE))[1]
  datasetid <- unlist(strsplit(id.eml, ".", fixed=TRUE))[2]
  rev <- unlist(strsplit(id.eml, ".", fixed=TRUE))[3]
  
  # get the current revision number on EDI and increment by one,
  # then update in metadata list with the next revision number
  message('Checking revision number for ', datasetid, ' package in EDI ',
	  edi.env, ' and adding 1...')
  rev.edi <- tryCatch({
    #Try to get data package revisions
    EDIutils::list_data_package_revisions(scope,
					  datasetid,
					  filter='newest',
					  env=edi.env)
  },
    error=function(cond) {
      message(paste("There is no current package with this identifier: ",
		    datasetid))
      message("This will create a new package in the EDI environment")
      message("Here's the original error message:")
      message(cond)
      # Choose a return value
      return(0)
  },
    finally = {message('Done.\n')
  }
  )
  rev.next <- as.numeric(rev.edi) + 1
  message(paste("The next revision number for the package will be: ", rev.next))

  # Create packageID
  id.eml.next <- paste0(scope, "." , datasetid, ".", rev.next)
  # Create new eml.list with the new emlpkgid.next
  eml.list.next <- eml.list
  eml.list.next$packageId <- id.eml.next

  message('Done.\n')
  return(eml.list.next)
}

#' Get dataTable and otherEntity filenames from EML
#'
#' This function extracts the data entity filenames (dataTables and 
#' otherEntities) from an EML-schema-formatted list and returns a vector
#' of the filenames (useful for upload to s3 buckets).
#' 
#' @param eml.list An EML-schema-formatted R list
#' @return A list of data entity filenames found in the EML
#' @export
get_eml_entities <- function(eml.list){
  # Get dataTable filenames from EML
  entlist <- c()
  if (length(eml.list$dataset$dataTable) > 0){
    for (i in 1:length(eml.list$dataset$dataTable)){
      entlist <- append(entlist,
			eml.list$dataset$dataTable[[i]]$physical$objectName)
    }
  }
  # Also add otherentities
  if (length(eml.list$dataset$otherEntity) > 0){
    for (i in 1:length(eml.list$dataset$otherEntity)){
      entlist <- append(entlist,
			eml.list$dataset$otherEntity[[i]]$physical$objectName)
    }
  }
  return(entlist)
}
