#' Create EML list from an LTER metabase
#'
#' This function queries an LTER metabase and formats the metadata into an
#' R list in the EML schema
#'
#' @param pkgid Number or numeric vector of dataset IDs to query
#' @param dbname Name of the database ('metabase') to query metadata from
#' @param dbcred Credentials for the database (a list)
#' @return An EML-schema-formatted list of metadata
#' @export
eml_from_mb <- function(pkgid, dbname, dbcred){
	# Get metadata from metabase
	metadata <- do.call(MetaEgress::get_meta,
			    c(list(dbname = dbname,
				   dataset_ids = c(pkgid)), # can be a vector
			      mbcred)) # assigned in cred file}
	
	# Create a list of entities formatted for the EML document
	tables_pkg <- MetaEgress::create_entity_all(meta_list =  metadata,
						    file_dir = getwd(),
						    dataset_id = pkgid)
	# Create an EML schema list object
	eml.list <- MetaEgress::create_EML(
					   meta_list = metadata,
					   entity_list = tables_pkg,
					   dataset_id = pkgid)
	return(eml.list)
}

#' Update EML revision numbers using EDI repository
#'
#' This function queries the EDI repository to retrieve the current
#' revision number for a data package there, and then updates an EML 
#' list accordingly.
#'
#' @param eml.list R list of EML-schema-formatted metadata
#' @param edienv Name of EDI repository environment ('staging', 'production',
#' or 'development')
#' @return An EML-schema-formatted list of metadata with revised package 
#' revision numbers.
#' @export
update_revnum_edi <- function(eml.list, edienv='staging'){
	# Get the scope, packageid, and revision number
	emlpkgid <- eml.list$packageId
	scope <- unlist(strsplit(emlpkgid, ".", fixed=TRUE))[1]
	pkgid <- unlist(strsplit(emlpkgid, ".", fixed=TRUE))[2]
	rev.db <- unlist(strsplit(emlpkgid, ".", fixed=TRUE))[3]

	# get the current revision number on EDI and increment by one,
	# then update in metadata list with the next revision number
	rev.edi <- EDIutils::api_list_data_package_revisions(scope,
							  pkgid,
							  filter='newest',
							  environment=edienv)
	if (is.na(as.numeric(rev.edi))){
		    print(paste("WARNING: new data package in environment",
				edienv, ", revision will equal 1."))
		    rev.edi <- 0
	}
	rev.next <- as.numeric(rev.edi) + 1

	# Create packageID
	emlpkgid.next <- paste0(scope, "." , pkgid, ".", rev.next)
	# Create new eml.list with the new emlpkgid.next
	eml.list.next <- eml.list
	eml.list.next$packageId <- emlpkgid.next

	return(eml.list.next)
}

#' Get dataTable and otherEntity filenames
#'
#' This function extracts the data entity filenames (dataTables and 
#' otherEntities) from an EML-schema-formatted list and returns a vector
#' of the filenames (useful for upload to s3 buckets).
#' 
#' @param eml.list An EML-schema-formatted R list
#' @return A list of data entity filenames found in the EML
#' @export
get_entities <- function(eml.list){
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
