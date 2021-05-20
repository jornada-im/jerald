#' EML metadata from metabase
#'
#' This function queries a metabase and formats metadata to the EML spec
#'
#' @param pkgid Number or numeric vector of dataset IDs to query
#' @param dbname Name of the database ('metabase') to query metadata from
#' @param dbcred Credentials for the database (a list)
#' @return An EML-formatted list of metadata
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

#' Update EML revision number from EDI
#'
#' This function queries the EDI repository to retrieve the current
#' revision number for a data package and updates an EML list accordingly
#'
#' @param eml.list EML metadata list
#' @param edienv Name of EDI repository environment ('staging', 'production',
#' or 'development')
#' @return An EML-formatted list of metadata (with revised revnum)
#' @export
update_revnum_edi <- function(eml.list, edienv='staging'){
	# Get the scope, packageid, and revision number
	emlpkgid <- eml.list$packageId
	scope <- unlist(strsplit(emlpkgid, ".", fixed=TRUE))[1]
	pkgid <- unlist(strsplit(emlpkgid, ".", fixed=TRUE))[2]
	rev <- unlist(strsplit(emlpkgid, ".", fixed=TRUE))[3]

	# get the current revision number on EDI and increment by one,
	# then update in metadata list with the next revision number
	rev.start <- EDIutils::api_list_data_package_revisions(scope,
							  pkgid,
							  filter='newest',
							  environment=edienv)
	if (is.na(as.numeric(rev.start))){
		    print(paste("WARNING: new data package in environment",
				edienv, ", revision will equal 1."))
		    rev.start <- 0
	}
	rev.next <- as.numeric(rev.start) + 1

	# Create packageID
	emlpkgid.next <- paste0(scope, "." , pkgid, ".", rev.next)
	# Create new eml.list with the new emlpkgid.next
	eml.list.next <- eml.list
	eml.list.next$packageId <- emlpkgid.next

	return(eml.list.next)
}

#' Get dataTable and otherEntity filenames
#'
#' This function extracts the filenames for dataTables and otherEntities
# from an eml list into a vector.
# 
#' @param eml.list An EML list object 
#' @return A list of filenames
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
