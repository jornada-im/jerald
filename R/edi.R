#' Update EDI repository package
#'
#' This function takes an EDI package identifier and updates the package on EDI.
#' The correspondingly named EML document must be present in the working 
#' directory. NOTE that if this isn't working one might have to upload EML
#' and data at portal-s.edirepository.org
#'
#' @param emlpkgid Package identifier for EML document and EDI data package.
#' Should include scope, packagid, and revision number (e.g. 
#' knb-lter-jrn.210000000.3)
#' @param edicred EDI credentials in a list
#' @param edienv EDI repository environment (staging, production, or 
#' development)
#' @export
update_edi_package <- function(emlpkgid, edicred, edienv='staging'){
	do.call(EDIutils::api_update_data_package,
		c(list(path = getwd(),       # current directory
		       package.id = emlpkgid,   # package id
		       environment = edienv),# EDI environment
		  edicred))
	# The above should be equivalent to:
	# api_update_data_package(path=getwd(),package.id = pkgid,
	# 			  environment = edienv,
	#   			  affiliation='EDI', user.id='<user>',
	# 			  user.pass='<password>')
}
