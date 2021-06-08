#' Create EDI repository package
#'
#' This function takes an EDI package identifier and creates the package on EDI.
#' The correspondingly named EML document must be present in the working 
#' directory and must have revision number 1. NOTE that if this isn't working
#' one might have to upload EML and data at portal-s.edirepository.org
#'
#' @param emlpkgid Package identifier for EML document and EDI data package.
#' Should include scope, packagid, and revision number (e.g. 
#' knb-lter-jrn.210000000.1)
#' @param edi.cred EDI credentials in a list
#' @param edi.env EDI repository environment (staging, production, or 
#' development)
#' @export
edi_create_package <- function(packageid, edi.cred, edi.env='staging'){
  message('Creating new package ', packageid, ' at EDI ', edi.env, '...')
	do.call(EDIutils::api_create_data_package,
	        c(list(path = getwd(),        # current directory
	               package.id = packageid, # package id
	               environment = edi.env),# EDI environment
	          edi.cred))
	message('Done.\n')
	# The above should be equivalent to:
	# api_create_data_package(path=getwd(),package.id = packageid,
	# 			  environment = edi.env,
	#   			  affiliation='EDI', user.id='<user>',
	# 			  user.pass='<password>')
}

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
#' @param edi.cred EDI credentials in a list
#' @param edi.env EDI repository environment (staging, production, or 
#' development)
#' @export
edi_update_package <- function(packageid, edi.cred, edi.env='staging'){
  message('Updating package ', packageid, ' at EDI ', edi.env, '...')
  do.call(EDIutils::api_update_data_package,
          c(list(path = getwd(),        # current directory
                 package.id = packageid, # package id
                 environment = edi.env),# EDI environment
            edi.cred))
  message('Done.\n')
  # The above should be equivalent to:
  # api_update_data_package(path=getwd(),package.id = packageid,
  # 			  environment = edi.env,
  #   			  affiliation='EDI', user.id='<user>',
  # 			  user.pass='<password>')
}
