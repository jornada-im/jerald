#' Check EDI login status
#'
#' This function checks whether we are logged into EDI by looking at the
#' "EDI_TOKEN" environment variable. If it is an empty string, log back
#' in.
#' @param edi.cred EDI credentials in a list
#' @export
edi_login <- function(edi.cred){
  message('Checking EDI login status...')
  edi_token <- Sys.getenv('EDI_TOKEN')
  if (edi_token == ''){
    message('logging in...')
    EDIutils::login(edi.cred$user.id, edi.cred$user.pass)
  } else {
    message('already logged in...')
  }
  message('Done.\n')
}

#' Create EDI repository package
#'
#' This function takes a path to an EML file (with an EDI package identifier)
#' and creates the package in the given EDI environment (default is 'staging').
#' The given EML document must be present in the working directory and must
#' have revision number 1. NOTE that if this isn't working one might need
#' to upload EML and data at portal-s.edirepository.org
#'
#' @param eml.path Path to an EML document for the new EDI data package
#' Should include scope, packagid, and revision number (e.g. 
#' knb-lter-jrn.210000000.1)
#' @param edi.cred EDI credentials in a list
#' @param edi.env EDI repository environment (staging, production, or 
#' development)
#' @export
edi_create_package <- function(eml.path, edi.cred, edi.env='staging'){
  message('Sending a new EML file ', eml.path, ' to EDI ', edi.env, ' ...')
  edi_login(edi.cred)
	transaction <- EDIutils::create_data_package(
    eml = eml.path, # path (with package id)
    env = edi.env)  # EDI environment
  
  # Check status of transaction
  EDIutils::check_status_create(transaction, wait=TRUE, env=edi.env)
  packageid <- strsplit(transaction, '__')[[1]][2]
  message('Created new package ', packageid, ' on EDI ', edi.env)
  EDIutils::read_data_package_report_summary(packageid, env=edi.env)

	message('Done.\n')
	# The above should be equivalent to:
	# api_create_data_package(path=getwd(),package.id = packageid,
	# 			  environment = edi.env,
	#   			  affiliation='EDI', user.id='<user>',
	# 			  user.pass='<password>')
}

#' Update EDI repository package
#'
#' This function takes a path to an EML file (with an EDI package identifier)
#' and updates the package in the given EDI environment (default is 'staging').
#' The given EML document must be present in the working directory. NOTE that
#' if this isn't working one might need to upload EML and data at
#' portal-s.edirepository.org
#'
#' @param eml.path Path to an EML document for the EDI data package.
#' Should include scope, packagid, and revision number (e.g. 
#' knb-lter-jrn.210000000.3)
#' @param edi.cred EDI credentials in a list
#' @param edi.env EDI repository environment (staging, production, or 
#' development)
#' @export
edi_update_package <- function(eml.path, edi.cred, edi.env='staging'){
  message('Sending EML file ', eml.path, ' to EDI ', edi.env, ' for update...')
  edi_login(edi.cred)
  transaction <- EDIutils::update_data_package(
    eml = eml.path, # path (with package id)
    env = edi.env)  # EDI environment
  
  # Check status of transaction
  EDIutils::check_status_update(transaction, wait=TRUE, env=edi.env)
  packageid <- strsplit(transaction, '__')[[1]][2]
  message('Updated package ', packageid, ' on EDI ', edi.env)
  EDIutils::read_data_package_report_summary(packageid, env=edi.env)
  
  message('Done.\n')
  # The above should be equivalent to:
  # api_update_data_package(path=getwd(),package.id = packageid,
  # 			  environment = edi.env,
  #   			  affiliation='EDI', user.id='<user>',
  # 			  user.pass='<password>')
}
