#' Check EDI login status
#'
#' This function checks whether we are logged into EDI by looking at the
#' "EDI_TOKEN" environment variable. If it is an empty string, log back
#' in.
#' @param edi_cred EDI credentials in a list
#' @export
edi_login <- function(edi_cred){
  message('Checking EDI login status...')
  edi_token <- Sys.getenv('EDI_TOKEN')
  if (edi_token == ''){
    message('logging in...')
    EDIutils::login(edi_cred$user.id, edi_cred$user.pass)
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
#' @param eml_path Path to an EML document for the new EDI data package
#' Should include scope, packagid, and revision number (e.g. 
#' knb-lter-jrn.999.1)
#' @param edi_cred EDI credentials in a list
#' @param edi_env EDI repository environment (staging, production, or 
#' development)
#' @export
edi_create_package <- function(eml_path, edi_cred, edi_env='staging'){
  message('Sending a new EML file ', eml_path, ' to EDI ', edi_env, ' ...')
  edi_login(edi_cred)
	transaction <- EDIutils::create_data_package(
    eml = eml_path, # path (with package id)
    env = edi_env)  # EDI environment
  
  # Check status of transaction
  EDIutils::check_status_create(transaction, wait=TRUE, env=edi_env)
  packageid <- strsplit(transaction, '__')[[1]][2]
  message('Created new package ', packageid, ' on EDI ', edi_env)
  EDIutils::read_data_package_report_summary(packageid, env=edi_env)
  
  message('Done.\n')
  # The above should be equivalent to:
  # api_create_data_package(path=getwd(),package.id = packageid,
  # 			  environment = edi_env,
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
#' @param eml_path Path to an EML document for the EDI data package.
#' Should include scope, packagid, and revision number (e.g. 
#' knb-lter-jrn.999.3)
#' @param edi_cred EDI credentials in a list
#' @param edi_env EDI repository environment (staging, production, or 
#' development)
#' @export
edi_update_package <- function(eml_path, edi_cred, edi_env='staging'){
  message('Sending EML file ', eml_path, ' to EDI ', edi_env, ' for update...')
  edi_login(edi_cred)
  transaction <- EDIutils::update_data_package(
    eml = eml_path, # path (with package id)
    env = edi_env)  # EDI environment
  
  # Check status of transaction
  EDIutils::check_status_update(transaction, wait=TRUE, env=edi_env)
  packageid <- strsplit(transaction, '__')[[1]][2]
  message('Updated package ', packageid, ' on EDI ', edi_env)
  EDIutils::read_data_package_report_summary(packageid, env=edi_env)
  
  message('Done.\n')
  # The above should be equivalent to:
  # api_update_data_package(path=getwd(),package.id = packageid,
  # 			  environment = edi_env,
  #   			  affiliation='EDI', user.id='<user>',
  # 			  user.pass='<password>')
}

#' Get the EDI dataset revision number from an EML document
#'
#' Any EML document created for the EDI repository should have a packageId
#' field. Parse out the parts of that.
#'
#' @param eml_list R list of EML-schema-formatted metadata
#' @param parse A string designating which part of the EDI package ID to return
#' ('scope', 'dataset', or 'revision')
#' @return The requested portion of the EDI package ID 
#' @export
parse_edi_pid <- function(eml_list, parse='scope'){
  if (parse=='scope'){
    pid <- unlist(strsplit(eml_list$packageId, ".", fixed=TRUE))[1]}
  else if (parse=='dataset'){
    pid <- as.numeric(unlist(strsplit(eml_list$packageId, ".", fixed=TRUE))[2])}
  else if (parse=='revision'){
    pid <- as.numeric(unlist(strsplit(eml_list$packageId, ".", fixed=TRUE))[3])}
  return(pid)
}

#' Update EML revision numbers using EDI repository
#'
#' This function queries the EDI repository to retrieve the current
#' revision number for a data package there, and then updates an EML 
#' list accordingly.
#'
#' @param eml_list R list of EML-schema-formatted metadata
#' @param edi_env Name of EDI repository environment ('staging', 'production',
#' or 'development')
#' @return An EML-schema-formatted list of metadata with revised package 
#' revision numbers.
#' @export
increment_edi_revision <- function(eml_list, edi_env='staging'){
  # Get the scope, packageid, and revision number
  scope <- parse_edi_pid(eml_list, 'scope')
  datasetid <- parse_edi_pid(eml_list, 'dataset')
  rev_in <- parse_edi_pid(eml_list, 'revision')
  
  # get the current revision number on EDI and increment by one,
  # then update in metadata list with the next revision number
  message('Checking revision number for ', datasetid, ' package in EDI ',
          edi_env, ' and adding 1...')
  rev_edi <- tryCatch({
    #Try to get data package revisions
    EDIutils::list_data_package_revisions(scope,
                                          datasetid,
                                          filter='newest',
                                          env=edi_env)
  },
  error=function(cond) {
    message(paste("There is no current package with identifier ",
                  datasetid))
    message("A new package must be created in the EDI environment")
    message("Here's the original error message:")
    message(cond)
    # Choose a return value (should be zero)
    return(0)
  },
  finally = {message('Done.\n')
  }
  )
  rev_next <- as.numeric(rev_edi) + 1
  message(paste("The next revision number for the package will be: ", rev_next))
  
  # Warn if the revisions on metabase and EDI don't match
  if (rev_in != (rev_next-1)){
    warning("The metabase revision (", rev_in, "), does not match the EDI ",
            edi_env, " revision (", rev_next-1, ").")
  }
  # Create packageID
  id_eml_next <- paste0(scope, "." , datasetid, ".", rev_next)
  # Create new eml_list with the new emlpkgid_next
  eml_list_next <- eml_list
  eml_list_next$packageId <- id_eml_next
  
  message('Done.\n')
  return(eml_list_next)
}