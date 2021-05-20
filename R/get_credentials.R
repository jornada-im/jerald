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

#' Get EDI credentials
#'
#' This function reads credentials for an EDI user into the edicred
#' list variable.
#'
#' @param path Path to the 'edi_keys.R' file containing your EDI credentials
#' credentials
#' @return edicred - a list containing metabase credentials
#' @export
load_edi_cred <- function(path){
  # Source the file
  source(paste0(path, 'edi_keys.R'))
}

#' Get s3 bucket API keys
#'
#' This function reads credentials for an s3 bucket into environmental
#' variable for access by the aws.s3 package.
#'
#' @param path Path to the 's3_keys.R' file containing s3 API keys
#' @return 1 if successful
#' @export
load_s3_cred <- function(path){
  # Source the file
  source(paste0(path, 's3_keys.R'))
}
