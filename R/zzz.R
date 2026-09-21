#' Set the environment when jerald is loaded
#' 
#' This "hook" will execute whenever 'jerald' is loaded or attached and 
#' create objects accessible in the jerald namespace. Mainly these are 
#' secret things like credentials and host addresses that users don't need
#' direct access to during a session, but functions do. These are not 
#' exported, but there are some public functions that will expose edicred
#' and mbcred if asked. More at the
#' [R package book](https://r-pkgs.org/code.html#sec-code-onLoad-onAttach).
#'
#' @param libname library name - automatically set
#' @param pkgname name of the package (jerald)
#'
.onLoad <- function(libname, pkgname){
  ns <- asNamespace(pkgname)
  # Create the edicred list of EDI credentials
  assign("edicred", list(
    key = Sys.getenv("EDI_API_KEY"),
    user = Sys.getenv("EDI_USER_NAME"),
    password = Sys.getenv("EDI_USER_PASS"),
    affil = "EDI"),
    envir = ns)
  # Create the mbcred list of Metabase credentials
  assign("mbcred", list(
    dbname = Sys.getenv("METABASE_NAME"),
    host = Sys.getenv("METABASE_HOST"),
    port = Sys.getenv("METABASE_PORT"),
    user = Sys.getenv("METABASE_USER"),
    password = Sys.getenv("METABASE_PASSWORD")),
    envir = ns)
}