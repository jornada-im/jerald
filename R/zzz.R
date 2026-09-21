#' On load of the package do this stuff
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