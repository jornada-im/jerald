% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/edi.R
\name{edi_login}
\alias{edi_login}
\title{Check EDI login status}
\usage{
edi_login(edi.cred)
}
\arguments{
\item{edi.cred}{EDI credentials in a list}
}
\description{
This function checks whether we are logged into EDI by looking at the
"EDI_TOKEN" environment variable. If it is an empty string, log back
in.
}
