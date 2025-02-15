% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eml.R
\name{eml_serialize}
\alias{eml_serialize}
\title{Create EML-compliant XML file from an R list}
\usage{
eml_serialize(eml.list, fname)
}
\arguments{
\item{eml.list}{An EML-schema-formatted R list (EML R package compliant)}

\item{fname}{Name of the EML file to create}
}
\description{
Take an EML object (EML packages R list in EML schema), validate it,
and write it to a file
}
