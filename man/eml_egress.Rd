% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eml.R
\name{eml_egress}
\alias{eml_egress}
\title{Create EML list from an LTER metabase using MetaEgress}
\usage{
eml_egress(datasetid, mb.name, mb.cred, skip_checks = FALSE)
}
\arguments{
\item{datasetid}{Number or numeric vector of dataset IDs to query}

\item{mb.name}{Name of the LTER metabase to query metadata from}

\item{mb.cred}{Credentials for mb.name (a list, see \code{load_metabase_cred})}

\item{skip_checks}{Boolean value (T/F) indicating whether or not to check
for congruence between data entity and attribute metadata
(check_attribute_congruence function). May want to set as True if the data
are online and not in the working directory.}
}
\value{
An EML-schema-formatted list of metadata (EML R package compliant)
}
\description{
This function queries an LTER metabase and formats the metadata into an
R list in the EML schema
}
