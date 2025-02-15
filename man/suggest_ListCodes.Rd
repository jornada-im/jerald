% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/meta_ingress.R
\name{suggest_ListCodes}
\alias{suggest_ListCodes}
\title{Suggest coded values from metabase ListCodes to match ListCodes from eml}
\usage{
suggest_ListCodes(from_eml, from_metabase)
}
\arguments{
\item{from_eml}{A dataframe formatted to match the lter_metabase."ListCodes"
table - taken from an EML file}

\item{from_metabase}{A dataframe formatted to match the
lter_metabase."ListCodes" table - taken from an LTER metabase table}
}
\value{
A table merging eml- and metabase-derived ListCodes tables on "Code"
}
\description{
Suggest coded values from metabase ListCodes to match ListCodes from eml
}
