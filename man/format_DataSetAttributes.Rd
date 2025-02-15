% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/meta_ingress.R
\name{format_DataSetAttributes}
\alias{format_DataSetAttributes}
\title{Create the DataSetAttributes and related tables from an EML list (emld object)}
\usage{
format_DataSetAttributes(dsid, eml)
}
\arguments{
\item{dsid}{The dataset id value, which is the primary key for lter_metabase}

\item{eml}{An emld object derived from an EML file}
}
\value{
A named list containing four dataframes formatted to match the
lter_metabase.'DataSetAttributes',  lter_metabase.'DataSetAttributeEnumeration',
lter_metabase.'DataSetAttributeMissingCodes', and lter_metabase.'ListCodes' tables.
}
\description{
Create the DataSetAttributes and related tables from an EML list (emld object)
}
