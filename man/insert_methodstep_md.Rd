% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eml.R
\name{insert_methodstep_md}
\alias{insert_methodstep_md}
\title{Insert markdown methodstep element into an EML document}
\usage{
insert_methodstep_md(eml.list, md_file, islist = TRUE)
}
\arguments{
\item{eml.list}{An EML-schema-formatted R list (EML R package compliant)}

\item{md_file}{A markdown methods file (full path)}
}
\value{
An updated EML-schema-formatted R list (EML R package compliant)
}
\description{
This function inserts a markdown methods section into a methodstep for an
EML document. Supply a markdown file and it will be inserted into the first
methodstep.
}
