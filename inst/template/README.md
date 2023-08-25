# Dataset 999 (example)

* Dataset ID: 999

(EXAMPLE - All template text in this README file can be replaced with your own,
but general guidelines are given.)

## Description

(Describe the dataset in general terms here)

This is an example dataset directory for use with an LTER Metabase and jerald.

## Metadata sources

(Describe the sources of the metadata in this package. What came from EML files
on EDI, researchers, research site archives, and other places?)

Metadata templates and other metadata source files are placed in the
`metadata_docs/` directory. 

The abstract and methods text files referred to in the Metabase 
(`abstract.999.md` and `methods.999.md`) should be kept in the
parent directory and can be updated there. Otherwise, the Metabase is the 
primary store of metadata for building EML files, and all changes should be 
made there.

## Data table sources

(Describe the source of the data entities published with this dataset. Where
did raw data, published CSVs, or other files come from? Will new data be added,
and if so, how?).

Raw data and other source data files are found in the `source_data/` directory.

The raw data file in the example dataset (mtcars.csv) comes from R's built in
mtcars dataset.

## Other entities

(Describe any other entities included in the data package, such as maps,
procedures, documents, non-tabular data files, etc.)

## Build scripts

The build scripts here prepare the data and metadata for publication using
various R packages. The resulting EML and data entities may be published using
the scripts, or manually using the 
[EDI portal](https://portal-s.edirepository.org).

* `build_dataset.999.R` - Basic build script that formats & QA/QCs data
in `source_data/` and writes publishable data files to the working directory
(`./`)
* `build_eml.999.R` - Creates an EML file and optionally uploads data
entities and pushes the package to EDI using APIs and the
[`jerald`](https://github.com/jornada-im/jerald) R package.

## How to make EML and publish the dataset

For more information on updating and publishing this dataset, see the Jornada
IM documentation site:

* [Metadata standards](https://jornada-im.github.io/documentation/jornada_metadata_standards.html)
* [Updating datasets in Metabase](https://jornada-im.github.io/documentation/jrn_metabase_create_update_dataset.html)
* [Make EML and publish](https://jornada-im.github.io/documentation/makeEML_metabase_jerald.html)

