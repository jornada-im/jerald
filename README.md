# jerald

This R package provides a workflow and tools to manage and publish research datasets to online repositories. It includes functions to harvest metadata from a metadata database, or "Metabase", create [EML](https://eml.ecoinformatics.org/) documents, and publish datasets to the [EDI data repository](https://portal.edirepository.org). The Metabase is expected to follow the [LTER-core-metabase](https://github.com/lter/LTER-core-metabase) schema.

`jerald` depends on a few required R packages:

* [MetaEgress](https://github.com/BLE-LTER/MetaEgress) to access the Metabase
* [aws.s3](https://cloud.r-project.org/web/packages/aws.s3/index.html)
* [EDIutils](https://ediorg.github.io/EDIutils/)

**It also requires credentials to use the Metabase and web resources.**

The package was developed by the [Jornada LTER](https://lter.jornada.nmsu.edu), so `jerald` is a bit specific to LTER sites and their research data workflows. It should, however, be somewhat extensible for other sites and purposes.

## Installation

Install the GitHub version with `devtools`.

    devtools::install_github("jornada-im/jerald")

In some cases you might want the development version, but be careful and talk to the developer first.

    devtools::install_github("jornada-im/jerald@dev")

The dependencies listed above should be pulled in at install time if you don't already have them.

### Setting up credentials

Once `jerald` is installed you will need to make credentials available for accessing the metadata database and the necessary web resources, which include a data repository (usually EDI) and an s3 bucket. You do this with an `.Renviron` file that is read at R startup and adds credentials to your session environment. See the [`.Renviron.template`](inst/template/.Renviron.template) file for an example of the proper format. Your `.Renviron` can live at either the project level (`<project name>/.Renviron`) or user level (in your home directory: `$HOME/.Renviron`) and should contain credentials for:

1. A "Metabase", or metadata database, running on PostgreSQL. 
2. An S3 bucket at a cloud provider. Currently we use DigitalOcean and the [`aws.s3`](https://github.com/cloudyr/aws.s3) client R package.
3. A repository to publish to. Currently we use [EDI](https://edirepository.org) and authenticate via an API key (described [here](https://edirepository.org/user-guides/iam)).

To get started, copy the template (`.Renviron.template`) to your project or home directory. You can also use `usethis::edit_r_environ("user|project")` to open (or edit) an `.Renviron` in one of those places. Once template values are there you will need to contact your lead IM or administrator for the correct credentials to fill in. Always restart R after making changes.

## IM tasks and relevant jerald functions

`jerald` has several user-facing functions, described below, that will help accomplish most data management and publication tasks performed by information managers at the Jornada. A template script that uses these functions to update an example dataset (id=999) on EDI is provided when making a new dataset directory using the instructions below, or is available at `inst/template/build_eml.999.R`. This script gives a demonstration of a basic Jornada workflow.

### Create a new dataset directory or migrate an older one

**NOTE: These methods are a little out of date at the moment!**

`jerald` includes a number of templates (see `inst/template/`) for setting up a data management project directory and scripts. To create a new dataset directory and populate with recommended files and subdirectories, run:

    template_dataset_dir(datasetid)

where `datasetid` is a unique identifier for the dataset in the Metabase and at EDI.

An existing dataset directory formatted for use with [EMLassemblyline](https://ediorg.github.io/EMLassemblyline/) (EAL) can be migrated to `jerald`'s template format using:

    migrate_eal_dir(eal.dir, jerald.dir)

where `eal.dir` is the path to the EAL directory, and `jerald.dir` is the path to a new dataset directory created with `template_dataset_dir`. All metadata and data files from `eal.dir` should be copied into a new `EAL_archive` directory in `jerald.dir`. Be aware that there is the potential for data or metadata loss here, so check that all necessary metadata has been copied before deleting the old directory.

### Building the dataset entities

`jerald` doesn't do this directly yet, but there is a template script called `build_dataset.999.R` that demonstrates how to prepare a tabular data entity. This template script will be created in any new `jerald` dataset directory (using `template_dataset_dir`) or can be found in `inst/template/`.

**NOTE:** More recently for the Jornada, these dataset build scripts are being developed and stored in the `juntar` repositories in our GitHub organization (there are several).

### Update or create a dataset

To publish a dataset in a repository the `publish_dataset` function wraps up most of the functionality in jerald. You provide a datasetid, the destination repository (currently only EDI supported), and the path to the data entities. 

    publish_dataset(datasetid, "edi.staging", "./path/to/data" )

#### Supported options

* `dry_run` controls whether publication completes or is a dry run (as explained above). `TRUE` by default.
* `s3_upload` controls whether the data entities will be pushed to the s3 bucket (`TRUE` by default). If the entities already exist at the URL specified in the EML this may be set to `FALSE`. Note that file identities (md5 hashes) are not checked.
* `multi_part` controls whether data entities are pushed to the s3 bucket with a multi-part upload (`FALSE` by default). This is useful for large files, and required beyond a certain filesize threshold.
* `skip_checks` controls whether congruence between attribute metadata and data files is checked (`FALSE` by default). When the data are not locally present, for example when they are hosted at ezEML, this can be set to `TRUE` to avoid a missing file error. Make sure congruence has been checked in the remote location first.

### EML to metabase (Meta-ingress)

Working on this!

## Other functions

There are some lower-level functions that might occasionally be useful... (will document these later)
