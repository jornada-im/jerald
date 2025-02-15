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


The requirements listed above should be pulled in at install time if you don't already have them.

Once `jerald` is installed you will need to direct it to stored credentials for accessing the metadata database and the necessary web resources, which include a data repository (usually EDI) and an s3 bucket. Ask your lead IM or administrator for help with this.

## IM tasks and relevant jerald functions

`jerald` has several user-facing functions, described below, that will help accomplish most data management and publication tasks performed by information managers at the Jornada. A template script that uses these functions to update an example dataset (id=999) on EDI is provided when making a new dataset directory using the instructions below, or is available at `inst/template/build_eml.999.R`. This script gives a demonstration of a basic Jornada workflow.

### Create a new dataset directory or migrate an older one

`jerald` includes a number of templates (see `inst/template/`) for setting up a data management project directory and scripts. To create a new dataset directory and populate with recommended files and subdirectories, run:

    template_dataset_dir(datasetid)

where `datasetid` is a unique identifier for the dataset in the Metabase and at EDI.

An existing dataset directory formatted for use with [EMLassemblyline](https://ediorg.github.io/EMLassemblyline/) (EAL) can be migrated to `jerald`'s template format using:

    migrate_eal_dir(eal.dir, jerald.dir)

where `eal.dir` is the path to the EAL directory, and `jerald.dir` is the path to a new dataset directory created with `template_dataset_dir`. All metadata and data files from `eal.dir` should be copied into a new `EAL_archive` directory in `jerald.dir`. Be aware that there is the potential for data or metadata loss here, so check that all necessary metadata has been copied before deleting the old directory.

### Building the dataset entities

`jerald` doesn't do this directly yet, but there is a template script called `build_dataset.999.R` that demonstrates how to prepare a tabular data entity. This template script will be created in any new `jerald` dataset directory (using `template_dataset_dir`) or can be found in `inst/template/`.

### Update or create a dataset on EDI

There are several user functions for publishing data to EDI. The first step is to load credentials for your Metabase and web resources:

    load_metabase_cred(mb.pathname)

    load_destination_cred(dest.path)

where `mb.pathname` is the path to a Metabase credentials file and `dest.path` is the directory path containing a `jerald_destination_keys.R` file. Again, your lead IM or system administrator can provide templates for the Metabase credentials file, and will likely set up the destination credentials file for you.

Once the credentials have been loaded you can create and upload datasets at EDI (or possibly other repositories in the future...). To publish a dataset to EDI use:

    publish_dataset_edi(datasetid, mb.name, mb.cred, edi.cred)

where `datasetid` is the unique identifier for the dataset in your Metabase and EDI; `mb.name` is the name of your Metabase and is returned by `load_metabase_cred`; `mb.cred` is a list of credentials for Metabase and is returned by `load_metabase_cred`; `edi.cred` is a list of credentials for EDI and is returned by `load_destination_cred`. For safety, the default arguments for this function will either do a "dry-run" of the process (no update to EDI), or will update the data package at the EDI "staging" repository. You can change both behaviors using the `edi.env` and `dry.run` arguments once you are sure you are ready to publish. 

Make sure to check revision numbers, dataset id, and other details before setting `dry.run=FALSE`. If the dataset is new to EDI repository it should be created with revision number 1, and if it is an update the dataset will be published with a revision number incremented by 1 from what is already present at EDI. Note that a dataset is called a data package in EDI terms.

#### Useful options for `publish_dataset_edi()`

* `dry.run` controls whether publication completes or is a dry run (as explained above). `TRUE` by default.
* `s3.upload` controls whether the data entities will be pushed to the s3 bucket (`TRUE` by default). If the entities already exist at the URL specified in the EML this may be set to `FALSE`. Note that file identities (md5 hashes) are not checked.
* `multi.part` controls whether data entities are pushed to the s3 bucket with a multi-part upload (`FALSE` by default. This is useful for large files, and required beyond a certain filesize threshold.
* `skip_checks` controls whether congruence between attribute metadata and data files is checked (`FALSE` by default). When the data are not locally present, for example when they are hosted at ezEML, this can be set to `TRUE` to avoid a missing file error. Make sure congruence has been checked in the remote location first.

## Other functions

There are some lower-level functions that might occasionally be useful... (will document these later)
