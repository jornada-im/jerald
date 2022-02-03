# build_eml.210000000.R
# 
# BOILERPLATE >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# This is a template build script using R to prepare eml and 
# send a dataset to EDI using the jerald R package. You need 
# credentials for this to work, and there is a template credentials
# file in jrn-metabase-utils repository.
#
# Required metadata documents (abstract, methods) and any data entity
# files (CSVs, images, zipfiles etc.) must be in the directory with 
# this script. The data entities, abstract, and methods files
# should be named to match the values in the lter-metabase 
# (DataSetEntities.FileName, DataSet.Abstract and 
# DataSetMethod.Description).
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# Set the working directory to a local or network share path
# (this only works in RStudio). 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# If this fails try something like these:
# setwd('/Volumes/unix/path/to/datasets/210.../')
# setwd('Z:\\windows\path\to\datasets\210...\)

options(scipen=999)   # turns off scientific notation

library('jerald')

# Dataset ID
id <- 210000000
# EDI environment
env <- 'staging'
# Metabase, EDI, and s3 credential paths
mbcred_path <- '/path/to/jrn_metabase_keys.R'
destcred_path <- '/path/to/directory/containing/jerald_destination_keys.R/'

# Get credentials for JRN metabase
# see a template for this file: 
#   system.file('template', 'metabase_keys.template.R',
#               package='jerald')
load_metabase_cred(mbcred_path)

# get credentials for s3 and repository destinations
# see a template for this file: 
#   system.file('template', 'jerald_destination_keys.template.R',
#               package='jerald')
load_destination_cred(destcred_path)

# Now create or update the dataset on EDI...
# Note that you must pass `publish=TRUE` to actually
# publish the data. Only do that when you have checked
# dataset identifiers, revision numbers, eml, etc.
# create_dataset_edi(id, mbname, mbcred, edicred, edi.env=env)
update_dataset_edi(id, mbname, mbcred, edicred, edi.env=env)

# Clean up
remove(mbcred, edicred, mbname, mbcred_path, destcred_path)

