# build_eml.210000000.R
# 
# BOILERPLATE >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# This is a template build script using R to prepare eml and 
# send a dataset to EDI using the jerald R package. You need 
# credentials for this to work, and there is a template credentials
# file in jrn-metabase-utils repository.
#
# All metadata documents (abstract, methods) and any data entity
# files (CSVs, images, zipfiles etc.) must be in the directory with 
# this script. The data entities, abstract, and methods files
# should be named to match the values in the lter-metabase 
# (DataSetEntities.FileName, DataSet.Abstract and 
# DataSetMethod.Description).
#
# You can safely remove this and other boilerplate and use
# the rest to design a new R script for your dataset.
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

options(scipen=999)   # turns off scientific notation

library('jerald')

# Dataset ID
id <- 210000000
# EDI environment
env <- 'staging'
# Metabase, EDI, and s3 credential paths
mbcred_path <- '~/Desktop/jrn_metabase_keys.R'
destcred_path <- '/media/greg/jrn-DataProducts/LTER_IM/im-keys/'

# Get credentials for JRN metabase
# template for this file: 
#   system.file('template', 'metabase_keys.template.R',
#               package='jerald')
load_metabase_cred(mbcred_path)

# get credentials for s3 and repository destinations
# there is a template for this file: 
#   system.file('template', 'jerald_destination_keys.template.R',
#               package='jerald')
load_destination_cred(destcred_path)

# Now update the dataset on EDI...
# Note that you must pass `publish=TRUE` to actually
# publish the data. Only do that when you have checked
# dataset identifiers, revision numbers, eml, etc.
update_dataset_edi(id, mbname, mbcred, edicred, edi.env=env)

# Clean up
remove(mbcred, edicred, mbname, mbcred_path, destcred_path)

