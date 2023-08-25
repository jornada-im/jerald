# build_dataset.999.R
# 
# BOILERPLATE >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# This is a template build script using R to prepare a dataset
# for EDI. You can safely remove this and other boilerplate
# and use the rest to design a new R script for your data.
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


# Set the working directory to a local or network share path
# (this only works in RStudio). 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# If this fails try something like these:
# setwd('/Volumes/unix/path/to/datasets/ds999.../')
# setwd('Z:\\windows\path\to\datasets\ds999...\)

library(tidyverse)

# Path to incoming source data files
dsource <- "./source_data/"

# Output data file name
f_out <- "ds999_mtcars.csv"

# read in mtcars (our example data)
df_in <- mtcars
# In reality do something like:
# df_in <- read_csv(paste0(dsource, "npp_values_20200915.csv"), 
#		  skip = 12, na = c('nan', '.', '-9999'))

# Now create a new column with the rownames and remove some columns in
# a data file for export
df_in$type <- row.names(df_in)

df.export <- df_in %>%
  dplyr::select(type, mpg, wt, cyl, gear)

# In more complex cases with a csv you may want to assign data type
#df_in <- read_csv('Tromble_Weir_Precip_data_EDI.csv',
#                  col_types = cols(Year = col_character(),
#                                   Month = col_character(),
#                                   Day = col_character(),
#                                   Hour = col_character(),
#                                   Minute = col_character(),
#                                   Second = col_character()))

# Mutating columns is good sometimes also
#df.export <- df_in %>%
#  mutate(date = as.Date(paste(Year,Month,Day,Hour,Minute,Second, sep='-'),
#                        format="%Y-%m-%d-%H-%M-%S")) %>%
#  select(date, R_tower, R2, R3, R4)

# Check for NAs and unique values of catvars
sapply(df.export, function(x) sum(is.na(x)))
unique(df.export$type)

# Export df.export as a csv to current directory (no rownames or quoting)
options(scipen=999)   # turns of scientific notation
write.csv(df.export, f_out, quote=F, row.names=F)

