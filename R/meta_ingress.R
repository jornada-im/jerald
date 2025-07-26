#library("tidyverse")

#' Create a list of template tables for metabase

#' Create the DataSet table from an EML list (emld object)
#'
#' @param dsid The dataset id value, which is the primary key for lter_metabase
#' @param eml An emld object derived from an EML file
#' @param outpath The path to output an abstract markdown file to. Defaults to
#' current working directory
#' @param boilerplate_id The identifier of a "boilerplate" record in metabase,
#' which will set the project tree and other elements.
#' @return A named list containing a dataframe formatted to match the 
#' lter_metabase."DataSet" table
#' @export
format_DataSet <- function(dsid, eml,
                           revnum=0,
                           outpath=getwd(),
                           boilerplate_id="jrn-default") {
  # Write out the Abstract
  lines <- character(length(eml$dataset$abstract$para))
  for (i in 1:length(lines)) {
    print(eml$dataset$abstract$para[[i]])
    lines[[i]] <- eml$dataset$abstract$para[[i]]
  }
  outfile = paste0("abstract.", dsid, ".ingress.md")
  fileConn<-file(paste0(outpath, "/", outfile))
  message(paste0("Writing ", paste0(outpath, "/", outfile)))
  writeLines(lines, fileConn)
  close(fileConn)

  # Now populate the DataSet table (one row for a new dataset)
  # Setting some defaults here... could revise
  mbtable <- data.frame(
    "DataSetID"= dsid,
    "Revision"= revnum,
    "Title"= eml$dataset$title,
    "PubDate"= return_if_node_exists(eml$dataset$pubDate),
    "Abstract"= outfile, #unlist(eml$dataset$abstract),
    "ShortName"= return_if_node_exists(eml$dataset$shortName),
    "UpdateFrequency"= return_if_node_exists(eml$dataset$maintenance$maintenanceUpdateFrequency),
    "MaintenanceDescription"= return_if_node_exists(eml$dataset$maintenance$description),
    "AbstractType"= "file",
    "BoilerplateSetting"= boilerplate_id
  )
  # Return a named list
  return(list("DataSet" = mbtable))
}

# WIP!!!!
format_DataSet2 <- function(mb, eml,
                           revnum=0,
                           outpath=getwd(),
                           boilerplate_id="jrn-default") {
  # Get the metabase datasetID and compare to eml
  dsid <- mb$DataSetID
  if (dsid != strsplit(eml$packageId, "\\.")[[1]][[2]]){
    message("Warning: the metabase and eml dataset IDs don't match!")
  }


  # Write out the Abstract
  lines <- character(length(eml$dataset$abstract$para))
  for (i in 1:length(lines)) {
    print(eml$dataset$abstract$para[[i]])
    lines[[i]] <- eml$dataset$abstract$para[[i]]
  }
  outfile = paste0("abstract.", dsid, ".ingress.md")
  fileConn<-file(paste0(outpath, "/", outfile))
  message(paste0("Writing ", paste0(outpath, "/", outfile)))
  writeLines(lines, fileConn)
  close(fileConn)

  # Now populate the DataSet table (one row for a new dataset)
  # Setting some defaults here... could revise
  mbtable <- data.frame(list(dsid, revnum,
    eml$dataset$title,
    return_if_node_exists(eml$dataset$pubDate),
    outfile, #unlist(eml$dataset$abstract),
    return_if_node_exists(eml$dataset$shortName),
    return_if_node_exists(eml$dataset$maintenance$maintenanceUpdateFrequency),
    return_if_node_exists(eml$dataset$maintenance$description),
    "file",
    boilerplate_id
  )
  )
  
  stopifnot(identical(names(mb$DataSetID), names(mbtable)))

  # Return a named list
  return(list("DataSet" = mbtable))
}

#' Create the DataSetMethod table from an EML list (emld object)
#' 
#' Methods elements vary widely and may have complex nesting structures. In
#' addition, not all methods children are well-represented in metabase. This 
#' extracts all child elements of any <methods> nested at dataset or other 
#' levels in eml, and prints to a file. The file needs editing afterwards, but
#' this is probably a more effient approach for now.
#'
#' @param dsid The dataset id value, which is the primary key for lter_metabase
#' @param eml An emld object derived from an EML file
#' @param outpath The path to output a methods markdown file to. Defaults to
#' current working directory
#' @return A named list containing a dataframe formatted to match the 
#' lter_metabase."DataSetMethod" table
#' @export
format_DataSetMethod <- function(dsid, eml, outpath=getwd()){
  
  # Commented below - code to extract by methodStep - deprecated for now
  # does not work with sections, sampling, instrumentation and other elements
  # # Count methodsteps
  # n_methodsteps <- length(eml$dataset$methods$methodStep)
  # n_sampling <- length(eml$dataset$methods$sampling$samplingDescription)

  # # If just one MethodStep
  # if ("description" %in% names(eml$dataset$methods$methodStep)){
  #   lines <- return_if_node_exists(eml$dataset$methods$methodStep$description)
  # } else {
  # # Loop through multiple MethodSteps if present)
  #   lines <- character(length(eml$dataset$methods$methodStep))
  #   for (i in 1:length(eml$dataset$methods$methodStep)) {
  #     # Get description
  #     print(return_if_node_exists(eml$dataset$methods$methodStep[[i]]$description))
  #     lines[[i]] <- return_if_node_exists(eml$dataset$methods$methodStep[[i]]$description)
  #   }
  # }

  # Unnest the eml and filter any methods elements into lines
  lines <- unlist(eml)[grepl("methods", names(unlist(eml)), fixed=T)]
  # Write out the methods 
  outfile = paste0("methods.", dsid, ".ingress.md")
  fileConn<-file(paste0(outpath, "/", outfile))
  message(paste0("Writing ", paste0(outpath, "/", outfile)))
  writeLines(lines, fileConn)
  close(fileConn)
  # Now make a DataSetMethod table referring to the file (assuming 1 Methods)
  mbtable <- data.frame(
    "DataSetID" = dsid,
    "MethodStepID" = 1,
    "DescriptionType" = "file",
    "Description" = outfile,
    "Method_xml" = NA
  )
  # Return a named list
  return(list("DataSetMethod" = mbtable))
}


#' Create the DataSetMethodProvenance table from an EML list (emld object)
#' 
#' INCOMPLETE!
#'
#' @param dsid The dataset id value, which is the primary key for lter_metabase
#' @param eml An emld object derived from an EML file
#' @param outpath The path to output a methods markdown file to. Defaults to
#' current working directory
#' @return A named list containing a dataframe formatted to match the 
#' lter_metabase."DataSetMethod" table
#' @export
format_DataSetMethodProvenance <- function(dsid, eml, outpath=getwd()){
  # Get the data sources
  # If just one Methodstep
  if ("dataSource" %in% names(eml$dataset$methods$methodStep)){
    dsource_url <- eml$dataset$methods$methodStep$dataSource$distribution$online$url
    dsource <- paste(tail(strsplit(dsource_url, "/")[[1]], 3), collapse=".")
  } else {
  # loop through MethodSteps if present)
    dsources <- character(length(eml$dataset$methods$methodStep))
    for (i in 1:length(lines)) {
      print(eml$dataset$methods$methodStep[[i]]$description$para)
      dsource_url <- eml$dataset$methods$methodStep[[i]]$dataSource$distribution$online$url
      dsources[[i]] <- paste(tail(strsplit(dsource_url, "/")[[1]], 3), collapse=".")
    }
  }
  # Now make a DataSetMethodProvenance table referring to the file (assuming 1 Methods)
  mbtable <- data.frame(
    "DataSetID" = dsid,
    "MethodStepID" = 1,
    "DescriptionType" = "file",
    "Description" = outfile,
    "Method_xml" = NA
  )
  # Return a named list
  return(list("DataSetMethod" = mbtable))
}


#' Create the DataSetEntities table from an EML list (emld object)
#'
#' NOTE: currently this does not handle otherEntities
#'
#' @param dsid The dataset id value, which is the primary key for lter_metabase
#' @param eml An emld object derived from an EML file
#' @return A named list containing a dataframe formatted to match the 
#' lter_metabase."DataSetEntities" table
#' @export
format_DataSetEntities <- function(dsid, eml){

  # First concatenate lists of all dataTable and otherEntity elements
  data_tables <- return_list_if_single(eml$dataset$dataTable)
  other_ents <- return_list_if_single(eml$dataset$otherEntity)

  entities <- do.call("c", list(data_tables, other_ents))

  # Preallocate lists for entities
  entsortorder <- numeric(length(entities))
  entnames <- character(length(entities))
  entdescs <- character(length(entities))
  entrecords <- rep(NA, length(entities))
  filetypes <- character(length(entities))
  urlheads <- character(length(entities))
  subpaths <- rep(NA, length(entities))
  filenames <- character(length(entities))
  addinfo <- rep(NA, length(entities))
  filesizes <-rep(NA, length(entities))
  filesizeunits <- character(length(entities))
  checksums <- character(length(entities))

  # Loop and populate vectors
  for (i in 1:length(entities)) {
    entsortorder[i] <- i
    entnames[i] <- entities[[i]]$entityName
    entdescs[i] <- entities[[i]]$entityDescription
    entrecords[i] <- return_if_node_exists(entities[[i]]$numberOfRecords)
    #filetypes[i] <- eml$... some kind of check
    # The ifelse is for some minor attribute inconsistency in ezEML XML files
    if (length(entities[[i]]$physical$distribution$online$url) > 1){
      filenames[i] <- tail(unlist(strsplit(entities[[i]]$physical$distribution$online$url$url, "/")), n=1)
      urlheads[i] <- unlist(strsplit(entities[[i]]$physical$distribution$online$url$url, filenames[i]))
    } else {
      filenames[i] <- tail(unlist(strsplit(entities[[i]]$physical$distribution$online$url, "/")), n=1)
      urlheads[i] <- unlist(strsplit(entities[[i]]$physical$distribution$online$url, filenames[i]))
    }
    addinfo[i] = return_if_node_exists(entities[[i]]$physical$additionalInfo)
    filesizes[i] <- entities[[i]]$physical$size$size
    filesizeunits[i] <- entities[[i]]$physical$size$unit
    checksums[i] <- return_if_node_exists(entities[[i]]$physical$authentication$authentication)
  }
  # Make dataframe
  mbtable <- data.frame("DataSetID"=dsid,
                        "EntitySortOrder"=entsortorder,
                        "EntityName"=entnames,
                        "EntityType"="dataTable",
                        "EntityDescription"=entdescs,
                        "EntityRecords"=as.numeric(entrecords),
                        "FileType"="csv_B", # sensible default but needs a check
                        "Urlhead"=urlheads,
                        "Subpath"=as.character(subpaths),
                        "FileName"=filenames,
                        "AdditionalInfo"=as.character(addinfo),
                        "FileSize"=as.numeric(filesizes),
                        "FileSizeUnits"=as.character(filesizeunits),
                        "Checksum"=checksums
  )
  # Return a named list
  return(list("DataSetEntities" = mbtable))
}


#' Create the DataSetAttributes and related tables from an EML list (emld object)
#'
#' @param dsid The dataset id value, which is the primary key for lter_metabase
#' @param eml An emld object derived from an EML file
#' @return A named list containing four dataframes formatted to match the 
#' lter_metabase."DataSetAttributes", lter_metabase."ListCodes",
#' lter_metabase."DataSetAttributeEnumeration", and
#' lter_metabase."DataSetAttributeMissingCodes" tables.
#' @export
format_DataSetAttributes <- function(dsid, eml){

  # First concatenate lists of all dataTable and otherEntity elements
  data_tables <- return_list_if_single(eml$dataset$dataTable)

  # Preallocate lists to hold tables
  att_tbls <- vector("list", length = length(data_tables))
  attenum_tbls <- vector("list", length = length(data_tables))
  missval_tbls <- vector("list", length = length(data_tables))
  listcode_tbls <- vector("list", length = length(data_tables))

  # Loop and populate attribute tables
  for (i in 1:length(data_tables)) {
    # Use handy EML::get_attributes
    attList <- EML::get_attributes(data_tables[[i]]$attributeList)

    # 1. Format the attributes table to match metabase (DataSetAttributes)
    att_i <- attList$attributes %>% 
      dplyr::mutate(DataSetID = dsid,
                    EntitySortOrder = i,
                    ColumnPosition = rownames(.),
                    AttributeID = attributeName,
                    AttributeLabel = if("attributeLabel" %in% colnames(.)) attributeLabel else "Unlabeled",
                    # Moosh two colums together and then replace values to mimic
                    # what is in metabase
                    MeasurementScaleDomainID = paste0(measurementScale, domain),
                    MeasurementScaleDomainID = dplyr::case_when(
                        grepl("ratio", MeasurementScaleDomainID) ~ "ratio",
                        grepl("interval", MeasurementScaleDomainID) ~ "interval",
                        grepl("dateTime", MeasurementScaleDomainID) ~ "dateTime",
                        grepl("nominalenum", MeasurementScaleDomainID) ~ "nominalEnum",
                        grepl("ordinalenum", MeasurementScaleDomainID) ~ "ordinalEnum",
                        grepl("nominaltext", MeasurementScaleDomainID) ~ "nominalText",
                        grepl("ordinaltext", MeasurementScaleDomainID) ~ "ordinalText"),
                    DateTimeFormatString = if("formatString" %in% colnames(.)) formatString else NA,
                    DateTimePrecision = if("dateTimePrecision" %in% colnames(.)) dateTimePrecision else NA,
                    TextPatternDefinition = if("definition" %in% colnames(.)) definition else NA,
                    Unit = if("unit" %in% colnames(.)) unit else NA,
                    NumericPrecision = if("precision" %in% colnames(.)) precision else NA,
                    NumberType = if("numberType" %in% colnames(.)) numberType else NA,
                    BoundsMinimum = if("minimum" %in% colnames(.)) minimum else NA, 
                    BoundsMaximum = if("maximum" %in% colnames(.)) maximum else NA) %>%
      dplyr::select(DataSetID, EntitySortOrder, ColumnPosition,
                    ColumnName = attributeName,
                    AttributeID, AttributeLabel,
                    Description = attributeDefinition,
                    StorageType = storageType,
                    MeasurementScaleDomainID,
                    DateTimeFormatString, DateTimePrecision,
                    TextPatternDefinition, Unit,
                    NumericPrecision, NumberType,
                    BoundsMinimum, BoundsMaximum)
    # Add the metabase formatted table to the list
    att_tbls[[i]] <- att_i

    # 2. Create missing values table for metabase (DataSetAttributeMissingCodes)
    miss_i <- attList$attributes %>% 
      dplyr::filter(!is.na(missingValueCodeExplanation)) %>%
      dplyr::mutate(DataSetID = dsid,
                    EntitySortOrder = i) %>%
      dplyr::select(DataSetID, EntitySortOrder, ColumnName=attributeName,
                    MissingValueCodeID=missingValueCode)
    missval_tbls[[i]] <- miss_i

    # 3. Format enumeration table for metabase (DataSetAttributeEnumeration)
    enum_i <- attList$factors %>% 
      dplyr::mutate(DataSetID = dsid,
                    EntitySortOrder = i) %>%
      dplyr::select(DataSetID, EntitySortOrder,ColumnName=attributeName,
                    CodeID=code)
    # Add the metabase formatted table to the list
    attenum_tbls[[i]] <- enum_i


    # 4. Format unique codes to match metabase (ListCodes)
    # First get categorical codes from "factors" table
    code1_i <- attList$factors %>% 
      dplyr::mutate(CodeID = paste("emlCode", code, sep="_")) %>%
      dplyr::select(CodeID, Code=code, CodeExplanation=definition)

    # Then missing codes from attributes table
    code2_i <- attList$attributes %>% 
      dplyr::filter(!is.na(missingValueCodeExplanation)) %>%
      dplyr::mutate(CodeID = paste("emlMiss", missingValueCode, sep="_"),
                    Code = missingValueCode,
                    CodeExplanation = missingValueCodeExplanation) %>%
      dplyr::select(CodeID, Code, CodeExplanation)

    # Concatenate categorical and missing codes
    code_i <- do.call(rbind, list(code1_i, code2_i))
    # join to ListCode list
    listcode_tbls[[i]] <- code_i
  }

  # Now join the collected tables
  atts <- do.call(rbind, att_tbls)
  attenums <- do.call(rbind, attenum_tbls)
  missvals <- do.call(rbind, missval_tbls)
  listcodes <- unique(do.call(rbind, listcode_tbls)) # get unique rows

  # Return a named list
  return(list("DataSetAttributes"=atts,
              "ListCodes"=listcodes,
              "DataSetAttributeEnumeration"=attenums,
              "DataSetAttributeMissingCodes"=missvals)
        )
}

#' Create the DataSetAttributes and related tables from a dataframe 
#'
#' @param dsid The dataset id value, which is the primary key for lter_metabase
#' @param df A dataframe object or list of dataframes
#' @param enum_cols A vector of column names that are categorical
#' @return A named list containing four dataframes formatted to match the 
#' lter_metabase."DataSetAttributes", lter_metabase."ListCodes",
#' lter_metabase."DataSetAttributeEnumeration", and
#' lter_metabase."DataSetAttributeMissingCodes" tables.
#' @export
format_DataSetAttributes_df <- function(dsid, df, enumcols_df = NULL){

  # First concatenate lists of all dataTable and otherEntity elements
  data_tables <- if (is.data.frame(df)) list(df) else df

  # Preallocate lists to hold tables
  att_tbls <- vector("list", length = length(data_tables))
  attenum_tbls <- vector("list", length = length(data_tables))
  missval_tbls <- vector("list", length = length(data_tables))
  listcode_tbls <- vector("list", length = length(data_tables))

  # Loop and populate attribute tables
  for (i in 1:length(data_tables)) {
    # Initialize the attribute table
    # First get column classes. Sometimes class returns more than one item
    classes <- unlist(unname(sapply(df, class)))
    att_i_init <- data.frame(
      attributeName = names(df),
      # Remove the extra class items (difftime)
      storageType = classes[! classes %in% c("difftime")],
      numMissing = unlist(unname(sapply(df, function(x) sum(is.na(x)))))
      )

    # 1. Format the attributes table to match metabase (DataSetAttributes)
    att_i <- att_i_init %>% 
      dplyr::mutate(DataSetID = dsid,
                    EntitySortOrder = i,
                    ColumnPosition = rownames(.),
                    AttributeID = attributeName,
                    AttributeLabel = "Unlabeled",
                    Description = "No description",
                    StorageType = dplyr::case_when(
                        storageType == "factor" ~ "string",
                        storageType == "character" ~ "string",
                        storageType == "integer" ~ "integer",
                        storageType == "numeric" ~ "float",
                        storageType == "Date" ~ "dateTime",
                        storageType == "hms" ~ "dateTime",
                        storageType == "POSIXct" ~ "dateTime",
                        storageType == "POSIXlt" ~ "dateTime",
                        storageType == "logical" ~ "boolean"
                    ),
                    MeasurementScaleDomainID = dplyr::case_when(
                        # Evaluate enumcols argument first, 
                        # then fill StorageType
                        attributeName %in% enumcols_df ~ "nominalEnum",
                        StorageType == "string" ~ "nominalText",
                        StorageType == "integer" ~ "interval",
                        StorageType == "float" ~ "ratio",
                        StorageType == "dateTime" ~ "dateTime",
                        StorageType == "boolean" ~ "nominalEnum",
                    ),
                    DateTimeFormatString = dplyr::case_when(
                        StorageType == "dateTime" ~ "YYYY-MM-DD"),
                    DateTimePrecision = dplyr::case_when(
                        StorageType == "dateTime" ~ 1),
                    TextPatternDefinition = dplyr::case_when(
                        MeasurementScaleDomainID == "nominalText" ~ "any text"),
                    Unit = dplyr::case_when(
                        MeasurementScaleDomainID == "interval" ~ "number",
                        MeasurementScaleDomainID == "ratio" ~ "dimensionless"),
                    NumericPrecision = NA,
                    NumberType = dplyr::case_when(
                        MeasurementScaleDomainID == "interval" ~ "integer",
                        MeasurementScaleDomainID == "ratio" ~ "real"),
                    BoundsMinimum = NA, 
                    BoundsMaximum = NA) %>%
      dplyr::select(DataSetID, EntitySortOrder, ColumnPosition,
                    ColumnName = attributeName,
                    AttributeID, AttributeLabel, Description,
                    StorageType, MeasurementScaleDomainID,
                    DateTimeFormatString, DateTimePrecision,
                    TextPatternDefinition, Unit,
                    NumericPrecision, NumberType,
                    BoundsMinimum, BoundsMaximum)
    # Add the metabase formatted table to the list
    att_tbls[[i]] <- att_i

    # 2. Create missing values table for metabase (DataSetAttributeMissingCodes)
    miss_i <- att_i_init %>% 
      dplyr::filter(numMissing > 0) %>%
      # By default all codes are NA1 - will need review
      dplyr::mutate(DataSetID = dsid,
                    EntitySortOrder = i,
                    MissingValueCodeID = "dfMiss_NA") %>%
      dplyr::select(DataSetID, EntitySortOrder, ColumnName=attributeName,
                    MissingValueCodeID)
    missval_tbls[[i]] <- miss_i

    # 3. Format enumeration table for metabase (DataSetAttributeEnumeration)
    enumcols <- att_tbls[[i]] %>%
      dplyr::filter(MeasurementScaleDomainID=="nominalEnum") %>%
      dplyr::pull(ColumnName) %>% unique()
    enum_i <- df[, enumcols] %>%
      tidyr::pivot_longer(seq_along(enumcols), names_to = "ColumnName",
                          values_to = "CodeID") %>%
      dplyr::mutate(CodeID = paste("dfCode", CodeID, sep="_")) %>%
      unique() %>% dplyr::mutate(DataSetID = dsid, EntitySortOrder = i) %>%
      dplyr::select(DataSetID, EntitySortOrder,ColumnName, CodeID)
    # Add the metabase formatted table to the list
    attenum_tbls[[i]] <- enum_i


    # 4. Format unique codes to match metabase (ListCodes)
    # First get categorical codes from "factors" table
    code1_i <- attenum_tbls[[i]] %>%
      dplyr::mutate(Code = stringr::str_replace(CodeID, "dfCode_", ""),
                    CodeExplanation = paste("Explanation for enumerated code ", CodeID, sep=" ")) %>%
      dplyr::select(CodeID, Code, CodeExplanation)

    # Then missing codes from attributes table
    code2_i <- missval_tbls[[i]] %>% 
      dplyr::mutate(Code = "NA",
                    CodeExplanation = paste("Explanation for missing code ",
                    MissingValueCodeID, sep=" ")) %>%
      dplyr::select(CodeID=MissingValueCodeID, Code, CodeExplanation)

    # Concatenate categorical and missing codes
    code_i <- do.call(rbind, list(code1_i, code2_i))
    # join to ListCode list
    listcode_tbls[[i]] <- code_i
  }

  # Now join the collected tables
  atts <- do.call(rbind, att_tbls)
  attenums <- do.call(rbind, attenum_tbls)
  missvals <- do.call(rbind, missval_tbls)
  listcodes <- unique(do.call(rbind, listcode_tbls)) # get unique rows

  # Return a named list
  return(list("DataSetAttributes"=atts,
              "DataSetAttributeMissingCodes"=missvals,
              "DataSetAttributeEnumeration"=attenums,
              "ListCodes"=listcodes)
        )
}

#' Suggest coded values from metabase ListCodes to match ListCodes from eml
#'
#' @param from_eml A dataframe formatted to match the lter_metabase."ListCodes"
#' table - taken from an EML file
#' @param from_metabase A dataframe formatted to match the 
#' lter_metabase."ListCodes" table - taken from an LTER metabase table
#' @return A table merging eml- and metabase-derived ListCodes tables on "Code"
#' @export
suggest_ListCodes <- function(from_eml, from_metabase){
  # make some ListCode suggestions
  samecode <- from_metabase[from_metabase$Code %in% from_eml$Code,]
  suggested <- merge(from_eml, samecode, by="Code", all=T, suffixes=c("_eml","_metabase"))
  return(suggested)
}


#' Create the DataSetKeywords and related tables from an EML list (emld object)
#'
#' @param dsid The dataset id value, which is the primary key for lter_metabase
#' @param eml An emld object derived from an EML file
#' @return A named list containing dataframes formatted to match the 
#' lter_metabase."DataSetKeywords" and lter_metabase."ListKeywords" tables
#' @export
format_DataSetKeywords <- function(dsid, eml){
  # Take the KeywordSet list and form into a rough dataframe
  for(i in 1:length(eml$dataset$keywordSet)){
    df <- as_tibble(eml$dataset$keywordSet[[i]])
    # If keywordset has >1 element the keyword column is a list ()
    if(is.list(df$keyword)){
      # If no keywordType attributes unnest a 1 element keyword list
      if(length(df$keyword[[1]])==1){
        df <- unnest(df, cols=c(keyword))
      # With keywordType attributes we have 2 element lists to unnest
      } else if(length(df$keyword[[1]])>1){
        df <- unnest_wider(df, keyword)
        }
    }
    # Add keywordType column if missing
    if(!("keywordType" %in% names(df))){
      df["keywordType"] <- NA
    }
    # Join tibbles from each element
    if(i==1){df1 <- df
    } else {df1 <- rbind(df1, df)}
  }

  # Replace missing and common thesaurus values with metabase equivalents
  df1 <- df1 %>%  
    dplyr::mutate(Keyword = if_else(is.na(keyword), "NA", keyword),
      ThesaurusID = if_else(is.na(keywordThesaurus), "none", keywordThesaurus),
      ThesaurusID = gsub("\\(No thesaurus\\)", "none", ThesaurusID),
      ThesaurusID = if_else(grepl("lter core", tolower(ThesaurusID)),
                                  "lter_cv_cra", ThesaurusID),
      ThesaurusID = if_else(grepl("lter controlled", tolower(ThesaurusID)),
                                  "lter_cv", ThesaurusID)
    )
  # Reformat to get the DataSetKeywords table
  dskws <- df1 %>%
    dplyr::mutate(DataSetID = dsid) %>%
    dplyr::select(DataSetID, Keyword, ThesaurusID)
  # Reformat for ListKeywords table
  lkws <- df1 %>% 
    dplyr::mutate(KeywordType = ifelse(is.na(keywordType), "theme", keywordType)) %>%
    dplyr::select(Keyword, ThesaurusID, KeywordType)

  # Return a named list
  return(list("DataSetKeywords" = dskws, "ListKeywords" = lkws))
}


#' Suggest keywords from metabase ListKeywords to match ListKeywords from eml
#'
#' @param from_eml A dataframe formatted to match the lter_metabase."ListKeywords"
#' table - taken from an EML file
#' @param from_metabase A dataframe formatted to match the 
#' lter_metabase."ListKeywords" table - taken from an LTER metabase table
#' @return A table merging eml- and metabase-derived ListKeywords tables on "Keyword"
#' @export
suggest_Keywords <- function(from_eml, from_metabase){
  # make some ListKeywords suggestions
  samekw <- from_metabase[from_metabase$Keyword %in% from_eml$Keyword,]
  suggested <- merge(from_eml, samekw, by="Keyword", all=T, suffixes=c("_eml","_metabase"))
  return(suggested)
}


#' Create the DataSetPersonnel and related tables from an EML list (emld object)
#'
#' @param dsid The dataset id value, which is the primary key for lter_metabase
#' @param eml An emld object derived from an EML file
#' @return A named list containing dataframes formatted to match the 
#' lter_metabase."DataSetPersonnel", lter_metabase."ListPeople", and
#' lter_metabase."ListPeopleID" tables
#' @export
format_DataSetPersonnel <- function(dsid, eml){

  # First concatenate lists of all the responsibleParties elements (creator,
  # associatedParty, contact)
  creators <- return_list_if_single(eml$dataset$creator)
  assoc_parties <- return_list_if_single(eml$dataset$associatedParties)
  contacts <- return_list_if_single(eml$dataset$contact)

  parties <- do.call("c", list(creators, contacts, assoc_parties))

  # This doesn't work but there is probably a slick way to linearize the 
  # nested elements into a dataframe
  #df1 <- purrr::map(eml$dataset$creator, as_tibble) %>% 
  #  list_rbind() %>% 
  #  unnest_wider(individualName, names_repair = "unique" ,names_sep = "x")

  # Preallocate lists to hold personnel info
  partysortord <- numeric(length(parties))
  nameid <- character(length(parties))
  givennm <- rep(NA, length(parties))
  middlenm <- rep(NA, length(parties))
  surnm <- rep(NA, length(parties))
  org <- rep(NA, length(parties))
  add1 <- rep(NA, length(parties))
  add2 <- rep(NA, length(parties))
  add3 <- rep(NA, length(parties))
  city <- rep(NA, length(parties))
  state <- rep(NA, length(parties))
  country <- rep(NA, length(parties))
  zipcode <- rep(NA, length(parties))
  email <- rep(NA, length(parties))
  weburl <- rep(NA, length(parties))
  phone <- rep(NA, length(parties))
  pos <- rep(NA, length(parties))
  iddir <- rep(NA, length(parties))
  id <- rep(NA, length(parties))
  roles <- character(length(parties))

  # Loop and populate personnel tables
  for (i in 1:length(parties)) {
    givennm[i] <- return_if_node_exists(parties[[i]]$individualName$givenName, 1)
    middlenm[i] <- return_if_node_exists(parties[[i]]$individualName$givenName, 2)
    surnm[i] <- return_if_node_exists(parties[[i]]$individualName$surName)
    nameid[i] <- paste0(tolower(substr(givennm[i], 1, 1)), tolower(surnm[i]))
    org[i] <- return_if_node_exists(parties[[i]]$organizationName)
    email[i] <- return_if_node_exists(parties[[i]]$electronicMailAddress)
    pos[i] <- return_if_node_exists(parties[[i]]$positionName)
    iddir[i] <- return_if_node_exists(parties[[i]]$userId, "directory")
    id[i] <- return_if_node_exists(parties[[i]]$userId, "userId")
    # Since we joined contacts, assoc, and contacts, AuthorshipRole varies with
    # iteration. Choose the value depending what part of list we're in
    if (i %in% 1:length(creators)){
      roles[i] <- "creator"
      partysortord[i] <- i
    } else if (i %in% (length(creators)+1):(length(creators)+length(contacts))){
      roles[i] <- "contact"
      partysortord[i] <- i - length(creators)
    } else {
      roles[i] <- parties[[i]]$role
      partysortord[i] <- i - (length(creators)+length(contacts))
    }
  }
  # Create the DataSetPersonnel, ListPeople, and ListPeopleID dataframes
  parties_ds <- data.frame("DataSetID"=dsid,
                            "NameID"=nameid,
                            "AuthorshipOrder"=partysortord,
                            "AuthorshipRole"=roles)

  parties_l <- data.frame("NameID"=nameid,
                           "GivenName"=as.character(givennm),
                           "MiddleName"=as.character(middlenm),
                           "SurName"=as.character(surnm),
                           "Organization"=as.character(org),
                           "Address1"=as.character(add1),
                           "Address2"=as.character(add2),
                           "Address3"=as.character(add3),
                           "City"=as.character(city),
                           "State"=as.character(state),
                           "Country"=as.character(country),
                           "ZipCode"=as.character(zipcode),
                           "Email"=as.character(email),
                           "WebPage"=as.character(weburl),
                           "Phone"=as.character(phone),
                           "Position"=pos)

  parties_l_id <- data.frame("NameID"=nameid,
                             "IdentificationID"=1,
                             "IdentificationSystem"=as.character(iddir),
                             "IdentificationURL"=as.character(id))

  # Return a named list
  return(list("DataSetPersonnel" = parties_ds, "ListPeople" = parties_l,
              "ListPeopleID" = parties_l_id))
}


#' Create the DataSetTemporal table from an EML list (emld object)
#'
#' @param dsid The dataset id value, which is the primary key for lter_metabase
#' @param eml An emld object derived from an EML file
#' @return A named list containing a dataframe formatted to match the 
#' lter_metabase."DataSetTemporal" table
#' @export
format_DataSetTemporal <- function(dsid, eml){
  # Populate the DataSetTemporal table (assuming 1 row)
  # Setting some defaults here... could revise
  mbtable <- data.frame(
    "DataSetID"=dsid,
    "EntitySortOrder"=0,
    "BeginDate"=eml$dataset$coverage$temporalCoverage$rangeOfDates$beginDate$calendarDate,
    "EndDate"=eml$dataset$coverage$temporalCoverage$rangeOfDates$endDate$calendarDate,
    "UseOnlyYear"=FALSE
    )
  return(list("DataSetTemporal" = mbtable))
}


#' Create the DataSetSites and related tables from an EML list (emld object)
#'
#' @param dsid The dataset id value, which is the primary key for lter_metabase
#' @param eml An emld object derived from an EML file
#' @return A named list containing dataframes formatted to match the 
#' lter_metabase."DataSetSites" and lter_metabase."ListSites" tables
#' @export
format_DataSetSites <- function(dsid, eml){
  # Get the geographic coverage elements as a list
  geocov <- return_list_if_single(eml$dataset$coverage$geographicCoverage)

  # Preallocate lists to hold sites info
  siteid <- character(length(geocov))
  entsortord <- numeric(length(geocov))
  geosortord <- numeric(length(geocov))
  sitetype <- character(length(geocov))
  sitename <- rep(NA, length(geocov))
  siteloc <- rep(NA, length(geocov))
  sitedesc <- character(length(geocov))
  owners <- rep(NA, length(geocov))
  shapetype <- character(length(geocov))
  centlon <- rep(NA, length(geocov))
  centlat <- rep(NA, length(geocov))
  wlon <- numeric(length(geocov))
  elon <- numeric(length(geocov))
  slat <- numeric(length(geocov))
  nlat <- numeric(length(geocov))
  altmin <- rep(NA, length(geocov))
  altmax <- rep(NA, length(geocov))
  altunit <- rep(NA, length(geocov))

  # Loop and populate Geo Coverage tables
  for (i in 1:length(geocov)) {
    siteid[i] <- paste0("unknown", i)
    entsortord[i] <- 0
    geosortord[i] <- i
    # Test if the east/west and north/south bounding coords are the same
    test1 <- (geocov[[i]]$boundingCoordinates$westBoundingCoordinate ==
                geocov[[i]]$boundingCoordinates$eastBoundingCoordinate)
    test2 <- (geocov[[i]]$boundingCoordinates$northBoundingCoordinate ==
                geocov[[i]]$boundingCoordinates$southBoundingCoordinate)
    # If they are its a point, and fill in center coords
    if (test1 & test2){
      sitetype[i] <- "point"
      shapetype[i] <- "point"
      centlon[i] <- geocov[[i]]$boundingCoordinates$westBoundingCoordinate
      centlat[i] <- geocov[[i]]$boundingCoordinates$northBoundingCoordinate
    # Otherwise its a bbox
    } else {
      sitetype[i] <- "bbox"
      shapetype[i] <- "polygon"
    }
    sitedesc[i] <- return_if_node_exists(geocov[[i]]$geographicDescription)
    wlon[i] <- geocov[[i]]$boundingCoordinates$westBoundingCoordinate
    elon[i] <- geocov[[i]]$boundingCoordinates$eastBoundingCoordinate
    slat[i] <- geocov[[i]]$boundingCoordinates$southBoundingCoordinate
    nlat[i] <- geocov[[i]]$boundingCoordinates$northBoundingCoordinate
  }

  # Create the DataSetSites and ListSites dataframes
  sites_ds <- data.frame("DataSetID"=dsid,
                         "EntitySortOrder"=entsortord,
                         "SiteID"=siteid,
                         "GeoCoverageSortOrder"=geosortord)

  sites_l <- data.frame("SiteID"=siteid,
                        "SiteType"=sitetype,
                        "SiteName"=as.character(sitename),
                        "SiteLocation"=as.character(siteloc),
                        "SiteDescription"=as.character(sitedesc),
                        "Ownership"=as.character(owners),
                        "ShapeType"=as.character(shapetype),
                        "CenterLon"=as.numeric(centlon),
                        "CenterLat"=as.numeric(centlat),
                        "WBoundLon"=as.numeric(wlon),
                        "EBoundLon"=as.numeric(elon),
                        "SBoundLat"=as.numeric(slat),
                        "NBoundLat"=as.numeric(nlat),
                        "AltitudeMin"=as.numeric(altmin),
                        "AltitudeMax"=as.numeric(altmax),
                        "AltitudeUnit"=as.character(altunit)
  )
  # Return a named list
  return(list("DataSetSites" = sites_ds, "ListSites" = sites_l))
}


#' Append a formatted dataframe to an LTER metabase table
#'
#' @param conn Connection to an LTER Metabase instance
#' @param tablename Name (char) of a table in the given schema in LTER Metabase
#' @param df A dataframe formatted to match the given LTER Metabase table
#' @param schema Name (char) of the LTER Metabase schema to target 
#' ("lter_metabase" by default)
#' @return Nothing
#' @export
ingress_table <- function(conn, tablename, df, schema="lter_metabase"){
  # Write to the named table and print output
  out <- RPostgres::dbWriteTable(conn, DBI::Id(schema = schema,
                                              table = tablename),
                                 df, row.names=FALSE,
                                 append=TRUE)
  message(out)
}


#' Get an EML element at a specified address if it exists in the EML list (emld)
#'
#' @param qlist An EML list address to query
#' @param nodeid Name (char) or index (int) of the element to check existence of
#' and return if present in qlist
#' @return NA if address is NULL or nodeid doesn't exist, otherwise the element
#' value at qlist/nodeid
#' @export
return_if_node_exists <- function(qlist, nodeid=1){
  # First check if qlist is null (not present)
  if (is.null(qlist)){
    val = NA
  }
  # If present return a named (character) element in qlist, or NA if not present
  else if (class(nodeid)=="character"){
    if (exists(nodeid, where=qlist)) {
      val = qlist[[nodeid]]
    } else {
      val = NA
    }
  # Return an indexed element in qlist, or NA if not present (first is default)
  } else if (class(nodeid)=="numeric"){
    if (length(qlist) >= nodeid){
      val = qlist[[nodeid]]
    } else {
      val = NA
    }
  }
  return(val)
}


#' Return single EML elements at a specified address as a list
#' 
#' This helps standardize EML element types into lists for iteration
#'
#' @param qlist An EML list address to query
#' @param nodeid Name (char) or index (int) of the element to check existence of
#' and return if present in qlist
#' @return NULL if qlist is NULL (doesn't exist), qlist if it is an unnamed list
#' (list of element types), and list(qlist) if it contains named elements 
#' (single, unlisted element type).
#' @export
return_list_if_single <- function(qlist){
  # If the node is not present (NULL), return NULL
  if (is.null(qlist)){
    return(NULL)
  }
  # If there are no named elements in the list, it is a list of EML element
  # types - return it.
  else if (is.null(names(qlist))){
    return(qlist)
  } 
  # Otherwise there are named elements and its a single element of a type,
  # so return it inside a list
  else {
    return(list(qlist))
  }
}