#' Create EML list from an LTER metabase using MetaEgress
#'
#' This function queries an LTER metabase and formats the metadata into an
#' R list in the EML schema
#'
#' @param datasetid Number or numeric vector of dataset IDs to query
#' @param mb_cred Credentials for metabase (a list, see `load_metabase_cred`)
#' @param skip_checks Boolean value (T/F) indicating whether or not to check
#' for congruence between data entity and attribute metadata 
#' (check_attribute_congruence function). May want to set as True if the data
#' are online and not in the working directory.
#' @return An EML-schema-formatted list of metadata (EML R package compliant)
#' @export
eml_egress <- function(datasetid, mb_cred,
                       skip_checks=FALSE){
  # Get metadata (list of dataframes) from metabase
  message('MetaEgress is collecting metadata for ', datasetid, ' from LTER ',
          'Metabase ', mb_cred$dbname, '...')
  metadata <- do.call(MetaEgress::get_meta, 
                      c(list(dataset_ids = c(datasetid)),
                        mb_cred)) # assigned in cred file
  # Create a nested list of dataTables and otherEntities formatted for the
  # EML document
  message('Generating entity table...')
  entity_list <- MetaEgress::create_entity_all(meta_list =  metadata,
                                            file_dir = getwd(),
                                            dataset_id = datasetid,
                                            skip_checks = skip_checks)
  # Create an EML schema list object (nested list)
  message('Creating EML schema list...')
  eml_list <- MetaEgress::create_EML(meta_list = metadata,
                                     entity_list = entity_list,
                                     dataset_id = datasetid,
                                     expand_taxa = TRUE,
                                     skip_taxa = FALSE)
  message('Done.\n')
  return(eml_list)	
}

#' Create EML-compliant XML file from an R list
#'
#' Take an EML object (EML packages R list in EML schema), validate it,
#' and write it to a file
#'
#' @param eml_list An EML-schema-formatted R list (EML R package compliant)
#' @param fname Name of the EML file to create
#' @export
eml_serialize <- function(eml_list, fname){
  # Validate and serialize (write) EML document
  message('Validating EML...')
  out <- EML::eml_validate(eml_list)
  message(out)
  message('Writing EML...')
  EML::write_eml(eml_list, file=fname)
  message('Done.\n')
}

#' Get dataTable and otherEntity filenames from EML
#'
#' This function extracts the data entity filenames (dataTables and 
#' otherEntities) from an EML-schema-formatted list and returns a vector
#' of the filenames (useful for upload to s3 buckets).
#' 
#' @param eml_list An EML-schema-formatted R list (EML R package compliant)
#' @return A list of data entity filenames found in the EML
#' @export
get_eml_entities <- function(eml_list){
  # Get dataTable filenames from EML
  entityname_list <- c()
  if (length(eml_list$dataset$dataTable) > 0){
    for (i in 1:length(eml_list$dataset$dataTable)){
      entityname_list <- append(entityname_list,
			eml_list$dataset$dataTable[[i]]$physical$objectName)
    }
  }
  # Also add other entities
  if (length(eml_list$dataset$otherEntity) > 0){
    for (i in 1:length(eml_list$dataset$otherEntity)){
      entityname_list <- append(entityname_list,
			eml_list$dataset$otherEntity[[i]]$physical$objectName)
    }
  }
  return(entityname_list)
}

#' Insert markdown methodstep element into an EML document
#'
#' This function inserts a markdown methods section into a methodstep for an
#' EML document. Supply a markdown file and it will be inserted into the first
#' methodstep.
#' 
#' @param eml_list An EML-schema-formatted R list (EML R package compliant)
#' @param md_file A markdown methods file (full path)
#' @return An updated EML-schema-formatted R list (EML R package compliant)
#' @export
insert_methodstep_md <- function(eml_list, md_file, islist=TRUE){
  methodmd <- readChar(md_file, file.info(md_file)$size)
  # Add the methods text as a list called "markdown" (becomes <markdown> element)
  if (islist){
    eml_list$dataset$methods$methodStep[[1]]$description <- list(markdown=methodmd)
  } else {
    eml_list$dataset$methods$methodStep$description <- list(markdown=methodmd)
  }
  # Validate and return
  out <- EML::eml_validate(eml_list)
  message(out)
  return(eml_list)
}

#' Insert dataSource XML elements into an EML document
#'
#' This function inserts <dataSource> elements into a methodstep for an
#' EML document. Supply an XML file with a list of data sources and it
#' will be inserted into the first methodstep.
#' 
#' @param eml_list An EML-schema-formatted R list (EML R package compliant)
#' @param xml_file XML file with one or more <dataSource> elements (full path)
#' @return An updated EML-schema-formatted R list (EML R package compliant)
#' @export
insert_datasource_xml <- function(eml_list, xml_file, islist=TRUE){
  dsources <- EML::read_eml(xml_file)
  # Add the list of data source elements (becomes <dataSource> list element)
  if (islist){
    eml_list$dataset$methods$methodStep[[1]]$dataSource <- dsources$dataSource
  } else {
    eml_list$dataset$methods$methodStep$dataSource <- dsources$dataSource
  }
  # Validate and return
  out <- EML::eml_validate(eml_list)
  message(out)
  return(eml_list)
}