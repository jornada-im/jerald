#' Create EML list from an LTER metabase using MetaEgress
#'
#' This function queries an LTER metabase and formats the metadata into an
#' R list in the EML schema
#'
#' @param datasetid Number or numeric vector of dataset IDs to query
#' @param mb.name Name of the LTER metabase to query metadata from
#' @param mb.cred Credentials for mb.name (a list, see `load_metabase_cred`)
#' @param skip_checks Boolean value (T/F) indicating whether or not to check
#' for congruence between data entity and attribute metadata 
#' (check_attribute_congruence function). May want to set as True if the data
#' are online and not in the working directory.
#' @return An EML-schema-formatted list of metadata (EML R package compliant)
#' @export
eml_egress <- function(datasetid, mb.name, mb.cred,
                       skip_checks=FALSE){
  # Get metadata from metabase
  message('MetaEgress is collecting metadata for ', datasetid, ' from LTER ',
          'Metabase ', mb.name, '...')
  metadata <- do.call(MetaEgress::get_meta, 
                      c(list(dbname = mb.name,
                             dataset_ids = c(datasetid)),
                        mb.cred)) # assigned in cred file
  # Create a list of entities formatted for the EML document
  message('Generating entity table...')
  entities <- MetaEgress::create_entity_all(meta_list =  metadata,
                                            file_dir = getwd(),
                                            dataset_id = datasetid,
                                            skip_checks = skip_checks)
  # Create an EML schema list object
  message('Creating EML schema list...')
  eml.list <- MetaEgress::create_EML(meta_list = metadata,
                                     entity_list = entities,
                                     dataset_id = datasetid,
                                     expand_taxa = TRUE,
                                     skip_taxa = FALSE)
  message('Done.\n')
  return(eml.list)	
}

#' Create EML-compliant XML file from an R list
#'
#' Take an EML object (EML packages R list in EML schema), validate it,
#' and write it to a file
#'
#' @param eml.list An EML-schema-formatted R list (EML R package compliant)
#' @param fname Name of the EML file to create
#' @export
eml_serialize <- function(eml.list, fname){
  # Validate and serialize (write) EML document
  message('Validating EML...')
  out <- EML::eml_validate(eml.list)
  message(out)
  message('Writing EML...')
  EML::write_eml(eml.list, file=fname)
  message('Done.\n')
}

#' Get dataTable and otherEntity filenames from EML
#'
#' This function extracts the data entity filenames (dataTables and 
#' otherEntities) from an EML-schema-formatted list and returns a vector
#' of the filenames (useful for upload to s3 buckets).
#' 
#' @param eml.list An EML-schema-formatted R list (EML R package compliant)
#' @return A list of data entity filenames found in the EML
#' @export
get_eml_entities <- function(eml.list){
  # Get dataTable filenames from EML
  entlist <- c()
  if (length(eml.list$dataset$dataTable) > 0){
    for (i in 1:length(eml.list$dataset$dataTable)){
      entlist <- append(entlist,
			eml.list$dataset$dataTable[[i]]$physical$objectName)
    }
  }
  # Also add other entities
  if (length(eml.list$dataset$otherEntity) > 0){
    for (i in 1:length(eml.list$dataset$otherEntity)){
      entlist <- append(entlist,
			eml.list$dataset$otherEntity[[i]]$physical$objectName)
    }
  }
  return(entlist)
}

#' Insert markdown methodstep element into an EML document
#'
#' This function inserts a markdown methods section into a methodstep for an
#' EML document. Supply a markdown file and it will be inserted into the first
#' methodstep.
#' 
#' @param eml.list An EML-schema-formatted R list (EML R package compliant)
#' @param md_file A markdown methods file (full path)
#' @return An updated EML-schema-formatted R list (EML R package compliant)
#' @export
insert_methodstep_md <- function(eml.list, md_file, islist=TRUE){
  methodmd <- readChar(md_file, file.info(md_file)$size)
  # Add the methods text as a list called "markdown" (becomes <markdown> element)
  if (islist){
    eml.list$dataset$methods$methodStep[[1]]$description <- list(markdown=methodmd)
  } else {
    eml.list$dataset$methods$methodStep$description <- list(markdown=methodmd)
  }
  # Validate and return
  out <- EML::eml_validate(eml.list)
  message(out)
  return(eml.list)
}

#' Insert dataSource XML elements into an EML document
#'
#' This function inserts <dataSource> elements into a methodstep for an
#' EML document. Supply an XML file with a list of data sources and it
#' will be inserted into the first methodstep.
#' 
#' @param eml.list An EML-schema-formatted R list (EML R package compliant)
#' @param xml_file XML file with one or more <dataSource> elements (full path)
#' @return An updated EML-schema-formatted R list (EML R package compliant)
#' @export
insert_datasource_xml <- function(eml.list, xml_file, islist=TRUE){
  dsources <- EML::read_eml(xml_file)
  # Add the list of data source elements (becomes <dataSource> list element)
  if (islist){
    eml.list$dataset$methods$methodStep[[1]]$dataSource <- dsources$dataSource
  } else {
    eml.list$dataset$methods$methodStep$dataSource <- dsources$dataSource
  }
  # Validate and return
  out <- EML::eml_validate(eml.list)
  message(out)
  return(eml.list)
}