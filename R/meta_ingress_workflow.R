# Workflow for importing an EML document into an LTER Metabase.
#
# `eml_to_metabase`, at the bottom of this file, is the user-facing function.
# It ties together the `format_*` functions in `meta_ingress.R`. Everything
# above it is either the ingress log or a check that compares the formatted
# tables against what the metabase schema will actually accept.


# ---------------------------------------------------------------------------
# The ingress log
# ---------------------------------------------------------------------------

#' Start an ingress log
#'
#' The log collects notes as an EML document is translated into metabase
#' tables. It is an environment rather than a dataframe so that the check
#' functions can append to it without having to return it.
#'
#' Entries have one of four levels:
#'
#' * `problem` - metabase will reject this; fix it before ingress
#' * `gap` - EML content that was not transferred to metabase
#' * `decision` - a default or a dropped duplicate row, chosen for you
#' * `info` - progress
#'
#' @param dsid The dataset id value, which is the primary key for lter_metabase
#' @return An ingress log
new_ingress_log <- function(dsid) {
  lg <- new.env(parent = emptyenv())
  lg$dsid <- dsid
  lg$started <- Sys.time()
  lg$entries <- list()
  return(lg)
}


#' Add an entry to an ingress log
#'
#' @param lg An ingress log (see `new_ingress_log`)
#' @param level One of "problem", "gap", "decision", or "info"
#' @param tablename Name of the metabase table the note is about, or "-"
#' @param note The text of the note
#' @return The log, invisibly
log_note <- function(lg, level, tablename, note) {
  lg$entries[[length(lg$entries) + 1]] <- data.frame(
    "Level" = level,
    "Table" = tablename,
    "Note" = note,
    stringsAsFactors = FALSE)
  # Everything but routine progress is worth seeing as it happens
  if (level != "info") {
    message("  [", level, "] ", tablename, ": ", note)
  }
  invisible(lg)
}


#' Write an ingress log to a markdown file
#'
#' Entries are grouped by level, most urgent first, so that the things
#' needing attention are at the top of the file.
#'
#' @param lg An ingress log (see `new_ingress_log`)
#' @param path Path of the markdown file to write
#' @return The path written, invisibly
write_ingress_log <- function(lg, path) {
  entries <- do.call(rbind, lg$entries)
  lines <- c(
    paste("# Metabase ingress log for dataset", lg$dsid),
    "",
    paste("* Started:", format(lg$started, "%Y-%m-%d %H:%M:%S")),
    paste("* Finished:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    "",
    "Entries are grouped by level:",
    "",
    "* **problem** - metabase will reject this; fix it before ingress",
    "* **gap** - EML content that was not transferred to metabase",
    "* **decision** - a default or a dropped duplicate row, chosen for you",
    "* **info** - progress",
    "")
  for (level in c("problem", "gap", "decision", "info")) {
    rows <- entries[entries$Level == level, ]
    if (nrow(rows) == 0) next
    lines <- c(lines,
               paste0("## ", level, " (", nrow(rows), ")"),
               "",
               paste0("* `", rows$Table, "` - ", rows$Note),
               "")
  }
  writeLines(lines, path)
  message("Wrote ingress log to ", path)
  invisible(path)
}


# ---------------------------------------------------------------------------
# Reading the metabase schema
# ---------------------------------------------------------------------------

#' Quote a metabase schema and table for use in a SQL statement
#'
#' @param conn Connection to an LTER Metabase instance
#' @param schema Name (char) of the schema
#' @param tablename Name (char) of the table
#' @return A quoted SQL identifier
mb_id <- function(conn, schema, tablename) {
  DBI::dbQuoteIdentifier(conn, DBI::Id(schema = schema, table = tablename))
}


#' Describe the columns of a metabase table
#'
#' @param conn Connection to an LTER Metabase instance
#' @param schema Name (char) of the LTER Metabase schema to target
#' @param tablename Name (char) of a table in that schema
#' @return A dataframe with a column_name, is_nullable, and
#' character_maximum_length column, one row per column of the table
mb_columns <- function(conn, schema, tablename) {
  DBI::dbGetQuery(conn,
    "SELECT column_name, is_nullable, character_maximum_length
       FROM information_schema.columns
      WHERE table_schema = $1 AND table_name = $2",
    params = list(schema, tablename))
}


#' Get the primary key columns of a metabase table
#'
#' @param conn Connection to an LTER Metabase instance
#' @param schema Name (char) of the LTER Metabase schema to target
#' @param tablename Name (char) of a table in that schema
#' @return A character vector of column names, empty if there is no
#' primary key
mb_primary_key <- function(conn, schema, tablename) {
  key <- DBI::dbGetQuery(conn,
    "SELECT kcu.column_name
       FROM information_schema.table_constraints tc
       JOIN information_schema.key_column_usage kcu
         ON kcu.constraint_name = tc.constraint_name
        AND kcu.constraint_schema = tc.constraint_schema
      WHERE tc.constraint_type = 'PRIMARY KEY'
        AND tc.table_schema = $1 AND tc.table_name = $2",
    params = list(schema, tablename))
  return(key$column_name)
}


#' Get the values allowed in a metabase controlled vocabulary column
#'
#' @param conn Connection to an LTER Metabase instance
#' @param schema Name (char) of the schema holding the vocabulary table
#' @param tablename Name (char) of the vocabulary table
#' @param column Name (char) of the column holding the allowed values
#' @return A vector of the values in that column
mb_allowed_values <- function(conn, schema, tablename, column) {
  sql <- paste("SELECT DISTINCT", DBI::dbQuoteIdentifier(conn, column),
               "AS value FROM", mb_id(conn, schema, tablename))
  DBI::dbGetQuery(conn, sql)$value
}


#' Metabase columns constrained by a controlled vocabulary table
#'
#' Each row names a column written by one of the `format_*` functions and the
#' parent table whose values it has to match. An NA ParentSchema means the
#' same schema the dataset is being written to.
#'
#' The `CodeID` and `MissingValueCodeID` foreign keys into `ListCodes` are
#' deliberately absent: `ListCodes` is populated by the same ingress run, so
#' checking it against the database would flag rows that are about to exist.
#'
#' @return A dataframe of the vocabularies to check
mb_cv_columns <- function() {
  data.frame(
    "Table" = c("DataSet", "DataSetEntities", "DataSetAttributes",
                "DataSetAttributes", "DataSetAttributes", "DataSetAttributes",
                "ListKeywords", "ListKeywords", "ListSites"),
    "Column" = c("UpdateFrequency", "FileType", "StorageType",
                 "MeasurementScaleDomainID", "NumberType", "Unit",
                 "ThesaurusID", "KeywordType", "AltitudeUnit"),
    "ParentSchema" = c("pkg_mgmt", NA, NA, NA, NA, NA, NA, NA, NA),
    "ParentTable" = c("cv_maint_freq", "EMLFileTypes", "EMLStorageTypes",
                      "EMLMeasurementScaleDomains", "EMLNumberTypes",
                      "EMLUnitDictionary", "ListKeywordThesauri",
                      "EMLKeywordTypes", "EMLUnitDictionary"),
    "ParentColumn" = c("eml_maintenance_frequency", "FileType", "StorageType",
                       "MeasurementScaleDomainID", "NumberType", "id",
                       "ThesaurusID", "KeywordType", "id"),
    stringsAsFactors = FALSE)
}


#' The order in which metabase tables must be filled
#'
#' This follows the foreign keys: the shared `List*` vocabularies first, then
#' the `DataSet` row, then everything that points at it.
#'
#' @return A character vector of table names
mb_ingress_order <- function() {
  c("ListPeople", "ListPeopleID", "ListSites", "ListCodes", "ListKeywords",
    "DataSet", "DataSetPersonnel", "DataSetKeywords", "DataSetSites",
    "DataSetTemporal", "DataSetMethod", "DataSetEntities",
    "DataSetAttributes", "DataSetAttributeEnumeration",
    "DataSetAttributeMissingCodes")
}


# ---------------------------------------------------------------------------
# Checking formatted tables
# ---------------------------------------------------------------------------

#' Check a formatted table against the metabase schema
#'
#' Compares a dataframe from one of the `format_*` functions against the
#' metabase table it is bound for and logs anything metabase will not accept:
#' columns it does not have, empty values in NOT NULL columns, values too long
#' for their column, and values outside a controlled vocabulary. Nothing is
#' altered - the log tells the information manager what to fix.
#'
#' @param conn Connection to an LTER Metabase instance
#' @param lg An ingress log (see `new_ingress_log`)
#' @param schema Name (char) of the LTER Metabase schema to target
#' @param tablename Name (char) of the table the dataframe is bound for
#' @param df A dataframe formatted to match that table
#' @return The log, invisibly
check_table <- function(conn, lg, schema, tablename, df) {
  cols <- mb_columns(conn, schema, tablename)
  if (nrow(cols) == 0) {
    log_note(lg, "problem", tablename,
             paste0("There is no such table in the '", schema, "' schema."))
    return(invisible(lg))
  }

  # Columns the format_* function produced that metabase does not have
  extra <- setdiff(names(df), cols$column_name)
  if (length(extra) > 0) {
    log_note(lg, "problem", tablename,
             paste0("Formatted with column(s) that metabase does not have: ",
                    paste(extra, collapse = ", "), "."))
  }

  for (i in seq_len(nrow(cols))) {
    colname <- cols$column_name[i]
    if (!colname %in% names(df)) next
    values <- as.character(df[[colname]])

    # NOT NULL columns need a value in every row
    if (cols$is_nullable[i] == "NO" && any(is.na(values))) {
      log_note(lg, "problem", tablename,
               paste0("'", colname, "' is NOT NULL in metabase but the EML ",
                      "supplied no value for ", sum(is.na(values)), " of ",
                      length(values), " row(s)."))
    }

    # Character columns have a maximum width
    maxlen <- cols$character_maximum_length[i]
    if (!is.na(maxlen)) {
      toolong <- which(nchar(values, keepNA = TRUE) > maxlen)
      if (length(toolong) > 0) {
        log_note(lg, "problem", tablename,
                 paste0("'", colname, "' holds at most ", maxlen,
                        " characters but ", length(toolong),
                        " value(s) are longer, starting with '",
                        substr(values[toolong[1]], 1, 40), "'."))
      }
    }
  }

  # Values that have to come from a controlled vocabulary table
  cv <- mb_cv_columns()
  cv <- cv[cv$Table == tablename, ]
  for (i in seq_len(nrow(cv))) {
    colname <- cv$Column[i]
    if (!colname %in% names(df)) next
    parent_schema <- if (is.na(cv$ParentSchema[i])) schema else cv$ParentSchema[i]
    allowed <- mb_allowed_values(conn, parent_schema, cv$ParentTable[i],
                                 cv$ParentColumn[i])
    unknown <- setdiff(unique(stats::na.omit(as.character(df[[colname]]))), allowed)
    if (length(unknown) > 0) {
      log_note(lg, "problem", tablename,
               paste0("'", colname, "' value(s) are not in ", cv$ParentTable[i],
                      ": ", paste(unknown, collapse = ", "),
                      ". Add them there, or correct the EML."))
    }
  }
  invisible(lg)
}


#' Drop rows metabase already has, or that repeat within the formatted table
#'
#' The `List*` tables are shared vocabularies, so an EML document routinely
#' describes people, keywords, sites and codes that metabase already holds. It
#' also repeats them - one person is often both creator and contact. Either
#' way the row already in metabase wins, because it has been curated. Every
#' dropped row is logged so that it can be reconciled by hand.
#'
#' @param conn Connection to an LTER Metabase instance
#' @param lg An ingress log (see `new_ingress_log`)
#' @param schema Name (char) of the LTER Metabase schema to target
#' @param tablename Name (char) of the table the dataframe is bound for
#' @param df A dataframe formatted to match that table
#' @return The dataframe with duplicate and pre-existing rows removed
drop_existing_rows <- function(conn, lg, schema, tablename, df) {
  key <- mb_primary_key(conn, schema, tablename)
  if (length(key) == 0 || !all(key %in% names(df))) {
    log_note(lg, "decision", tablename,
             "Primary key columns are missing, so duplicate rows were not checked.")
    return(df)
  }

  # Identify each row by its primary key, as one string
  row_keys <- function(x) {
    do.call(paste, c(lapply(key, function(k) as.character(x[[k]])), sep = "|"))
  }
  keys <- row_keys(df)

  # Rows that repeat a primary key within the EML
  repeated <- duplicated(keys)
  if (any(repeated)) {
    log_note(lg, "decision", tablename,
             paste0(sum(repeated), " row(s) repeat a primary key already used ",
                    "in this EML, and only the first of each was kept: ",
                    paste(unique(keys[repeated]), collapse = "; "), "."))
    df <- df[!repeated, , drop = FALSE]
    keys <- keys[!repeated]
  }

  # Rows metabase already holds
  existing <- DBI::dbGetQuery(conn,
    paste("SELECT", paste(as.character(DBI::dbQuoteIdentifier(conn, key)),
                          collapse = ", "),
          "FROM", mb_id(conn, schema, tablename)))
  present <- keys %in% row_keys(existing)
  if (any(present)) {
    log_note(lg, "decision", tablename,
             paste0(sum(present), " row(s) are already in metabase and were ",
                    "not re-inserted, so the curated values there are kept. ",
                    "Check that they match the EML: ",
                    paste(keys[present], collapse = "; "), "."))
    df <- df[!present, , drop = FALSE]
  }
  return(df)
}


#' Get an EML element by its path, or NULL if it is not there
#'
#' @param eml An emld object derived from an EML file
#' @param path A slash-separated element path, e.g. "dataset/coverage"
#' @return The element at that path, or NULL
eml_at <- function(eml, path) {
  node <- eml
  for (name in strsplit(path, "/")[[1]]) {
    if (!is.list(node) || is.null(names(node))) return(NULL)
    node <- node[[name]]
    if (is.null(node)) return(NULL)
  }
  return(node)
}


#' Log EML content that this workflow does not carry into metabase
#'
#' Two kinds of note are made: EML elements that metabase can hold but that no
#' `format_*` function reads, and simplifications the `format_*` functions make
#' that need a human to check afterwards.
#'
#' @param lg An ingress log (see `new_ingress_log`)
#' @param eml An emld object derived from an EML file
#' @return The log, invisibly
log_eml_gaps <- function(lg, eml) {

  # Elements metabase can hold, but that no format_* function reads
  not_read <- c(
    "dataset/coverage/taxonomicCoverage" =
      "taxonomic coverage is not read; fill in ListTaxa and DataSetTaxa by hand",
    "dataset/associatedParty" =
      paste("associated parties are not read into DataSetPersonnel, because",
            "format_DataSetPersonnel looks for an 'associatedParties' element;",
            "add them by hand"),
    "dataset/annotation" =
      "semantic annotations are not read; see the semantic_annotation schema",
    "dataset/project" =
      "the project tree comes from mb2eml_r.boilerplate, not from the EML",
    "dataset/intellectualRights" =
      "intellectual rights come from mb2eml_r.boilerplate, not from the EML",
    "dataset/licensed" =
      "the license comes from mb2eml_r.boilerplate, not from the EML",
    "dataset/publisher" =
      "the publisher comes from mb2eml_r.boilerplate, not from the EML",
    "dataset/metadataProvider" =
      "the metadata provider comes from mb2eml_r.boilerplate, not from the EML",
    "dataset/purpose" = "metabase has no column for it",
    "dataset/pubPlace" = "metabase has no column for it",
    "dataset/additionalInfo" = "metabase has no dataset-level column for it")

  for (path in names(not_read)) {
    if (!is.null(eml_at(eml, path))) {
      log_note(lg, "gap", "-",
               paste0("<", path, "> is in the EML but ", not_read[[path]], "."))
    }
  }

  # Simplifications made by the format_* functions
  if (!is.null(eml_at(eml, "dataset/otherEntity"))) {
    log_note(lg, "gap", "DataSetAttributes",
             paste("otherEntity elements are listed in DataSetEntities, but",
                   "their attributeLists are not read."))
  }
  if (!is.null(eml_at(eml, "dataset/dataTable"))) {
    log_note(lg, "decision", "DataSetEntities",
             paste("EntityType was set to 'dataTable' and FileType to 'csv_B'",
                   "for every entity. Correct any entity that is not a unix",
                   "CSV with a one line header."))
  }
  if (!is.null(eml_at(eml, "dataset/methods"))) {
    log_note(lg, "decision", "DataSetMethod",
             paste("Every descendant of <methods> was flattened into one",
                   "markdown file and recorded as a single method step. Edit",
                   "that file before generating EML from metabase."))
  }
  if (!is.null(eml_at(eml, "dataset/coverage/geographicCoverage"))) {
    log_note(lg, "decision", "ListSites",
             paste("SiteIDs were generated as 'unknown1', 'unknown2', and so",
                   "on, and SiteName and SiteLocation are empty. Match these",
                   "to real ListSites entries before ingress."))
  }
  if (!is.null(eml_at(eml, "dataset/coverage/temporalCoverage")) &&
      is.null(eml_at(eml, "dataset/coverage/temporalCoverage/rangeOfDates"))) {
    log_note(lg, "gap", "DataSetTemporal",
             paste("Temporal coverage is not a <rangeOfDates>, probably a",
                   "<singleDateTime>, and was not read."))
  }
  log_note(lg, "gap", "ListPeople",
           paste("Postal address, phone and web page are never read from the",
                 "EML. Fill them in for any newly added person."))
  invisible(lg)
}


# ---------------------------------------------------------------------------
# The workflow
# ---------------------------------------------------------------------------

#' Run one of the format_* functions, logging a failure instead of stopping
#'
#' Letting one table fail without ending the run means a single pass reports
#' every problem with the EML rather than only the first.
#'
#' @param lg An ingress log (see `new_ingress_log`)
#' @param tablename Name (char) of the table being formatted, for the log
#' @param tables A call to a `format_*` function, evaluated here
#' @return The named list of dataframes it returned, or NULL if it failed
try_format <- function(lg, tablename, tables) {
  tryCatch(tables, error = function(e) {
    log_note(lg, "problem", tablename,
             paste0("Could not be formatted from the EML, so nothing will be ",
                    "inserted into it or its related tables. The error was: ",
                    conditionMessage(e)))
    NULL
  })
}


#' Insert formatted tables into metabase in a single transaction
#'
#' Either every table goes in or none does, so a failure part way through
#' cannot leave a half-described dataset behind.
#'
#' @param conn Connection to an LTER Metabase instance
#' @param lg An ingress log (see `new_ingress_log`)
#' @param schema Name (char) of the LTER Metabase schema to target
#' @param tables A named list of dataframes, in the order they are inserted
#' @return TRUE if the transaction was committed, FALSE if it was rolled back
ingress_tables <- function(conn, lg, schema, tables) {
  message("Writing ", length(tables), " table(s) to metabase...")
  DBI::dbBegin(conn)
  tryCatch({
    for (tablename in names(tables)) {
      ingress_table(conn, tablename, tables[[tablename]], schema = schema)
      log_note(lg, "info", tablename,
               paste("Inserted", nrow(tables[[tablename]]), "row(s)."))
    }
    DBI::dbCommit(conn)
    log_note(lg, "info", "-", "Ingress committed.")
    TRUE
  }, error = function(e) {
    DBI::dbRollback(conn)
    log_note(lg, "problem", "-",
             paste0("Ingress failed and was rolled back, so metabase is ",
                    "unchanged. The error was: ", conditionMessage(e)))
    FALSE
  })
}


#' Import an EML document into an LTER Metabase
#'
#' This is the "meta-ingress" workflow. It reads an EML document, formats its
#' contents into LTER Metabase tables with the `format_*` functions, checks
#' those tables against the metabase schema, and - unless this is a dry run -
#' inserts them in one transaction.
#'
#' These files are written to `outpath` on every run:
#'
#' * `abstract.<dsid>.ingress.md` and `methods.<dsid>.ingress.md`, the text
#'   that the new `DataSet` and `DataSetMethod` rows refer to.
#' * `metabase.<table>.<dsid>.csv`, one per formatted table. Empty cells are
#'   NULLs.
#' * `ingress.<dsid>.log.md`, the log.
#'
#' The log records everything the workflow decided on your behalf and
#' everything in the EML it could not transfer, grouped into `problem`
#' (metabase will reject this, fix it first), `gap` (EML content that did not
#' make it into metabase), `decision` (a default or a dropped duplicate row),
#' and `info` (progress). Read it before running with `dry_run=FALSE`.
#'
#' Rows are only ever added, never changed. The shared `List*` vocabularies
#' (people, keywords, sites and codes) are treated as authoritative: a row
#' whose primary key metabase already holds is skipped and logged rather than
#' overwritten. Ingress is refused outright if the dataset id is already in
#' the `DataSet` table, because revising a dataset already in metabase is a
#' manual job - see the LTER-core-metabase documentation on updating datasets.
#'
#' @param eml Path to an EML file, or an emld object derived from one
#' @param dsid The dataset id value, which is the primary key for
#' lter_metabase. Read from the EML packageId if not given.
#' @param revision The dataset revision number. Read from the EML packageId
#' if not given.
#' @param outpath The path to write the abstract, methods, table CSVs and log
#' to. Defaults to the current working directory.
#' @param dry_run Boolean value - if TRUE (the default) format, check and
#' write files but leave the database alone. Pass FALSE to insert the rows.
#' @param boilerplate_id The identifier of a "boilerplate" record in metabase,
#' which will set the project tree and other elements.
#' @param schema Name (char) of the LTER Metabase schema to target
#' ("lter_metabase" by default)
#' @param conn Connection to an LTER Metabase instance. One is opened with
#' `metabase_connect()`, and closed again, if this is not given.
#' @return Invisibly, a named list with `tables` (the formatted dataframes, in
#' the order they are inserted) and `log` (a dataframe of the log entries)
#' @export
eml_to_metabase <- function(eml,
                            dsid = NULL,
                            revision = NULL,
                            outpath = getwd(),
                            dry_run = TRUE,
                            boilerplate_id = "jrn-default",
                            schema = "lter_metabase",
                            conn = NULL) {

  if (!dir.exists(outpath)) {
    stop("outpath does not exist: ", outpath, call. = FALSE)
  }
  if (is.character(eml)) {
    message("Reading EML from ", eml, " ...")
    eml <- EML::read_eml(eml)
  }

  # The packageId (e.g. "knb-lter-jrn.210.5") gives the dataset id and the
  # revision number unless they were passed in
  packageid <- as.character(eml$packageId)
  pid <- if (length(packageid) == 1) {
    strsplit(packageid, ".", fixed = TRUE)[[1]]
  } else {
    character(0)
  }
  if (is.null(dsid)) {
    dsid <- if (length(pid) > 1) pid[2] else NA
  }
  if (is.null(revision)) {
    revision <- if (length(pid) > 2) pid[3] else 0
  }
  dsid <- suppressWarnings(as.integer(dsid))
  revision <- suppressWarnings(as.integer(revision))
  if (is.na(dsid)) {
    stop("Could not read a dataset id from packageId '",
         paste(packageid, collapse = " "),
         "'. Pass one with the dsid argument.", call. = FALSE)
  }

  lg <- new_ingress_log(dsid)
  log_note(lg, "info", "-",
           paste0("EML packageId '", packageid, "' read as dataset ",
                  dsid, ", revision ", revision, "."))

  if (is.null(conn)) {
    conn <- metabase_connect()
    on.exit(DBI::dbDisconnect(conn), add = TRUE)
  }

  # Refuse to ingress on top of a dataset metabase already describes
  existing <- DBI::dbGetQuery(conn,
    paste("SELECT \"DataSetID\" FROM", mb_id(conn, schema, "DataSet"),
          "WHERE \"DataSetID\" = $1"),
    params = list(dsid))
  if (nrow(existing) > 0) {
    stop("Dataset ", dsid, " is already in ", schema, ".\"DataSet\". ",
         "eml_to_metabase only creates new datasets, so revise this one by ",
         "hand, or remove it first.", call. = FALSE)
  }

  # Format the metabase tables. Each format_* function returns a named list
  # of dataframes, and c() collects them into one list.
  message("Formatting metabase tables...")
  tables <- c(
    try_format(lg, "DataSet",
               format_DataSet(dsid, eml, revnum = revision, outpath = outpath,
                              boilerplate_id = boilerplate_id)),
    try_format(lg, "DataSetMethod",
               format_DataSetMethod(dsid, eml, outpath = outpath)),
    try_format(lg, "DataSetEntities", format_DataSetEntities(dsid, eml)),
    try_format(lg, "DataSetAttributes", format_DataSetAttributes(dsid, eml)),
    try_format(lg, "DataSetKeywords", format_DataSetKeywords(dsid, eml)),
    try_format(lg, "DataSetPersonnel", format_DataSetPersonnel(dsid, eml)),
    try_format(lg, "DataSetTemporal", format_DataSetTemporal(dsid, eml)),
    try_format(lg, "DataSetSites", format_DataSetSites(dsid, eml)))

  # Put the tables in foreign key order. Anything the format_* functions
  # returned that this workflow does not know how to place is dropped, so say
  # so rather than losing it quietly.
  unplaced <- setdiff(names(tables), mb_ingress_order())
  if (length(unplaced) > 0) {
    log_note(lg, "problem", "-",
             paste0("Formatted table(s) that mb_ingress_order() does not list, ",
                    "so they were not ingressed: ",
                    paste(unplaced, collapse = ", "), "."))
  }
  tables <- tables[intersect(mb_ingress_order(), names(tables))]
  log_note(lg, "info", "-",
           paste("Formatted", length(tables), "table(s) from the EML."))

  # A person with no <userId> in the EML gets a ListPeopleID row holding no
  # identifier at all, which is worse than having no row
  people_id <- tables[["ListPeopleID"]]
  if (!is.null(people_id)) {
    blank <- is.na(people_id$IdentificationURL)
    if (any(blank)) {
      log_note(lg, "decision", "ListPeopleID",
               paste0("No <userId> in the EML for ",
                      paste(people_id$NameID[blank], collapse = ", "),
                      ", so no identifier row was made for them. Add their ",
                      "ORCIDs by hand."))
      tables[["ListPeopleID"]] <- people_id[!blank, , drop = FALSE]
    }
  }

  log_eml_gaps(lg, eml)

  message("Checking the formatted tables against metabase...")
  for (tablename in names(tables)) {
    tables[[tablename]] <- drop_existing_rows(conn, lg, schema, tablename,
                                              tables[[tablename]])
    check_table(conn, lg, schema, tablename, tables[[tablename]])
    log_note(lg, "info", tablename,
             paste(nrow(tables[[tablename]]), "row(s) ready to insert."))
  }

  # Write every table out so that it can be reviewed, or loaded by hand
  for (tablename in names(tables)) {
    utils::write.csv(tables[[tablename]],
                     file.path(outpath, paste0("metabase.", tablename, ".",
                                               dsid, ".csv")),
                     row.names = FALSE, na = "")
  }
  message("Wrote ", length(tables), " table CSV(s) to ", outpath)

  ingressed <- TRUE
  if (dry_run) {
    log_note(lg, "info", "-", "Dry run: nothing was written to metabase.")
  } else {
    ingressed <- ingress_tables(conn, lg, schema, tables)
  }

  logfile <- file.path(outpath, paste0("ingress.", dsid, ".log.md"))
  write_ingress_log(lg, logfile)

  if (dry_run) {
    message("This was a dry run. Read ", logfile, ", fix anything listed ",
            "under 'problem', then call again with `dry_run=FALSE`.\n")
  } else if (!ingressed) {
    stop("Ingress failed and was rolled back. See ", logfile, call. = FALSE)
  } else {
    message("Done.\n")
  }

  invisible(list("tables" = tables, "log" = do.call(rbind, lg$entries)))
}
