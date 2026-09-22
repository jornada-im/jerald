# Change log

## March 2026

Changes for upcoming 0.1.0 release

* New `eml_to_metabase` function - a meta-ingress workflow that imports an EML
  document into an LTER Metabase, checks the formatted tables against the
  schema, and writes a log of its decisions and of anything it could not
  transfer. Defaults to a dry run.
* New `publish_dataset` function.
* Parameter name convention changed from xxx.yyy to xxx_yyy.
* Metabase name parameter not needed in older functions
* New `jerald_cred.R` credentials file format.
