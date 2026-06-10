# Bundles the schema- and survey-describing datasets from the `allofus` package
# into mockallofus internal data (R/sysdata.rda), so the mock database can be
# built without loading allofus (which pulls in the BigQuery stack).
#
# Run manually when the upstream allofus datasets change:
#   source("data-raw/prepare_data.R")

allofus_data <- "/Users/l.smith/Documents/Projects/All of Us/allofus/data"

e <- new.env()
for (f in list.files(allofus_data, pattern = "\\.rda$", full.names = TRUE)) {
  load(f, envir = e)
}

# schema source: table_name -> comma-separated column list
aou_table_info <- e$aou_table_info
# survey question metadata (concept_code / concept_id / choices / field_type)
aou_codebook <- e$aou_codebook
# SDOH / COPE answer code -> human-readable text
aou_concept_codes <- e$aou_concept_codes
# family health history survey structure
aou_health_history <- e$aou_health_history

usethis::use_data(
  aou_table_info, aou_codebook, aou_concept_codes, aou_health_history,
  internal = TRUE, overwrite = TRUE
)
