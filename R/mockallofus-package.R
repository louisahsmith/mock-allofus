#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang .data
#' @importClassesFrom duckdb duckdb_connection
## usethis namespace: end
NULL

# Quiet R CMD check notes for the bundled internal datasets.
utils::globalVariables(c(
  "aou_table_info", "aou_codebook", "aou_concept_codes", "aou_health_history"
))
