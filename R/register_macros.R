# DuckDB macros that emulate the BigQuery scalar/aggregate functions the
# `allofus` package (and common All of Us SQL snippets) emit verbatim. Most
# BigQuery functions allofus uses are already native in DuckDB (IFNULL,
# REGEXP_EXTRACT, STRING_AGG with ORDER BY, DATE_ADD with INTERVAL, CAST AS
# STRING/INT64); only the few below need a shim.

#' Register BigQuery-compatibility macros on a DuckDB connection
#'
#' Defines DuckDB macros so SQL written in the BigQuery dialect (as emitted by
#' some `allofus` functions, e.g. the `clean_answers` path of `aou_survey()`)
#' executes locally. Idempotent (`CREATE OR REPLACE`).
#'
#' @param con A DuckDB connection (read-write).
#' @return `con`, invisibly.
#' @export
register_bq_macros <- function(con) {
  macros <- c(
    # BigQuery CONTAINS_SUBSTR(value, substr) -> case-insensitive in BigQuery;
    # DuckDB contains() is case-sensitive, which matches the allofus usage where
    # the searched-for substrings ("cope_", "SDOH_", "_") are exact.
    "CREATE OR REPLACE MACRO CONTAINS_SUBSTR(s, sub) AS contains(s, sub)",
    # BigQuery REGEXP_CONTAINS(value, regexp)
    "CREATE OR REPLACE MACRO REGEXP_CONTAINS(s, p) AS regexp_matches(s, p)",
    # BigQuery COUNTIF(cond) aggregate
    "CREATE OR REPLACE MACRO COUNTIF(cond) AS sum(CASE WHEN cond THEN 1 ELSE 0 END)"
  )
  for (m in macros) DBI::dbExecute(con, m)
  invisible(con)
}
