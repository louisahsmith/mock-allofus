.onLoad <- function(libname, pkgname) {
  # Define the S4 subclass of DuckDB's connection here (rather than at top
  # level) so the duckdb package's "duckdb_connection" S4 class is available
  # when the subclass is created. mock_aou_connect() upgrades connections to
  # this class to attach BigQuery-compatible dbplyr translations (translation.R).
  if (!methods::isClass("mock_aou_connection")) {
    methods::setClass("mock_aou_connection", contains = "duckdb_connection")
  }
  invisible()
}
