#' Connect to the local mock All of Us database
#'
#' Opens (building first if necessary) the local mock DuckDB database and wires
#' it into the `allofus` package the same way [allofus::aou_connect()] does on
#' the Researcher Workbench: the connection is stored in
#' `getOption("aou.default.con")` and a curated-data-repository name in
#' `getOption("aou.default.cdr")`. After calling this, `allofus` analysis code
#' that relies on those defaults runs against the mock database unchanged.
#'
#' BigQuery-compatibility macros are registered on the connection (see
#' [register_bq_macros()]) so SQL emitted by `allofus` in the BigQuery dialect
#' executes on DuckDB.
#'
#' @param path Path to the DuckDB database file. Defaults to the cached location
#'   ([default_mock_db_path()]); built with [build_mock_db()] if absent.
#' @param cdr The schema name used for `{CDR}` interpolation in `allofus` SQL
#'   helpers. Defaults to `"main"` (DuckDB's default schema, where the mock
#'   tables live), so references like `` `{CDR}.person` `` resolve.
#' @param quiet Suppress the connection message.
#' @param ... Further arguments passed to [DBI::dbConnect()].
#'
#' @return A DuckDB connection object (`duckdb_connection`), also stored in
#'   `getOption("aou.default.con")`.
#' @export
#'
#' @examples
#' \dontrun{
#' con <- mock_aou_connect()
#' # the same code you would run on the Workbench:
#' dplyr::tbl(con, "person")
#' }
mock_aou_connect <- function(path = default_mock_db_path(),
                             cdr = "main",
                             quiet = FALSE,
                             ...) {
  if (!file.exists(path)) {
    if (!quiet) cli::cli_inform(c("i" = "No mock database found; building one (first run only)."))
    build_mock_db(path = path, quiet = quiet)
  }

  con <- DBI::dbConnect(
    duckdb::duckdb(),
    dbdir = path,
    bigint = "integer64", # mirror allofus::aou_connect()'s int64 handling
    ...
  )

  register_bq_macros(con)

  options(
    aou.default.con = con,
    aou.default.cdr = cdr
  )

  if (!quiet) {
    cli::cli_inform(c(
      "v" = "Connected to the mock All of Us database!",
      "i" = "This is synthetic data for local development; it does not reflect real All of Us data."
    ))
  }
  invisible(con)
}

#' Disconnect from the mock database and clear the stored connection
#' @param con The connection to close. Defaults to `getOption("aou.default.con")`.
#' @param shutdown Whether to shut down the DuckDB instance. Default `TRUE`.
#' @return `NULL`, invisibly.
#' @export
mock_aou_disconnect <- function(con = getOption("aou.default.con"), shutdown = TRUE) {
  if (!is.null(con) && DBI::dbIsValid(con)) {
    DBI::dbDisconnect(con, shutdown = shutdown)
  }
  options(aou.default.con = NULL)
  invisible(NULL)
}
