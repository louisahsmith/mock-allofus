# Build a small mock database in a temp file and connect to it, cleaning up when
# the calling test (or frame `envir`) finishes. Each call uses its own file so
# tests that write (seeding) don't interfere with one another.
local_mock_con <- function(n_persons = 300L, seed = 20240101L, envir = parent.frame()) {
  path <- withr::local_tempfile(fileext = ".duckdb", .local_envir = envir)
  build_mock_db(path, n_persons = n_persons, seed = seed, quiet = TRUE)
  con <- mock_aou_connect(path, quiet = TRUE)
  withr::defer(mock_aou_disconnect(con), envir = envir)
  con
}

n_rows <- function(con, table) {
  as.numeric(DBI::dbGetQuery(con, sprintf("SELECT count(*) FROM %s", table))[[1]])
}
