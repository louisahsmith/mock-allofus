test_that("mock_aou_connect returns a DuckDB connection and sets allofus options", {
  con <- local_mock_con()
  expect_s4_class(con, "duckdb_connection")
  expect_identical(getOption("aou.default.con"), con)
  expect_identical(getOption("aou.default.cdr"), "main")
})

test_that("{CDR} schema resolves for allofus SQL helpers", {
  con <- local_mock_con()
  cdr <- getOption("aou.default.cdr")
  n <- DBI::dbGetQuery(con, sprintf("SELECT count(*) AS n FROM %s.person", cdr))$n
  expect_equal(as.numeric(n), 300)
})

test_that("BigQuery-compatibility macros execute on DuckDB", {
  con <- local_mock_con()
  expect_true(DBI::dbGetQuery(con, "SELECT CONTAINS_SUBSTR('race_White', 'race') AS x")$x)
  expect_true(DBI::dbGetQuery(con, "SELECT REGEXP_CONTAINS('race_White', 'race') AS x")$x)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNTIF(person_id > 0) AS n FROM person")$n |> as.numeric(), 300)
})
