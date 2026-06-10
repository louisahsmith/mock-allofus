test_that("build_mock_db creates all dictionary tables", {
  con <- local_mock_con()
  tables <- DBI::dbListTables(con)
  expect_true(all(c("person", "concept", "condition_occurrence", "measurement",
                    "observation", "drug_exposure", "observation_period",
                    "cb_search_person", "ds_survey") %in% tables))
})

test_that("core tables are populated", {
  con <- local_mock_con(n_persons = 300L)
  expect_equal(n_rows(con, "person"), 300)
  expect_gt(n_rows(con, "condition_occurrence"), 0)
  expect_gt(n_rows(con, "measurement"), 0)
  expect_gt(n_rows(con, "observation"), 0)
  # _ext rows align 1:1 with their domain table
  expect_equal(n_rows(con, "condition_occurrence"), n_rows(con, "condition_occurrence_ext"))
  expect_equal(n_rows(con, "measurement"), n_rows(con, "measurement_ext"))
})

test_that("vignette concepts are present so documented workflows return data", {
  con <- local_mock_con()
  q <- function(sql) as.numeric(DBI::dbGetQuery(con, sql)[[1]])
  expect_gt(q("SELECT count(*) FROM condition_occurrence WHERE condition_concept_id IN (201826,4193704)"), 0)
  expect_gt(q("SELECT count(*) FROM drug_exposure WHERE drug_concept_id IN (40164929,40164897)"), 0)
  expect_gt(q("SELECT count(*) FROM measurement WHERE measurement_concept_id IN (3004410,3005673)"), 0)
  expect_gt(q("SELECT count(*) FROM observation WHERE observation_source_concept_id IN (1585838,1586135)"), 0)
})

test_that("EHR rows are tagged so aou_observation_period's filter matches", {
  con <- local_mock_con()
  ehr <- as.numeric(DBI::dbGetQuery(con,
    "SELECT count(*) FROM measurement_ext WHERE LOWER(src_id) LIKE 'ehr site%'")[[1]])
  expect_gt(ehr, 0)
})

test_that("build is deterministic for a fixed seed", {
  p1 <- withr::local_tempfile(fileext = ".duckdb")
  p2 <- withr::local_tempfile(fileext = ".duckdb")
  build_mock_db(p1, n_persons = 200L, seed = 5L, quiet = TRUE)
  build_mock_db(p2, n_persons = 200L, seed = 5L, quiet = TRUE)
  c1 <- DBI::dbConnect(duckdb::duckdb(), p1); on.exit(DBI::dbDisconnect(c1, shutdown = TRUE), add = TRUE)
  c2 <- DBI::dbConnect(duckdb::duckdb(), p2); on.exit(DBI::dbDisconnect(c2, shutdown = TRUE), add = TRUE)
  expect_equal(
    DBI::dbGetQuery(c1, "SELECT count(*) n FROM condition_occurrence")$n,
    DBI::dbGetQuery(c2, "SELECT count(*) n FROM condition_occurrence")$n
  )
})
