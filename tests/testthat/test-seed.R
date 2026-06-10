test_that("mock_seed_concept_set inserts a novel concept at a partial prevalence", {
  con <- local_mock_con(n_persons = 1000L)
  novel <- 99999001L
  expect_equal(
    as.numeric(DBI::dbGetQuery(con, sprintf(
      "SELECT count(*) FROM condition_occurrence WHERE condition_concept_id=%d", novel))[[1]]),
    0
  )
  mock_seed_concept_set(con, concepts = novel, domain = "condition",
                        prevalence = 0.3, seed = 1, quiet = TRUE)
  n_with <- as.numeric(DBI::dbGetQuery(con, sprintf(
    "SELECT count(DISTINCT person_id) FROM condition_occurrence WHERE condition_concept_id=%d", novel))[[1]])
  # partial cohort: neither nobody nor everybody
  expect_gt(n_with, 0)
  expect_lt(n_with, 1000)
  expect_lt(abs(n_with - 300), 80) # roughly the requested prevalence
  # the concept was added to the concept table too
  expect_equal(as.numeric(DBI::dbGetQuery(con, sprintf(
    "SELECT count(*) FROM concept WHERE concept_id=%d", novel))[[1]]), 1)
})

test_that("seeded events carry EHR src_id so they are visible to EHR-based functions", {
  con <- local_mock_con(n_persons = 500L)
  mock_seed_concept_set(con, concepts = 77777001L, domain = "measurement",
                        prevalence = 0.5, values = c(5, 10), seed = 3, quiet = TRUE)
  ehr <- as.numeric(DBI::dbGetQuery(con,
    "SELECT count(*) FROM measurement_ext WHERE LOWER(src_id) LIKE 'ehr site%'")[[1]])
  expect_gt(ehr, 0)
})

test_that("seeding is deterministic for a fixed seed", {
  count_seeded <- function() {
    con <- local_mock_con(n_persons = 800L, seed = 42L)
    mock_seed_concept_set(con, concepts = 88888001L, domain = "drug",
                          prevalence = 0.4, seed = 7, quiet = TRUE)
    as.numeric(DBI::dbGetQuery(con,
      "SELECT count(DISTINCT person_id) FROM drug_exposure WHERE drug_concept_id=88888001")[[1]])
  }
  expect_equal(count_seeded(), count_seeded())
})
