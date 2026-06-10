# These exercise real `allofus` package functions against the mock database --
# the core promise that the same analysis code runs locally. They require the
# (backend-aware) allofus package to be installed.

test_that("pure-dbplyr allofus usage works on the mock db", {
  skip_if_not_installed("allofus")
  con <- local_mock_con()
  out <- dplyr::tbl(con, "person") |>
    dplyr::summarise(n = dplyr::n()) |>
    dplyr::collect()
  expect_equal(as.numeric(out$n), 300)
})

test_that("aou_sql runs portable SQL locally", {
  skip_if_not_installed("allofus")
  con <- local_mock_con()
  res <- allofus::aou_sql("SELECT count(*) AS n FROM `{CDR}.person`", collect = TRUE)
  expect_equal(as.numeric(res$n), 300)
})

test_that("aou_concept_set returns indicator and count outputs", {
  skip_if_not_installed("allofus")
  con <- local_mock_con()
  cohort <- dplyr::tbl(con, "person") |> dplyr::select("person_id")
  ind <- allofus::aou_concept_set(cohort, concepts = c(201826, 4193704),
                                  domains = "condition", output = "indicator",
                                  concept_set_name = "t2dm", collect = TRUE)
  expect_true(all(c("person_id", "t2dm") %in% names(ind)))
  expect_true(all(ind$t2dm %in% c(0, 1)))
  expect_gt(sum(ind$t2dm), 0)
})

test_that("aou_compute materializes a temp table", {
  skip_if_not_installed("allofus")
  con <- local_mock_con()
  res <- dplyr::tbl(con, "concept") |>
    dplyr::select("concept_id") |>
    head(5) |>
    allofus::aou_compute() |>
    dplyr::collect()
  expect_equal(nrow(res), 5)
})

test_that("aou_observation_period builds one period per person", {
  skip_if_not_installed("allofus")
  con <- local_mock_con()
  op <- suppressWarnings(allofus::aou_observation_period(collect = TRUE))
  expect_true(all(c("person_id", "observation_period_start_date",
                    "observation_period_end_date") %in% names(op)))
  expect_gt(nrow(op), 0)
})

test_that("aou_survey extracts and pivots survey answers", {
  skip_if_not_installed("allofus")
  con <- local_mock_con()
  sv <- suppressWarnings(allofus::aou_survey(
    questions = c(1585838, 1586135),
    question_output = c("gender", "birthplace"),
    collect = TRUE
  ))
  expect_true("person_id" %in% names(sv))
  expect_true(any(grepl("birthplace", names(sv))))
  expect_gt(nrow(sv), 0)
})

test_that("aou_concept_set sees freshly-seeded concepts", {
  skip_if_not_installed("allofus")
  con <- local_mock_con(n_persons = 600L)
  mock_seed_concept_set(con, concepts = 66666001L, domain = "condition",
                        prevalence = 0.5, seed = 11, quiet = TRUE)
  cohort <- dplyr::tbl(con, "person") |> dplyr::select("person_id")
  res <- allofus::aou_concept_set(cohort, concepts = 66666001L, domains = "condition",
                                  output = "indicator", concept_set_name = "seeded",
                                  collect = TRUE)
  expect_gt(sum(res$seeded), 0)
})
