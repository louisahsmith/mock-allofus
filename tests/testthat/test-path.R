test_that("default_mock_db_path resolves to the project directory", {
  proj <- withr::local_tempdir()
  file.create(file.path(proj, "myproject.Rproj"))
  dir.create(file.path(proj, "code"))
  withr::with_dir(file.path(proj, "code"), {
    # compare the resolved directory (which exists) to avoid symlink artifacts
    # when normalizing a not-yet-created leaf file
    expect_equal(basename(default_mock_db_path()), "mock_aou.duckdb")
    expect_equal(
      dirname(default_mock_db_path()),
      normalizePath(proj, winslash = "/", mustWork = TRUE)
    )
  })
})

test_that("mockallofus.path option overrides the default", {
  withr::with_options(list(mockallofus.path = file.path(tempdir(), "custom.duckdb")), {
    expect_equal(default_mock_db_path(), file.path(tempdir(), "custom.duckdb"))
  })
})
