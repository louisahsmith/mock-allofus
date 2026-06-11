# Default location for the mock database

By default the mock database lives in the current **project directory**,
as `mock_aou.duckdb`, so each project gets its own database rather than
sharing a single global file. The project directory is found by looking
upward from the working directory for an RStudio project (`.Rproj`), a
git repository, a `.here` file, or an R package `DESCRIPTION`; if none
is found, the working directory is used.

## Usage

``` r
default_mock_db_path()
```

## Value

A path to the `.duckdb` file.

## Details

Override the location by setting `options(mockallofus.path = "...")` (a
full path to the `.duckdb` file), or pass `path` to
[`mock_aou_connect()`](https://louisahsmith.github.io/mockallofus/reference/mock_aou_connect.md)
/
[`build_mock_db()`](https://louisahsmith.github.io/mockallofus/reference/build_mock_db.md)
directly.

The database file is generated data — add `mock_aou.duckdb` to your
`.gitignore`.
