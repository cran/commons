test_that("data_source() registers every table on the connection", {
  src <- test_source()

  expect_s3_class(src, "commons_data_source")
  expect_setequal(list_tables(src), c("sales", "reps"))
})

test_that("data_source() honors `tables`", {
  expect_equal(list_tables(test_source(tables = "sales")), "sales")
  expect_equal(
    list_tables(test_source(tables = DBI::Id(table = "sales"))),
    "sales"
  )
  expect_setequal(
    list_tables(test_source(tables = c("sales", "reps"))),
    c("sales", "reps")
  )
})

test_that("data_source() errors on tables the connection doesn't have", {
  con <- test_con()

  expect_snapshot(error = TRUE, data_source(con, tables = c("sales", "nope")))
})

test_that("data_source() rejects anything but a single connection", {
  con <- test_con()

  expect_snapshot(error = TRUE, data_source())
  expect_snapshot(error = TRUE, data_source(sales = test_sales()))
  expect_snapshot(error = TRUE, data_source(con, con))
})

test_that("data_source() attaches a data dictionary", {
  src <- test_source(dictionary = test_dictionary_path())

  expect_s3_class(src$dictionary, "commons_data_dictionary")
  expect_named(src$dictionary$tables, c("sales", "reps"))
})

test_that("data_source() rejects a dictionary that isn't a path", {
  con <- test_con()

  expect_snapshot(error = TRUE, data_source(con, dictionary = list()))
})

test_that("list_tables() requires a data source", {
  expect_snapshot(error = TRUE, list_tables("sales"))
})

test_that("normalize_table_registry() parses schema-qualified names", {
  registry <- normalize_table_registry(c("public.sales", "sales"))

  expect_equal(registry$labels, c("public.sales", "sales"))
  expect_equal(registry$ids[["public.sales"]], DBI::Id(schema = "public", table = "sales"))
  expect_equal(registry$ids[["sales"]], DBI::Id(table = "sales"))
})

test_that("normalize_table_registry() rejects malformed entries", {
  expect_snapshot(error = TRUE, normalize_table_registry(c("sales", "sales")))
  expect_snapshot(error = TRUE, normalize_table_registry(".sales"))
  expect_snapshot(error = TRUE, normalize_table_registry(1))
  expect_snapshot(error = TRUE, normalize_table_registry(list(NA_character_)))
  expect_snapshot(error = TRUE, normalize_table_registry(DBI::Id(schema = "public")))
})

test_that("source_describe() returns a schema and sample rows", {
  d <- source_describe(test_source(), "sales", n_sample = 2)

  expect_equal(d$schema$column, names(test_sales()))
  expect_equal(nrow(d$sample), 2)
})

test_that("source_describe() errors on an unregistered table", {
  src <- test_source(tables = "sales")

  expect_snapshot(error = TRUE, source_describe(src, "reps"))
})

test_that("source_query() runs SELECT statements", {
  res <- source_query(test_source(), "SELECT sum(revenue) AS total FROM sales")

  expect_equal(res$total, sum(test_sales()$revenue))
})

test_that("check_query() rejects statements that would write", {
  expect_snapshot(error = TRUE, check_query("DROP TABLE sales"))
  expect_snapshot(error = TRUE, check_query("  insert into sales values (1)"))
  expect_snapshot(error = TRUE, check_query("update\n sales set revenue = 0"))
})

test_that("check_query() allows reads, including ones naming a keyword", {
  expect_silent(check_query("SELECT * FROM sales"))
  expect_silent(check_query("WITH x AS (SELECT 1) SELECT * FROM x"))
  expect_silent(check_query("SELECT 'DROP' AS word"))
})

test_that("resolve_sql_source() picks the only source without a name", {
  src <- test_source()

  expect_identical(resolve_sql_source(list(src), NULL), src)
  expect_identical(resolve_sql_source(list(a = src), "ignored"), src)
})

test_that("resolve_sql_source() requires a valid name with several sources", {
  sources <- list(a = test_source(), b = test_source())

  expect_identical(resolve_sql_source(sources, "b"), sources$b)
  expect_snapshot(error = TRUE, resolve_sql_source(sources, NULL))
  expect_snapshot(error = TRUE, resolve_sql_source(sources, "c"))
})

test_that("as_data_sources() wraps a bare source and validates lists", {
  src <- test_source()

  expect_equal(as_data_sources(src), list(src))
  expect_equal(as_data_sources(list(a = src)), list(a = src))

  expect_snapshot(error = TRUE, as_data_sources("sales"))
  expect_snapshot(error = TRUE, as_data_sources(list()))
  expect_snapshot(error = TRUE, as_data_sources(list(src, src)))
  expect_snapshot(error = TRUE, as_data_sources(list(a = src, a = src)))
})
