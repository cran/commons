test_that("data_dictionary() reads a data-dict.yaml file", {
  d <- data_dictionary(test_dictionary_path())

  expect_s3_class(d, "commons_data_dictionary")
  expect_equal(d$name, "Sales")
  expect_equal(d$description, "One row per closed order.")
  expect_named(d$tables, c("sales", "reps"))
  expect_named(d$tables$sales$columns, c("revenue", "region"))
  expect_named(d$glossary, c("net revenue", "book of business"))
})

test_that("data_dictionary() errors informatively on a bad path", {
  expect_snapshot(error = TRUE, data_dictionary("no-such-file.yaml"))
})

test_that("as_data_dictionary() passes through and rejects other input", {
  expect_null(as_data_dictionary(NULL))

  d <- new_data_dictionary(list(name = "x"))
  expect_identical(as_data_dictionary(d), d)

  expect_snapshot(error = TRUE, as_data_dictionary(list(name = "x")))
})

test_that("new_data_dictionary() tolerates an empty document", {
  d <- new_data_dictionary(NULL)

  expect_null(d$description)
  expect_equal(d$tables, list())
  expect_equal(d$glossary, list())
})

test_that("key_by_name() keys sequences and accepts pre-keyed maps", {
  entries <- list(list(name = "a", description = "A"), list(name = "b"))
  keyed <- key_by_name(entries, "table")

  expect_named(keyed, c("a", "b"))
  expect_equal(keyed$a$description, "A")
  expect_null(keyed$a$name)

  premapped <- list(a = list(description = "A"))
  expect_identical(key_by_name(premapped, "table"), premapped)
})

test_that("key_by_name() requires a name on each entry", {
  expect_snapshot(
    error = TRUE,
    key_by_name(list(list(description = "A")), "table")
  )
})

test_that("dictionary_entry_text() renders prose, columns, and joins", {
  d <- data_dictionary(test_dictionary_path())

  expect_snapshot(cat(dictionary_entry_text(d, "sales")))
})

test_that("dictionary_entry_text() returns NULL for undocumented tables", {
  d <- data_dictionary(test_dictionary_path())

  expect_null(dictionary_entry_text(d, "not_a_table"))
})

test_that("dictionary_columns_text() merges documented columns with a live schema", {
  d <- data_dictionary(test_dictionary_path())
  live <- data.frame(
    column = c("revenue", "extra"),
    type = c("numeric", "character")
  )

  out <- dictionary_columns_text(d$tables$sales$columns, live = live)

  expect_match(out, "- revenue (number, USD)", fixed = TRUE)
  expect_match(out, "- extra (character)", fixed = TRUE)
  expect_match(out, "not present in the table: region", fixed = TRUE)
})

test_that("dictionary_column_line() renders values, ranges, and examples", {
  expect_equal(
    dictionary_column_line("status", list(values = list("open", "closed"))),
    "- status: Values: open, closed."
  )
  expect_equal(
    dictionary_column_line("sex", list(values = list(M = "Male"))),
    "- sex: Values: M (Male)."
  )
  expect_equal(
    dictionary_column_line("age", list(range = list(0, 120))),
    "- age: Range: 0 to 120."
  )
  expect_equal(
    dictionary_column_line("id", list(examples = list("a1", "a2"))),
    "- id: Examples: a1, a2."
  )
  expect_equal(dictionary_column_line("bare", NULL), "- bare")
})

test_that("relationships are matched by whole table name", {
  d <- data_dictionary(test_dictionary_path())

  expect_match(dictionary_relationships_text(d, "sales"), "many-to-one")
  expect_null(dictionary_relationships_text(d, "ales"))
})

test_that("glossary terms past the ambient cap resolve at first touch", {
  d <- new_data_dictionary(list(
    tables = list(list(name = "t", description = "Uses churn.")),
    glossary = list(
      padding = strrep("x", 3980),
      churn = "Customers who left."
    )
  ))

  expect_equal(glossary_ambient(d), "padding")
  expect_match(dictionary_entry_text(d, "t"), "- churn: Customers who left.")
})

test_that("glossary_ambient() handles an empty glossary", {
  expect_equal(glossary_ambient(new_data_dictionary(NULL)), character(0))
})

test_that("dictionary_context_chunks() splits prose at YAML boundaries", {
  d <- data_dictionary(test_dictionary_path())
  chunks <- dictionary_context_chunks(d)

  expect_equal(
    chunks,
    c(
      "Revenue is always net of refunds.",
      "Table `sales`: Closed orders, one row each.\n\nExcludes orders still in flight.",
      "Table `reps`: One row per sales representative.",
      "net revenue: Revenue after refunds and credits.",
      "book of business: The set of accounts a rep owns."
    )
  )
})

test_that("dictionary_context_chunks() is empty without a dictionary", {
  expect_equal(dictionary_context_chunks(NULL), character(0))
})
