test_that("an agent registers the commons tools", {
  agent <- test_agent()

  expect_equal(
    unname(vapply(agent$get_tools(), tool_name, character(1))),
    c("search_measures", "call_measure", "search_context", "describe_table", "run_sql")
  )
})

test_that("tools build without bsicons installed", {
  state <- new_agent_state(list(test_source()), semantic_layer())
  local_mocked_bindings(is_installed = function(...) FALSE, .package = "rlang")

  expect_length(build_commons_tools(state), 5)
})

test_that("the `source` argument only appears with several sources", {
  one <- test_agent()
  several <- commons(
    test_client(),
    data_sources = list(a = test_source(), b = test_source())
  )

  expect_named(tool_properties(agent_tool(one, "run_sql")), "sql")
  expect_named(tool_properties(agent_tool(several, "run_sql")), c("sql", "source"))
  expect_equal(
    type_values(tool_properties(agent_tool(several, "run_sql"))$source),
    c("a", "b")
  )
})

test_that("call_measure runs a measure and injects the connection", {
  src <- test_source()
  layer <- semantic_layer(
    measure(
      "revenue_by_region",
      "Total revenue for a sales region.",
      function(region, warehouse) {
        DBI::dbGetQuery(
          warehouse,
          "SELECT sum(revenue) AS revenue FROM sales WHERE region = ?",
          params = list(region)
        )
      },
      arguments = list(region = ellmer::type_string("Sales region."))
    )
  )
  state <- new_agent_state(list(warehouse = src), layer)

  res <- call_measure_tool(
    state$registry,
    "revenue_by_region",
    '{"region": "EMEA"}',
    state$injections
  )

  expect_s3_class(res, "ellmer::ContentToolResult")
  expect_match(res@value, "2450")
  expect_equal(res@extra$display$title, "Measure: revenue by region")
})

test_that("call_measure shows the model's arguments alongside the result", {
  state <- new_agent_state(
    list(test_source()),
    semantic_layer(
      measure(
        "revenue_for",
        "Revenue for a region.",
        function(region) 100,
        arguments = list(region = ellmer::type_string("Region."))
      )
    )
  )

  res <- call_measure_tool(state$registry, "revenue_for", '{"region": "EMEA"}')

  expect_match(res@extra$display$markdown, "- region: EMEA", fixed = TRUE)
})

test_that("call_measure errors on an unknown measure", {
  registry <- semantic_layer(test_measure())$measures

  expect_snapshot(error = TRUE, call_measure_tool(registry, "nope", "{}"))
  expect_snapshot(error = TRUE, call_measure_tool(list(), "nope", "{}"))
})

test_that("parse_json_args() accepts the empty forms", {
  expect_equal(parse_json_args("{}"), list())
  expect_equal(parse_json_args(""), list())
  expect_equal(parse_json_args(NULL), list())
  expect_equal(parse_json_args('{"a": 1}'), list(a = 1))
  expect_equal(parse_json_args(list(a = 1)), list(a = 1))
})

test_that("format_measure_value() renders frames, scalars, and other objects", {
  expect_match(format_measure_value(data.frame(x = 1)), "|  1|", fixed = TRUE)
  expect_equal(format_measure_value(42), "42")
  expect_equal(format_measure_value(c("a", "b")), "a, b")
  expect_match(format_measure_value(as.list(1:30)), "[[1]]", fixed = TRUE)
})

test_that("search_context searches the dictionary's prose", {
  state <- new_agent_state(
    list(test_source(dictionary = test_dictionary_path())),
    semantic_layer()
  )

  res <- search_context_tool(state$context, "what is net revenue")

  expect_match(res@value, "Revenue after refunds and credits.")
})

test_that("search_context reports misses and missing documentation", {
  state <- new_agent_state(
    list(test_source(dictionary = test_dictionary_path())),
    semantic_layer()
  )

  expect_snapshot(cat(search_context_tool(state$context, "headcount")@value))
  expect_snapshot(cat(search_context_tool(character(), "net revenue")))
})

test_that("describe_table returns the live schema and sample rows", {
  res <- describe_table_tool(test_source(), "sales")

  expect_match(res@value, "Columns of `sales`", fixed = TRUE)
  expect_match(res@value, "Sample rows", fixed = TRUE)
  expect_equal(res@extra$display$title, "Described sales")
})

test_that("describe_table merges the dictionary with the live schema", {
  src <- test_source(dictionary = test_dictionary_path())

  expect_snapshot(cat(describe_table_tool(src, "reps")@value))
})

test_that("describe_table labels the source when there are several", {
  res <- describe_table_tool(test_source(), "sales", source_name = "warehouse")

  expect_equal(res@extra$display$title, "Described sales (warehouse)")
})

test_that("run_sql returns results and echoes the query for display", {
  res <- run_sql_tool(test_source(), "SELECT sum(revenue) AS total FROM sales")

  expect_match(res@value, "5650")
  expect_match(res@extra$display$markdown, "```sql", fixed = TRUE)
})

test_that("run_sql refuses to write", {
  expect_snapshot(error = TRUE, run_sql_tool(test_source(), "DELETE FROM sales"))
})

test_that("run_sql delivers a dictionary entry the first time it touches a table", {
  src <- test_source(dictionary = test_dictionary_path())
  tracker <- new.env(parent = emptyenv())
  sql <- "SELECT count(*) AS n FROM sales"

  first <- run_sql_tool(src, sql, tracker = tracker)
  second <- run_sql_tool(src, sql, tracker = tracker)

  expect_match(first@value, "Dictionary entry for `sales`", fixed = TRUE)
  expect_no_match(second@value, "Dictionary entry", fixed = TRUE)
})

test_that("describe_table marks a table as touched for run_sql", {
  src <- test_source(dictionary = test_dictionary_path())
  tracker <- new.env(parent = emptyenv())

  describe_table_tool(src, "sales", tracker = tracker)
  res <- run_sql_tool(src, "SELECT count(*) AS n FROM sales", tracker = tracker)

  expect_no_match(res@value, "Dictionary entry", fixed = TRUE)
})

test_that("first touch is tracked per source", {
  src <- test_source(dictionary = test_dictionary_path())
  tracker <- new.env(parent = emptyenv())
  sql <- "SELECT count(*) AS n FROM sales"

  a <- run_sql_tool(src, sql, source_name = "a", tracker = tracker)
  b <- run_sql_tool(src, sql, source_name = "b", tracker = tracker)

  expect_match(a@value, "Dictionary entry", fixed = TRUE)
  expect_match(b@value, "Dictionary entry", fixed = TRUE)
})

test_that("a query that names no documented table delivers no entry", {
  src <- test_source(dictionary = test_dictionary_path())

  res <- run_sql_tool(src, "SELECT 1 AS n", tracker = new.env(parent = emptyenv()))

  expect_no_match(res@value, "Dictionary entry", fixed = TRUE)
})

test_that("collect_lazy_table() collects lazy tables and passes others through", {
  skip_if_not_installed("dbplyr")
  skip_if_not_installed("dplyr")

  lazy <- dplyr::tbl(test_con(), "sales")

  expect_s3_class(collect_lazy_table(lazy), "tbl_df")
  expect_equal(collect_lazy_table(1:3), 1:3)
})
