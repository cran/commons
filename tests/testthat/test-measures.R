test_that("measure() builds a tool with a title and schema", {
  m <- measure(
    "revenue",
    "Total revenue.",
    function(region) region,
    arguments = list(region = ellmer::type_string("Region."))
  )

  expect_equal(tool_name(m), "revenue")
  expect_equal(tool_description(m), "Total revenue.")
  expect_equal(tool_title(m), "revenue")
  expect_named(tool_properties(m), "region")
})

test_that("measure() derives a title from the name and takes an override", {
  fn <- function() 1

  expect_equal(tool_title(measure("order_count", "d", fn)), "order count")
  expect_equal(tool_title(measure("order_count", "d", fn, title = "Orders")), "Orders")
})

test_that("measure() validates its scalar arguments", {
  fn <- function() 1

  expect_snapshot(error = TRUE, measure(1, "d", fn))
  expect_snapshot(error = TRUE, measure("m", 1, fn))
  expect_snapshot(error = TRUE, measure("m", "d", fn, title = 1))
})

test_that("undocumented arguments are hidden from the model", {
  m <- measure(
    "revenue",
    "Total revenue.",
    function(region, warehouse) NULL,
    arguments = list(region = ellmer::type_string("Region."))
  )

  expect_named(tool_properties(m), "region")
  expect_equal(measure_injection_names(m), "warehouse")
})

test_that("semantic_layer() collects measures and splices lists", {
  layer <- semantic_layer(
    test_measure("a"),
    list(test_measure("b"), test_measure("c"))
  )

  expect_s3_class(layer, "commons_semantic_layer")
  expect_named(layer$measures, c("a", "b", "c"))
})

test_that("semantic_layer() with no measures is empty", {
  expect_length(semantic_layer()$measures, 0)
})

test_that("semantic_layer() rejects duplicates and non-measures", {
  expect_snapshot(error = TRUE, semantic_layer(test_measure(), test_measure()))
  expect_snapshot(error = TRUE, semantic_layer(function() 1))
})

test_that("semantic_layer() reads measures from R scripts", {
  path <- withr::local_tempfile(fileext = ".R")
  writeLines(
    c(
      "#' Revenue by region",
      "#' @param region `string` Sales region.",
      "#' @measure",
      "revenue <- function(region, warehouse) NULL"
    ),
    path
  )

  layer <- semantic_layer(path)
  revenue <- layer$measures$revenue

  expect_named(layer$measures, "revenue")
  expect_named(tool_properties(revenue), "region")
  expect_equal(measure_injection_names(revenue), "warehouse")
})

test_that("resolve_injections() matches undocumented arguments to sources", {
  registry <- semantic_layer(
    measure(
      "revenue",
      "Total revenue.",
      function(region, warehouse) NULL,
      arguments = list(region = ellmer::type_string("Region."))
    )
  )$measures

  injections <- resolve_injections(registry, list(warehouse = "CON"))

  expect_equal(injections$revenue, list(warehouse = "CON"))
})

test_that("resolve_injections() leaves defaulted arguments alone", {
  registry <- semantic_layer(
    measure("m", "d", function(board = "default") board)
  )$measures

  expect_equal(resolve_injections(registry, list()), list(m = list()))
})

test_that("resolve_injections() errors on an unmatched argument with no default", {
  registry <- semantic_layer(
    measure("revenue", "d", function(warehouse) NULL)
  )$measures

  expect_snapshot(error = TRUE, resolve_injections(registry, list()))
  expect_snapshot(error = TRUE, resolve_injections(registry, list(finance = 1)))
})

test_that("validate_measure_args() coerces to the declared types", {
  m <- measure(
    "m",
    "d",
    function(count, ratio, flag, region) NULL,
    arguments = list(
      count = ellmer::type_integer("n"),
      ratio = ellmer::type_number("r"),
      flag = ellmer::type_boolean("f"),
      region = ellmer::type_string("s")
    )
  )

  args <- validate_measure_args(
    m,
    list(count = "3", ratio = "1.5", flag = "TRUE", region = "EMEA")
  )

  expect_identical(args, list(count = 3L, ratio = 1.5, flag = TRUE, region = "EMEA"))
})

test_that("validate_measure_args() enforces enums and arrays", {
  m <- measure(
    "m",
    "d",
    function(region, regions) NULL,
    arguments = list(
      region = ellmer::type_enum(values = c("EMEA", "APAC"), description = "r"),
      regions = ellmer::type_array(
        items = ellmer::type_enum(values = c("EMEA", "APAC")),
        description = "rs"
      )
    )
  )

  expect_equal(
    validate_measure_args(m, list(region = "EMEA", regions = c("EMEA", "APAC"))),
    list(region = "EMEA", regions = c("EMEA", "APAC"))
  )
  expect_snapshot(
    error = TRUE,
    validate_measure_args(m, list(region = "LATAM", regions = "EMEA"))
  )
})

test_that("validate_measure_args() reports missing and unknown arguments", {
  m <- measure(
    "m",
    "d",
    function(region) NULL,
    arguments = list(region = ellmer::type_string("r"))
  )

  expect_snapshot(error = TRUE, validate_measure_args(m, list()))
  expect_snapshot(error = TRUE, validate_measure_args(m, list(region = "EMEA", rep = "Ada")))
})

test_that("optional arguments may be omitted", {
  m <- measure(
    "m",
    "d",
    function(region = "EMEA") NULL,
    arguments = list(region = ellmer::type_string("r", required = FALSE))
  )

  expect_equal(validate_measure_args(m, list()), list())
})

test_that("search_measures_text() renders matching schemas", {
  registry <- semantic_layer(
    measure(
      "revenue_by_region",
      "Total revenue for a sales region.",
      function(region, warehouse) NULL,
      arguments = list(region = ellmer::type_string("Sales region."))
    ),
    test_measure("headcount")
  )$measures

  expect_snapshot(cat(search_measures_text(registry, "revenue by region")))
})

test_that("search_measures_text() names the sources a measure uses", {
  registry <- semantic_layer(
    measure(
      "revenue",
      "Total revenue.",
      function(warehouse) NULL
    )
  )$measures

  expect_match(
    search_measures_text(registry, "revenue", source_names = "warehouse"),
    "sources: warehouse"
  )
})

test_that("search_measures_text() handles empty registries and misses", {
  expect_equal(search_measures_text(list(), "revenue"), "No measures are registered.")
  expect_snapshot(
    cat(search_measures_text(semantic_layer(test_measure())$measures, "headcount"))
  )
})
