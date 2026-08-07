test_sales <- function() {
  data.frame(
    order_id = sprintf("o%02d", 1:6),
    revenue = c(500, 900, 1200, 300, 2000, 750),
    region = c("EMEA", "Americas", "EMEA", "APAC", "Americas", "EMEA"),
    product_line = c(
      "Platform",
      "Services",
      "Platform",
      "Training",
      "Platform",
      "Services"
    ),
    rep = c("Ada", "Ada", "Bo", "Cy", "Bo", "Ada"),
    stringsAsFactors = FALSE
  )
}

test_reps <- function() {
  data.frame(
    rep = c("Ada", "Bo", "Cy"),
    region = c("EMEA", "Americas", "APAC"),
    stringsAsFactors = FALSE
  )
}

# A DuckDB connection holding `sales` and `reps`, disconnected when the
# calling test finishes.
test_con <- function(env = parent.frame()) {
  testthat::skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb())
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE), envir = env)
  DBI::dbWriteTable(con, "sales", test_sales())
  DBI::dbWriteTable(con, "reps", test_reps())
  con
}

test_source <- function(..., env = parent.frame()) {
  data_source(test_con(env = env), ...)
}

# Constructing a Chat makes no request, so tests need no credentials.
test_client <- function() {
  suppressMessages(ellmer::chat_anthropic())
}

test_agent <- function(..., env = parent.frame()) {
  commons(test_client(), data_sources = test_source(env = env), ...)
}

test_measure <- function(name = "order_count") {
  measure(
    name,
    "Count of orders placed.",
    function() 6,
    arguments = list()
  )
}

scrub_path <- function(x) {
  gsub('"[^"]*\\.md"', '"<tempfile>"', x)
}

agent_tool <- function(agent, name) {
  tools <- agent$get_tools()
  tools[[which(vapply(tools, tool_name, character(1)) == name)]]
}

# A data-dict.yaml file covering the tables in test_con(), written to a
# temporary path that lives as long as the calling test.
test_dictionary_path <- function(env = parent.frame()) {
  testthat::skip_if_not_installed("yaml")
  path <- withr::local_tempfile(fileext = ".yaml", .local_envir = env)
  writeLines(test_dictionary_yaml(), path)
  path
}

test_dictionary_yaml <- function() {
  c(
    "name: Sales",
    "description: One row per closed order.",
    "details: Revenue is always net of refunds.",
    "tables:",
    "  - name: sales",
    "    description: Closed orders, one row each.",
    "    details: Excludes orders still in flight.",
    "    columns:",
    "      - name: revenue",
    "        type: number",
    "        units: USD",
    "        description: Net revenue for the order.",
    "      - name: region",
    "        description: Sales region.",
    "        values: [EMEA, Americas, APAC]",
    "  - name: reps",
    "    description: One row per sales representative.",
    "relationships:",
    "  - join: sales.rep = reps.rep",
    "    cardinality: many-to-one",
    "    description: Each order is credited to one rep.",
    "glossary:",
    "  net revenue: Revenue after refunds and credits.",
    "  book of business: The set of accounts a rep owns."
  )
}
