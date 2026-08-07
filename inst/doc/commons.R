## -----------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = rlang::is_installed(c("duckdb", "yaml"))
)

## -----------------------------------------------------------------------------
library(commons)

## -----------------------------------------------------------------------------
con <- DBI::dbConnect(duckdb::duckdb())

DBI::dbWriteTable(con, "orders", data.frame(
  order_id = 1:6,
  rep = c("Ada", "Ada", "Bo", "Cy", "Bo", "Ada"),
  region = c("EMEA", "Americas", "EMEA", "APAC", "Americas", "EMEA"),
  revenue = c(500, 900, 1200, 300, 2000, 750),
  refunded = c(0, 100, 0, 0, 0, 50)
))

DBI::dbWriteTable(con, "reps", data.frame(
  rep = c("Ada", "Bo", "Cy"),
  hired = as.Date(c("2021-03-01", "2023-07-15", "2024-01-20"))
))

## -----------------------------------------------------------------------------
data_source(con)$tables

## -----------------------------------------------------------------------------
data_source(con, tables = c("orders", "reps"))$tables

## -----------------------------------------------------------------------------
dictionary <- tempfile(fileext = ".yaml")
writeLines(
  '
name: Sales
description: One row per closed order, plus the reps who closed them.
details: >
  Revenue figures are gross. Net revenue subtracts the refunded column;
  always report net revenue unless asked otherwise.
tables:
  - name: orders
    description: Closed orders, one row each.
    columns:
      - name: revenue
        type: number
        units: USD
        description: Gross revenue for the order.
      - name: refunded
        type: number
        units: USD
        description: Amount refunded against the order.
      - name: region
        description: Sales region.
        values: [EMEA, Americas, APAC]
  - name: reps
    description: One row per sales representative.
relationships:
  - join: orders.rep = reps.rep
    cardinality: many-to-one
    description: Each order is credited to exactly one rep.
glossary:
  net revenue: Gross revenue minus refunds.
',
  dictionary
)

sales <- data_source(con, dictionary = dictionary)

## -----------------------------------------------------------------------------
measure_file <- tempfile(fileext = ".R")
writeLines(
  c(
    "#' Net Revenue by Region",
    "#'",
    "#' @param region `enum[EMEA, Americas, APAC]` Sales region.",
    "#' @measure",
    "net_revenue_by_region <- function(region, warehouse) {",
    "  DBI::dbGetQuery(",
    "    warehouse,",
    "    'SELECT sum(revenue - refunded) AS net_revenue FROM orders WHERE region = ?',",
    "    params = list(region)",
    "  )",
    "}"
  ),
  measure_file
)

layer <- semantic_layer(measure_file)
unlink(measure_file)

## -----------------------------------------------------------------------------
# agent <- commons(
#   ellmer::chat_anthropic(),
#   data_sources = list(warehouse = sales),
#   semantic_layer = layer
# )
# 
# agent$chat("What was net revenue in EMEA?")
# #> Net revenue in EMEA was $2,400.

## -----------------------------------------------------------------------------
# agent$chat("Which rep was hired most recently?")
# #> Cy, hired 2024-01-20.

## -----------------------------------------------------------------------------
# library(shiny)
# library(shinychat)
# 
# ui <- bslib::page_fillable(chat_mod_ui("chat"))
# 
# server <- function(input, output, session) {
#   agent <- commons(
#     ellmer::chat_anthropic(),
#     data_sources = list(warehouse = sales),
#     semantic_layer = layer
#   )
#   chat_mod_server("chat", client = agent)
# }
# 
# shinyApp(ui, server)

## -----------------------------------------------------------------------------
# file.copy(
#   system.file("prompts/system-prompt.md", package = "commons"),
#   "system-prompt.md"
# )
# 
# commons(
#   ellmer::chat_anthropic(),
#   data_sources = list(warehouse = sales),
#   semantic_layer = layer,
#   system_prompt = ellmer::interpolate_file(
#     "system-prompt.md",
#     date = Sys.Date()
#   )
# )

## -----------------------------------------------------------------------------
DBI::dbDisconnect(con, shutdown = TRUE)

