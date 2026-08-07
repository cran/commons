
<!-- README.md is generated from README.Rmd. Please edit that file -->

# commons

commons builds self-service data science agents for your organization:
agents that answer data questions using the definitions your data team
already maintains.

An agent is built from a `data_source()`, which is what it can query,
and a `semantic_layer()`, which is a pool of trusted calculations. When
a question matches a measure in the semantic layer, the agent runs that
measure. When nothing matches, it falls back to reading your data
documentation and writing a SQL query.

## Installation

``` r
install.packages("commons")
```

## Usage

``` r
library(commons)
```

Point a data source at a database and, optionally, at a [data dictionary](https://data-dict.tidyverse.org/)
describing it:

``` r
con <- DBI::dbConnect(duckdb::duckdb())
DBI::dbWriteTable(con, "orders", data.frame(
  region = c("EMEA", "Americas", "EMEA", "APAC"),
  revenue = c(500, 900, 1200, 300),
  refunded = c(0, 100, 0, 0)
))

sales <- data_source(con, tables = "orders")
```

Define the calculations you want the agent to prefer. Arguments that
aren’t in the `arguments` schema are hidden from the model. An argument
named after a data source receives that source’s connection.

``` r
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
    "    'SELECT sum(revenue - refunded) AS net FROM orders WHERE region = ?',",
    "    params = list(region)",
    "  )",
    "}"
  ),
  measure_file
)

layer <- semantic_layer(measure_file)
unlink(measure_file)
```

Then, assemble the pieces with `commons()`. The function outputs an
`ellmer::Chat`, so it works with
[shinychat](https://posit-dev.github.io/shinychat/) out of the box.

``` r
agent <- commons(
  ellmer::chat_anthropic(),
  data_sources = list(warehouse = sales),
  semantic_layer = layer
)

agent$chat("What was net revenue in EMEA?")
#> Net revenue in EMEA was $1,700.
```

That answer came from `net_revenue_by_region`, not from SQL the model
wrote, so “net revenue” means what your organization says it means.

See `vignette("commons")` to learn more.
