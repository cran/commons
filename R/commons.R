#' Create a commons agent
#'
#' `commons()` builds an [ellmer::Chat] with a system prompt that describes
#' the available data and tools for searching measures and context,
#' inspecting tables, and running SQL queries.
#'
#' `client` supplies the provider and model. commons replaces the client's
#' system prompt and tools. Use `agent$chat()` to ask questions or
#' [shinychat::chat_mod_ui()] and [shinychat::chat_mod_server()] to embed the
#' agent in a Shiny app.
#'
#' The agent first searches its [semantic_layer()] for a matching measure. If
#' one matches, the agent runs a calculation defined by your data team. If no
#' measure matches, it reads the data documentation, inspects the relevant
#' tables, and writes a SQL query.
#'
#' @param client An [ellmer::Chat] giving the provider and model to use, e.g.
#'   [ellmer::chat_anthropic()]. Any system prompt or tools already set on
#'   the client are ignored, with a warning; pass a system prompt to
#'   `system_prompt` instead.
#' @param data_sources A [data_source()], or a named list of them. Measures
#'   can take a source's connection as an argument named after the source; see
#'   [semantic_layer()]. When there are several sources, the `run_sql` and
#'   `describe_table` tools take a source's name as a `source` argument.
#' @param ... These dots are for future extensions and must be empty.
#' @param semantic_layer An optional [semantic_layer()].
#' @param system_prompt The agent's system prompt, as a single string. The
#'   default loads the markdown prompt shipped with commons and interpolates
#'   its `{{date}}` keyword. To customize it, copy the file into your project
#'   and interpolate the edited version:
#'
#'   ```r
#'   file.copy(
#'     system.file("prompts/system-prompt.md", package = "commons"),
#'     "system-prompt.md"
#'   )
#'   commons(
#'     # ...
#'     system_prompt = ellmer::interpolate_file(
#'       "system-prompt.md",
#'       date = Sys.Date()
#'     )
#'   )
#'   ```
#'
#'   Pass values for any `{{keyword}}` tokens you add as arguments to
#'   [ellmer::interpolate_file()]. commons appends the table and data
#'   dictionary documentation, so omit it from the file.
#'
#' @return An [ellmer::Chat] carrying commons' system prompt and tools.
#'
#' @seealso [data_source()] for database connections and table documentation,
#'   and [semantic_layer()] for governed calculations.
#'
#' @examples
#' if (requireNamespace("duckdb", quietly = TRUE)) {
#'   con <- DBI::dbConnect(duckdb::duckdb())
#'   DBI::dbWriteTable(
#'     con,
#'     "sales",
#'     data.frame(
#'       region = c("EMEA", "EMEA", "APAC"),
#'       revenue = c(100, 250, 90)
#'     )
#'   )
#'
#'   measure_file <- tempfile(fileext = ".R")
#'   writeLines(
#'     c(
#'       "#' Revenue by region",
#'       "#'",
#'       "#' @param region `string` Sales region.",
#'       "#' @measure",
#'       "revenue_by_region <- function(region, warehouse) {",
#'       "  DBI::dbGetQuery(",
#'       "    warehouse,",
#'       "    'SELECT sum(revenue) AS revenue FROM sales WHERE region = ?',",
#'       "    params = list(region)",
#'       "  )",
#'       "}"
#'     ),
#'     measure_file
#'   )
#'   layer <- semantic_layer(measure_file)
#'   unlink(measure_file)
#'
#'   agent <- commons(
#'     ellmer::chat_anthropic(),
#'     data_sources = list(warehouse = data_source(con)),
#'     semantic_layer = layer
#'   )
#'
#'   \dontrun{
#'   # Users will need an Anthropic API key to run this code.
#'   agent$chat("How much revenue came from EMEA?")
#'   }
#'
#'   DBI::dbDisconnect(con, shutdown = TRUE)
#' }
#'
#' @export
commons <- function(
  client,
  data_sources,
  ...,
  semantic_layer = NULL,
  system_prompt = ellmer::interpolate_file(
    system.file("prompts/system-prompt.md", package = "commons"),
    date = Sys.Date()
  )
) {
  rlang::check_dots_empty()
  check_client(client)
  data_sources <- as_data_sources(data_sources)
  semantic_layer <- semantic_layer %||% new_semantic_layer()
  check_semantic_layer(semantic_layer)
  check_system_prompt(system_prompt)

  state <- new_agent_state(data_sources, semantic_layer)

  agent <- client$clone()
  agent$set_turns(list())
  agent$set_system_prompt(commons_system_prompt(data_sources, system_prompt))
  agent$set_tools(build_commons_tools(state))
  agent
}

# The tool closures read from here rather than from their own enclosures, so
# every tool sees the same registry, sources, and first-touch tracker.
new_agent_state <- function(
  sources,
  semantic_layer,
  call = rlang::caller_env()
) {
  state <- new.env(parent = emptyenv())
  state$sources <- sources
  state$registry <- semantic_layer$measures
  state$injections <- resolve_injections(
    state$registry,
    measure_injectables(sources),
    call = call
  )
  state$context <- unlist(lapply(
    sources,
    function(source) dictionary_context_chunks(source$dictionary)
  )) %||%
    character()
  state$first_touch <- new.env(parent = emptyenv())
  state
}

# Measures can take a named source's connection as an argument.
measure_injectables <- function(sources) {
  named <- sources[rlang::have_name(sources)]
  lapply(named, function(source) source$con)
}

check_client <- function(client, call = rlang::caller_env()) {
  if (!inherits(client, "Chat")) {
    cli::cli_abort(
      "{.arg client} must be an {.cls ellmer::Chat}, e.g. from {.fn ellmer::chat_anthropic}.",
      call = call
    )
  }
  if (!is.null(client$get_system_prompt())) {
    cli::cli_warn(
      c(
        "The system prompt set on {.arg client} is ignored; commons builds
         its own.",
        i = "Pass it to the {.arg system_prompt} argument instead."
      ),
      call = call
    )
  }
  if (length(client$get_tools()) > 0) {
    cli::cli_warn(
      "The tools registered on {.arg client} are ignored; commons registers
       its own.",
      call = call
    )
  }
  invisible(client)
}
