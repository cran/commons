build_commons_tools <- function(state) {
  list(
    tool_search_measures(state),
    tool_call_measure(state),
    tool_search_context(state),
    tool_describe_table(state),
    tool_run_sql(state)
  )
}

tool_search_measures <- function(state) {
  source_names <- if (length(state$sources) > 1) {
    names(state$sources)
  } else {
    character()
  }
  ellmer::tool(
    function(query) {
      body <- search_measures_text(state$registry, query, source_names)
      tool_result(
        body,
        title = "Searched measures",
        icon = maybe_icon("search")
      )
    },
    "Search registered measures. Returns matching measures with their argument schemas. Use this before call_measure.",
    arguments = list(
      query = ellmer::type_string(
        "What you want to measure, in plain language."
      )
    ),
    name = "search_measures",
    annotations = ellmer::tool_annotations(
      title = "Search measures",
      icon = maybe_icon("search"),
      read_only_hint = TRUE
    )
  )
}

tool_call_measure <- function(state) {
  ellmer::tool(
    function(name, arguments = "{}") {
      call_measure_tool(
        state$registry,
        name,
        arguments,
        injections = state$injections
      )
    },
    "Run a registered measure returned by search_measures. `arguments` is a JSON object using exactly the argument names from search_measures.",
    arguments = list(
      name = ellmer::type_string(
        "The measure name, exactly as returned by search_measures."
      ),
      arguments = ellmer::type_string(
        "A JSON object of the measure's arguments."
      )
    ),
    name = "call_measure",
    annotations = ellmer::tool_annotations(
      title = "Measure",
      icon = maybe_icon("shield-check"),
      read_only_hint = TRUE
    )
  )
}

tool_search_context <- function(state) {
  ellmer::tool(
    function(query) search_context_tool(state$context, query),
    "Search the data documentation for metric definitions, data notes, and table relationships.",
    arguments = list(
      query = ellmer::type_string(
        "What you need context about, in plain language."
      )
    ),
    name = "search_context",
    annotations = ellmer::tool_annotations(
      title = "Search context",
      icon = maybe_icon("book"),
      read_only_hint = TRUE
    )
  )
}

tool_describe_table <- function(state) {
  ellmer::tool(
    function(table, source = NULL) {
      describe_table_tool(
        resolve_sql_source(state$sources, source),
        table,
        source_name = source,
        tracker = state$first_touch
      )
    },
    "Describe a table: columns, types, and sample rows. Use this before writing SQL against an unfamiliar table.",
    arguments = list(
      table = ellmer::type_string(
        "The table name, as listed in the system prompt."
      ),
      source = sql_source_type(state$sources)
    ),
    name = "describe_table",
    annotations = ellmer::tool_annotations(
      title = "Describe table",
      icon = maybe_icon("table"),
      read_only_hint = TRUE
    )
  )
}

tool_run_sql <- function(state) {
  ellmer::tool(
    function(sql, source = NULL) {
      run_sql_tool(
        resolve_sql_source(state$sources, source),
        sql,
        source_name = source,
        tracker = state$first_touch
      )
    },
    "Run a read-only SELECT query against a data source. Use this when no registered measure answers the question.",
    arguments = list(
      sql = ellmer::type_string(
        "A read-only SELECT query, in the data source's SQL dialect."
      ),
      source = sql_source_type(state$sources)
    ),
    name = "run_sql",
    annotations = ellmer::tool_annotations(
      title = "SQL",
      icon = maybe_icon("code-square"),
      read_only_hint = TRUE
    )
  )
}

# With one source there's nothing to choose, so the model never sees the
# `source` argument (type_ignore keeps it out of the schema).
sql_source_type <- function(sources) {
  if (length(sources) == 1) {
    return(ellmer::type_ignore())
  }
  ellmer::type_enum(
    values = names(sources),
    description = "The data source to use, as listed in the system prompt."
  )
}

call_measure_tool <- function(registry, name, arguments, injections = list()) {
  td <- registry[[name]]
  if (is.null(td)) {
    detail <- if (length(registry)) {
      cli::format_inline("Registered measures: {.val {names(registry)}}.")
    } else {
      "No measures are registered."
    }
    cli::cli_abort(c("No measure named {.val {name}}.", i = detail))
  }
  args <- validate_measure_args(td, parse_json_args(arguments))
  value <- collect_lazy_table(do.call(td, c(args, injections[[name]])))
  body <- format_measure_value(value)

  tool_result(
    body,
    title = sprintf("Measure: %s", html_escape(tool_title(td))),
    icon = maybe_icon("shield-check"),
    markdown = paste(
      c(measure_args_markdown(args), body),
      collapse = "\n\n"
    )
  )
}

search_context_tool <- function(context, query) {
  if (length(context) == 0) {
    return("No data documentation is available for this agent.")
  }
  hits <- lexical_rank(query, context, n = 3)
  body <- if (length(hits)) {
    paste(context[hits], collapse = "\n\n---\n\n")
  } else {
    sprintf("No context found for \"%s\".", query)
  }
  tool_result(
    body,
    title = "Searched context",
    icon = maybe_icon("book"),
    markdown = body
  )
}

describe_table_tool <- function(
  source,
  table,
  source_name = NULL,
  tracker = NULL
) {
  d <- source_describe(source, table)
  entry <- source$dictionary$tables[[table]]

  sample <- sprintf(
    "Sample rows:\n\n%s",
    df_to_markdown(d$sample, max_rows = 5)
  )
  if (is.null(entry)) {
    parts <- c(
      sprintf("Columns of `%s`:\n\n%s", table, df_to_markdown(d$schema)),
      sample
    )
  } else {
    mark_table_touched(tracker, source_name, table)
    columns <- sprintf(
      "Columns of `%s`:\n\n%s",
      table,
      dictionary_columns_text(entry$columns, live = d$schema)
    )
    parts <- c(
      dictionary_entry_parts(source$dictionary, table, columns),
      sample
    )
  }

  body <- paste(parts, collapse = "\n\n")
  tool_result(
    body,
    title = sprintf("Described %s%s", html_escape(table), source_label(source_name)),
    icon = maybe_icon("table"),
    markdown = body
  )
}

run_sql_tool <- function(source, sql, source_name = NULL, tracker = NULL) {
  res <- source_query(source, sql)
  body <- df_to_markdown(res)
  entries <- dictionary_sql_entries(source, sql, source_name, tracker)
  tool_result(
    paste(c(body, entries), collapse = "\n\n"),
    title = sprintf("Ran SQL%s", source_label(source_name)),
    icon = maybe_icon("code-square"),
    markdown = sprintf("```sql\n%s\n```\n\n%s", sql, body)
  )
}

# Dictionary entries for tables this query touches that the conversation
# hasn't seen yet, via either tool. Matching is by table name in the SQL
# text, which is reliable in a way column matching is not; a false positive
# appends a harmless note.
dictionary_sql_entries <- function(source, sql, source_name, tracker) {
  dictionary <- source$dictionary
  tables <- names(dictionary$tables)
  hits <- tables[vapply(
    tables,
    function(table) grepl(word_pattern(table), sql, ignore.case = TRUE),
    logical(1)
  )]
  hits <- hits[!vapply(
    hits,
    function(table) table_touched(tracker, source_name, table),
    logical(1)
  )]
  if (length(hits) == 0) {
    return(NULL)
  }

  for (table in hits) {
    mark_table_touched(tracker, source_name, table)
  }
  vapply(
    hits,
    function(table) dictionary_entry_text(dictionary, table),
    character(1)
  )
}

# Which tables' dictionary entries this conversation has already seen, so
# run_sql doesn't re-deliver them. A NULL tracker (tools used outside an
# agent) treats every touch as the first.
table_touched <- function(tracker, source_name, table) {
  !is.null(tracker) && isTRUE(tracker[[touch_key(source_name, table)]])
}

mark_table_touched <- function(tracker, source_name, table) {
  if (!is.null(tracker)) {
    tracker[[touch_key(source_name, table)]] <- TRUE
  }
  invisible(NULL)
}

touch_key <- function(source_name, table) {
  paste(source_name %||% "", table, sep = "\n")
}

source_label <- function(source_name) {
  if (is.null(source_name)) {
    return("")
  }
  sprintf(" (%s)", html_escape(source_name))
}

tool_result <- function(
  value,
  title,
  icon = NULL,
  markdown = NULL,
  open = FALSE
) {
  display <- list(title = title, open = open, show_request = FALSE)
  if (!is.null(icon)) {
    display$icon <- icon
  }
  if (!is.null(markdown)) {
    display$markdown <- markdown
  }

  ellmer::ContentToolResult(value = value, extra = list(display = display))
}

parse_json_args <- function(x) {
  if (is.null(x) || identical(x, "") || identical(x, "{}")) {
    return(list())
  }
  if (is.list(x)) {
    return(x)
  }
  as.list(jsonlite::fromJSON(x, simplifyVector = TRUE))
}

collect_lazy_table <- function(value) {
  if (inherits(value, "tbl_sql")) {
    rlang::check_installed("dplyr")
    value <- dplyr::collect(value)
  }
  value
}

format_measure_value <- function(value) {
  if (is.data.frame(value)) {
    return(df_to_markdown(value))
  }
  if (is.atomic(value) && length(value) <= 20) {
    return(paste(format(value, trim = TRUE), collapse = ", "))
  }
  paste(utils::capture.output(value), collapse = "\n")
}

# Shown above the result so a reader can see what the measure was asked for.
measure_args_markdown <- function(args) {
  if (length(args) == 0) {
    return(NULL)
  }
  lines <- vapply(
    names(args),
    function(nm) {
      sprintf("- %s: %s", humanize_name(nm), format_arg_value(args[[nm]]))
    },
    character(1)
  )
  paste(lines, collapse = "\n")
}

format_arg_value <- function(x) {
  if (is.null(x)) {
    return("")
  }
  if (is.atomic(x)) {
    return(paste(format(x, trim = TRUE), collapse = ", "))
  }
  as.character(jsonlite::toJSON(x, auto_unbox = TRUE))
}
