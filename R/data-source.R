#' Create a data source
#'
#' A data source combines a \pkg{DBI} connection with a table registry for a
#' [commons()] agent. The agent queries the connection directly; no data is
#' copied.
#'
#' @param ... A single \pkg{DBI} connection.
#' @param tables Tables to describe in the agent's system prompt. Supply a
#'   character vector of table names, schema-qualified strings such as
#'   `"schema.table"`, or [DBI::Id] objects. The registry does not restrict
#'   which tables the connection can query. The default is every table
#'   returned by [DBI::dbListTables()]. Strings containing dots are treated
#'   as schema-qualified names; use `DBI::Id(table = "a.b")` for a literal
#'   table name containing dots.
#' @param dictionary An optional path to a data dictionary describing the
#'   source's tables and columns, in the
#'   [data-dict.yaml](https://data-dict.tidyverse.org/) format. See the
#'   `Data dictionaries` section.
#'
#' @section Data dictionaries:
#' A data dictionary records what each table's rows represent, what its
#' columns mean, allowed values and units, table relationships, and domain
#' terms. commons uses it in three places:
#'
#' * The dataset-level `description` and `details`, along with the glossary,
#'   appear in the system prompt. Use these fields for rules that span tables
#'   and guidance about which tables answer a question.
#' * The first time a conversation touches a table---via the `describe_table`
#'   tool or a SQL query---the tool result includes that table's prose,
#'   documented columns, relationships, and relevant glossary definitions.
#'   `describe_table` merges documented columns with the live schema.
#' * The `search_context` tool searches the dictionary's prose, with one
#'   passage for each table and glossary term.
#'
#' @section Trust:
#' Before passing a query to the database, the `run_sql` tool checks its
#' leading statement keyword against a denylist of common data- and
#' schema-modifying operations. The check is a keyword filter; database
#' permissions remain the access-control boundary. Open the connection in
#' read-only mode where the backend supports it, and grant it access only to
#' the tables the agent needs.
#'
#' @return A `commons_data_source` object.
#'
#' @seealso [commons()] to build an agent over one or more data sources.
#'
#' @examples
#' if (requireNamespace("duckdb", quietly = TRUE)) {
#'   con <- DBI::dbConnect(duckdb::duckdb())
#'   DBI::dbWriteTable(
#'     con,
#'     "sales",
#'     data.frame(region = c("EMEA", "APAC"), revenue = c(100, 200))
#'   )
#'
#'   src <- data_source(con)
#'   src$tables
#'
#'   DBI::dbDisconnect(con, shutdown = TRUE)
#' }
#'
#' @export
data_source <- function(..., tables = NULL, dictionary = NULL) {
  dots <- rlang::list2(...)
  check_connection_dots(dots)
  dictionary <- as_data_dictionary(dictionary)

  con <- dots[[1]]
  if (is.null(tables)) {
    return(new_data_source(con, DBI::dbListTables(con), dictionary = dictionary))
  }

  registry <- normalize_table_registry(tables)
  check_table_ids_exist(con, registry)
  new_data_source(
    con,
    registry$labels,
    table_ids = registry$ids,
    dictionary = dictionary
  )
}

list_tables <- function(data_source) {
  check_data_source(data_source)
  data_source$tables
}

new_data_source <- function(
  con,
  tables,
  table_ids = table_ids_from_labels(tables),
  dictionary = NULL
) {
  structure(
    list(
      con = con,
      tables = tables,
      table_ids = table_ids,
      dictionary = dictionary
    ),
    class = "commons_data_source"
  )
}

# Pick the data source a SQL tool call runs against. With one source no
# choice is needed; with several, the model passes a `source` name. The tool
# schema's enum should prevent bad values, but validate anyway so a bad call
# gets a clear error the model can act on.
resolve_sql_source <- function(sources, name, call = rlang::caller_env()) {
  if (length(sources) == 1) {
    return(sources[[1]])
  }
  if (!is.null(name) && name %in% names(sources)) {
    return(sources[[name]])
  }

  problem <- if (is.null(name)) {
    "{.arg source} is required when an agent has multiple data sources."
  } else {
    "No data source named {.val {name}}."
  }
  cli::cli_abort(
    c(problem, i = "Available sources: {.val {names(sources)}}."),
    call = call
  )
}

# Best-effort dialect hint for the system prompt. odbc and several other
# backends report a dbms name; fall back to the connection class.
source_dialect <- function(source) {
  info <- tryCatch(DBI::dbGetInfo(source$con), error = function(e) NULL)
  info$dbms.name %||% sub("_connection$", "", class(source$con)[[1]])
}

source_describe <- function(source, table, n_sample = 5, call = rlang::caller_env()) {
  id <- source$table_ids[[table]]
  if (is.null(id)) {
    cli::cli_abort(
      c(
        "No table named {.val {table}}.",
        i = "Available tables: {.val {source$tables}}."
      ),
      call = call
    )
  }

  sample <- DBI::dbGetQuery(
    source$con,
    sprintf(
      "SELECT * FROM %s LIMIT %d",
      DBI::dbQuoteIdentifier(source$con, id),
      n_sample
    )
  )
  schema <- data.frame(
    column = names(sample),
    type = vapply(sample, function(x) class(x)[[1]], character(1)),
    row.names = NULL
  )
  list(schema = schema, sample = sample)
}

source_query <- function(source, sql, call = rlang::caller_env()) {
  check_query(sql, call = call)
  DBI::dbGetQuery(source$con, sql)
}

# A keyword denylist, not a SQL parser: it anchors on the leading statement
# keyword, so it pairs with the read-only-connection recommendation rather
# than standing alone. Ported from posit-dev/querychat.
check_query <- function(sql, call = rlang::caller_env()) {
  normalized <- toupper(trimws(gsub(
    " +",
    " ",
    gsub("[\r\n\t]+", " ", sql)
  )))
  blocked <- c(
    "DELETE",
    "TRUNCATE",
    "CREATE",
    "DROP",
    "ALTER",
    "GRANT",
    "REVOKE",
    "EXEC",
    "EXECUTE",
    "CALL",
    "INSERT",
    "UPDATE",
    "MERGE",
    "REPLACE",
    "UPSERT"
  )
  pattern <- paste0("^(", paste(blocked, collapse = "|"), ")\\b")
  if (grepl(pattern, normalized)) {
    matched <- regmatches(normalized, regexpr(pattern, normalized))
    cli::cli_abort(
      c(
        "The query contains a disallowed operation: {.code {matched}}.",
        i = "Only read-only SELECT queries are allowed."
      ),
      call = call
    )
  }
  invisible(sql)
}

normalize_table_registry <- function(tables, call = rlang::caller_env()) {
  entries <- table_entries(tables, call = call)
  ids <- lapply(entries, table_entry_id, call = call)
  labels <- vapply(ids, table_id_label, character(1), call = call)

  duplicated_labels <- unique(labels[duplicated(labels)])
  if (length(duplicated_labels)) {
    cli::cli_abort(
      "{.arg tables} must not contain duplicate labels: {.val {duplicated_labels}}.",
      call = call
    )
  }

  names(ids) <- labels
  list(labels = labels, ids = ids)
}

table_entries <- function(tables, call = rlang::caller_env()) {
  if (inherits(tables, "Id")) {
    return(list(tables))
  }
  if (is.character(tables)) {
    return(as.list(tables))
  }
  if (is.list(tables)) {
    return(tables)
  }

  cli::cli_abort(
    "{.arg tables} must be a character vector, a list, or a {.cls DBI::Id}.",
    call = call
  )
}

table_entry_id <- function(table, call = rlang::caller_env()) {
  if (inherits(table, "Id")) {
    return(table)
  }

  if (
    !is.character(table) ||
      length(table) != 1 ||
      is.na(table) ||
      table == ""
  ) {
    cli::cli_abort(
      "Each entry in {.arg tables} must be a table name or a {.cls DBI::Id}.",
      call = call
    )
  }

  parts <- strsplit(table, ".", fixed = TRUE)[[1]]
  if (any(parts == "")) {
    cli::cli_abort(
      "Schema-qualified entries in {.arg tables} must not contain empty name components.",
      call = call
    )
  }

  if (length(parts) == 1) {
    return(DBI::Id(table = table))
  }

  DBI::Id(
    schema = paste(parts[-length(parts)], collapse = "."),
    table = parts[[length(parts)]]
  )
}

table_id_label <- function(id, call = rlang::caller_env()) {
  components <- id@name
  table <- if ("table" %in% names(components)) components[["table"]]

  if (is.null(table) || is.na(table)) {
    cli::cli_abort(
      "{.cls DBI::Id} entries in {.arg tables} must include a {.arg table} component.",
      call = call
    )
  }

  if (any(is.na(components) | components == "")) {
    cli::cli_abort(
      "{.cls DBI::Id} entries in {.arg tables} must not contain empty name components.",
      call = call
    )
  }

  paste(components, collapse = ".")
}

table_ids_from_labels <- function(tables) {
  ids <- lapply(tables, function(table) DBI::Id(table = table))
  names(ids) <- tables
  ids
}

# Per-table dbExistsTable() calls cost one round trip each, which dominates
# startup against a remote warehouse. Probe every table in a single zero-row
# query instead, and fall back to per-table checks only to name the missing
# tables when that probe fails.
check_table_ids_exist <- function(con, registry, call = rlang::caller_env()) {
  probes <- vapply(
    registry$ids,
    function(id) {
      sprintf("SELECT 1 FROM %s WHERE 1 = 0", DBI::dbQuoteIdentifier(con, id))
    },
    character(1)
  )
  probe_error <- tryCatch(
    {
      DBI::dbGetQuery(con, paste(probes, collapse = " UNION ALL "))
      NULL
    },
    error = function(err) err
  )
  if (is.null(probe_error)) {
    return(invisible(registry))
  }

  # Some backends emit an S4 dispatch note when handed a DBI::Id; that's an
  # implementation detail of the fallback, not something to report.
  exists <- suppressMessages(vapply(
    registry$ids,
    function(id) isTRUE(DBI::dbExistsTable(con, id)),
    logical(1)
  ))
  missing <- registry$labels[!exists]

  if (length(missing) == 0) {
    cli::cli_abort(
      "Failed to verify {.arg tables} against the connection.",
      parent = probe_error,
      call = call
    )
  }

  cli::cli_abort(
    "{.arg tables} names table{?s} not on the connection: {.val {missing}}.",
    call = call
  )
}

check_connection_dots <- function(dots, call = rlang::caller_env()) {
  if (length(dots) == 1 && inherits(dots[[1]], "DBIConnection")) {
    return(invisible(dots))
  }
  if (length(dots) == 0) {
    cli::cli_abort(
      "{.fn data_source} needs a database connection.",
      call = call
    )
  }
  if (length(dots) > 1) {
    cli::cli_abort(
      "{.fn data_source} takes a single database connection.",
      call = call
    )
  }
  cli::cli_abort(
    "{.fn data_source} needs a {.cls DBIConnection}, not {.obj_type_friendly {dots[[1]]}}.",
    call = call
  )
}

check_data_source <- function(data_source, call = rlang::caller_env()) {
  if (!inherits(data_source, "commons_data_source")) {
    cli::cli_abort(
      "{.arg data_source} must be a {.fn data_source}.",
      call = call
    )
  }
}

# A bare data_source() is accepted for the quick-start path; it has no name,
# so measures can't take its connection as an argument.
as_data_sources <- function(x, call = rlang::caller_env()) {
  if (inherits(x, "commons_data_source")) {
    return(list(x))
  }

  all_sources <- is.list(x) &&
    length(x) > 0 &&
    all(vapply(x, inherits, logical(1), "commons_data_source"))
  if (!all_sources) {
    cli::cli_abort(
      "{.arg data_sources} must be a {.fn data_source} or a named list of them.",
      call = call
    )
  }

  if (length(x) > 1 && !rlang::is_named(x)) {
    cli::cli_abort(
      "Each entry in {.arg data_sources} must be named.",
      call = call
    )
  }

  duplicated_names <- unique(names(x)[duplicated(names(x))])
  if (length(duplicated_names)) {
    cli::cli_abort(
      "{.arg data_sources} names must be unique; duplicated name{?s}: {.val {duplicated_names}}.",
      call = call
    )
  }

  x
}
