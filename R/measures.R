#' Create a semantic layer
#'
#' A semantic layer collects governed measures for a [commons()] agent. The
#' agent searches these measures before writing SQL. When a measure matches,
#' the agent runs the calculation defined by your data team.
#'
#' @param ... Paths to R scripts or directories containing functions marked
#'   with `@measure`.
#'
#' @details
#' A measure is an ordinary R function documented with roxygen comments and
#' marked with `@measure`. It can take model-supplied and injected arguments:
#'
#' * Arguments documented with `@param` are supplied by the model.
#' * Undocumented arguments are hidden from the model. [commons()] supplies a
#'   matching data source's connection or keeps the argument's default. It
#'   errors if neither is available.
#'
#' Because the measure receives its connection as an argument, the semantic
#' layer can be defined before the connection is opened.
#'
#' For another dependency, such as an API client, use a default expression
#' that constructs it. Referring to a variable instead would make the measure
#' depend on the environment where the semantic layer was created.
#'
#' @return A `commons_semantic_layer` object.
#'
#' @seealso [commons()] to give a semantic layer to an agent.
#'
#' @examples
#' measure_file <- tempfile(fileext = ".R")
#' writeLines(
#'   c(
#'     "#' Revenue by region",
#'     "#'",
#'     "#' @param region `string` Sales region.",
#'     "#' @measure",
#'     "revenue_by_region <- function(region, warehouse) {",
#'     "  DBI::dbGetQuery(",
#'     "    warehouse,",
#'     "    'SELECT sum(revenue) AS revenue FROM sales WHERE region = ?',",
#'     "    params = list(region)",
#'     "  )",
#'     "}"
#'   ),
#'   measure_file
#' )
#'
#' layer <- semantic_layer(measure_file)
#' unlink(measure_file)
#'
#' @export
semantic_layer <- function(...) {
  measures <- expand_measures(rlang::list2(...), rlang::caller_env())

  check_measures(measures)
  names(measures) <- vapply(measures, tool_name, character(1))

  duplicated <- names(measures)[duplicated(names(measures))]
  if (length(duplicated)) {
    cli::cli_abort(
      "Measure names must be unique; duplicated name{?s}: {.val {unique(duplicated)}}."
    )
  }

  new_semantic_layer(measures)
}

measure <- function(name, description, fn, arguments = list(), title = NULL) {
  rlang::check_string(name)
  rlang::check_string(description)
  rlang::check_string(title, allow_null = TRUE)
  title <- title %||% humanize_name(name)
  ellmer::tool(
    fn,
    description,
    arguments = fill_injected_arguments(arguments, fn),
    name = name,
    annotations = ellmer::tool_annotations(title = title)
  )
}

expand_measures <- function(args, env = rlang::caller_env()) {
  expanded <- lapply(args, function(arg) {
    if (is.character(arg)) {
      read_measures(arg, env)
    } else if (is_measure_list(arg)) {
      arg
    } else {
      list(arg)
    }
  })
  unlist(expanded, recursive = FALSE) %||% list()
}

# Arguments of `fn` not described in `arguments` are supplied by commons(),
# not the model. type_ignore() satisfies ellmer's check that `arguments`
# matches formals(fn) but stays out of the model-visible schema, which is how
# measure_injection_names() tells the two kinds of argument apart.
fill_injected_arguments <- function(arguments, fn) {
  injected <- setdiff(names(formals(fn)), names(arguments))
  for (nm in injected) {
    arguments[[nm]] <- ellmer::type_ignore()
  }
  arguments
}

measure_injection_names <- function(td) {
  setdiff(names(formals(td)), names(tool_properties(td)))
}

# Look up each measure's undocumented arguments among the agent's named
# data_sources entries. An unmatched argument keeps its default; one with no
# default is an error.
resolve_injections <- function(
  registry,
  injectables,
  call = rlang::caller_env()
) {
  lapply(registry, function(td) {
    needed <- measure_injection_names(td)
    unmatched <- setdiff(needed, names(injectables))
    no_default <- unmatched[vapply(
      unmatched,
      function(nm) identical(formals(td)[[nm]], quote(expr = )),
      logical(1)
    )]
    if (length(no_default)) {
      available <- if (length(injectables)) {
        cli::format_inline("Available sources: {.val {names(injectables)}}.")
      } else {
        "{.arg data_sources} has no named sources."
      }
      cli::cli_abort(
        c(
          "Measure {.val {tool_name(td)}} has undocumented {cli::qty(no_default)}argument{?s} {.arg {no_default}} matching no data source.",
          i = available
        ),
        call = call
      )
    }
    injectables[intersect(needed, names(injectables))]
  })
}

new_semantic_layer <- function(measures = list()) {
  structure(list(measures = measures), class = "commons_semantic_layer")
}

check_semantic_layer <- function(semantic_layer, call = rlang::caller_env()) {
  if (!inherits(semantic_layer, "commons_semantic_layer")) {
    cli::cli_abort(
      "{.arg semantic_layer} must be a {.fn semantic_layer}.",
      call = call
    )
  }
}

check_measures <- function(measures, call = rlang::caller_env()) {
  ok <- vapply(measures, inherits, logical(1), "ellmer::ToolDef")
  if (!all(ok)) {
    cli::cli_abort(
      "Every item in {.arg semantic_layer} must be an {.cls ellmer::ToolDef}.",
      call = call
    )
  }
}

is_measure_list <- function(x) {
  is.list(x) && !inherits(x, "ellmer::ToolDef")
}

search_measures_text <- function(registry, query, source_names = character()) {
  if (length(registry) == 0) {
    return("No measures are registered.")
  }

  catalog <- vapply(
    registry,
    function(td) paste(tool_name(td), tool_description(td)),
    character(1)
  )
  hits <- lexical_rank(query, catalog, n = 5)
  if (length(hits) == 0) {
    return(sprintf(
      "No measure matches \"%s\". Consider writing a SQL query.",
      query
    ))
  }

  blocks <- vapply(
    registry[hits],
    measure_schema_text,
    character(1),
    source_names = source_names
  )
  paste(blocks, collapse = "\n\n")
}

measure_schema_text <- function(td, source_names = character()) {
  props <- tool_properties(td)
  args <- if (length(props) == 0) {
    "  (no arguments)"
  } else {
    paste(
      vapply(
        names(props),
        function(nm) arg_schema_line(nm, props[[nm]]),
        character(1)
      ),
      collapse = "\n"
    )
  }
  sprintf(
    "### %s\n%s\n\n%sarguments:\n%s",
    tool_name(td),
    tool_description(td),
    measure_sources_line(td, source_names),
    args
  )
}

# When an agent has several data sources, noting which one(s) a measure
# queries points the SQL fallback at the right source. `source_names` is empty
# for single-source agents, so the line never appears there.
measure_sources_line <- function(td, source_names) {
  used <- intersect(measure_injection_names(td), source_names)
  if (length(used) == 0) {
    return("")
  }
  sprintf("sources: %s\n", paste(used, collapse = ", "))
}

arg_schema_line <- function(name, type) {
  kind <- type_kind(type)
  required <- if (isTRUE(S7::prop(type, "required"))) "required" else "optional"
  detail <- switch(
    kind,
    enum = sprintf("one of {%s}", paste(type_values(type), collapse = ", ")),
    array = sprintf(
      "array of {%s}",
      paste(type_values(S7::prop(type, "items")), collapse = ", ")
    ),
    kind
  )
  desc <- S7::prop(type, "description") %||% ""
  sprintf("  - %s (%s, %s) %s", name, detail, required, desc)
}

# The provider sees only `call_measure`, so measure arguments are checked here.
validate_measure_args <- function(td, args, call = rlang::caller_env()) {
  props <- tool_properties(td)
  args <- args %||% list()

  unknown <- setdiff(names(args), names(props))
  if (length(unknown)) {
    cli::cli_abort(
      c(
        "Unknown {cli::qty(unknown)}argument{?s} for measure {.val {tool_name(td)}}: {.val {unknown}}.",
        i = "Valid arguments: {.val {names(props)}}."
      ),
      call = call
    )
  }

  for (nm in names(props)) {
    type <- props[[nm]]
    if (is.null(args[[nm]])) {
      if (isTRUE(S7::prop(type, "required"))) {
        cli::cli_abort(
          "Measure {.val {tool_name(td)}} requires argument {.arg {nm}}.",
          call = call
        )
      }
      next
    }
    args[[nm]] <- coerce_arg(td, nm, type, args[[nm]], call = call)
  }

  args
}

coerce_arg <- function(td, nm, type, value, call = rlang::caller_env()) {
  kind <- type_kind(type)

  if (kind %in% c("enum", "array")) {
    allowed <- if (kind == "enum") {
      type_values(type)
    } else {
      type_values(S7::prop(type, "items"))
    }
    bad <- setdiff(as.character(value), allowed)
    if (length(bad)) {
      cli::cli_abort(
        c(
          "Invalid {cli::qty(bad)}value{?s} for {.arg {nm}} of measure {.val {tool_name(td)}}: {.val {bad}}.",
          i = "Allowed: {.val {allowed}}."
        ),
        call = call
      )
    }
    return(as.character(value))
  }

  switch(
    kind,
    number = as.numeric(value),
    integer = as.integer(value),
    boolean = as.logical(value),
    as.character(value)
  )
}

# Isolate the ellmer internals used by registered measure tools.
tool_name <- function(td) S7::prop(td, "name")
tool_description <- function(td) S7::prop(td, "description")
tool_title <- function(td) {
  annotations <- S7::prop(td, "annotations")
  annotations$title %||% humanize_name(tool_name(td))
}
tool_properties <- function(td) {
  S7::prop(S7::prop(td, "arguments"), "properties")
}
type_values <- function(type) S7::prop(type, "values")

type_kind <- function(type) {
  cls <- class(type)[[1]]
  switch(
    cls,
    "ellmer::TypeEnum" = "enum",
    "ellmer::TypeArray" = "array",
    "ellmer::TypeBasic" = S7::prop(type, "type"),
    "string"
  )
}

humanize_name <- function(x) {
  gsub("_", " ", x, fixed = TRUE)
}
