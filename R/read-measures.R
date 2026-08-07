read_measures <- function(paths, env = globalenv()) {
  registerS3method(
    "roxy_tag_parse",
    "roxy_tag_measure",
    function(x) roxygen2::tag_toggle(x),
    envir = asNamespace("roxygen2")
  )

  files <- resolve_measure_files(paths)
  measure_env <- new.env(parent = env)
  for (file in files) {
    sys.source(file, envir = measure_env)
  }

  unlist(
    lapply(files, function(file) read_measures_file(file, measure_env)),
    recursive = FALSE
  ) %||% list()
}

resolve_measure_files <- function(paths, call = rlang::caller_env()) {
  missing <- paths[!file.exists(paths)]
  if (length(missing)) {
    cli::cli_abort(
      "{cli::qty(missing)}Path{?does/do} not exist: {.path {missing}}.",
      call = call
    )
  }

  files <- unlist(lapply(paths, function(path) {
    if (dir.exists(path)) {
      list.files(path, pattern = "[.][Rr]$", full.names = TRUE)
    } else {
      path
    }
  }))
  unique(files)
}

read_measures_file <- function(file, env) {
  blocks <- roxygen2::parse_file(file)
  measures <- lapply(blocks, function(block) block_to_measure(block, env))
  Filter(Negate(is.null), measures)
}

block_to_measure <- function(block, env) {
  if (is.null(roxygen2::block_get_tag(block, "measure"))) {
    return(NULL)
  }

  name <- block$object$topic
  fn <- get(name, envir = env)
  if (!is.function(fn)) {
    return(NULL)
  }

  measure(
    name,
    block_description(block),
    fn,
    arguments = block_arguments(block, fn)
  )
}

block_description <- function(block) {
  parts <- c(
    roxygen2::block_get_tag_value(block, "title"),
    roxygen2::block_get_tag_value(block, "description"),
    {
      value <- roxygen2::block_get_tag_value(block, "return")
      if (!is.null(value)) paste0("Returns: ", value)
    }
  )
  paste(parts, collapse = "\n\n")
}

block_arguments <- function(block, fn) {
  param_text <- block_param_text(block)
  args <- list()
  for (name in names(formals(fn))) {
    if (is.null(param_text[[name]])) {
      next
    }
    required <- identical(formals(fn)[[name]], quote(expr = ))
    default <- if (required) NULL else formals(fn)[[name]]
    args[[name]] <- param_type(
      param_text[[name]],
      default = default,
      required = required
    )
  }
  args
}

block_param_text <- function(block) {
  tags <- roxygen2::block_get_tags(block, "param")
  names <- vapply(tags, function(tag) tag$val$name, character(1))
  text <- lapply(tags, function(tag) trimws(sub("^\\s*\\S+\\s*", "", tag$raw)))
  names(text) <- names
  text
}

param_type <- function(text, default, required) {
  pattern <- "^\\s*`([a-zA-Z]+)(\\[[^]]*\\])?`\\s*(.*)$"
  match <- regmatches(text, regexec(pattern, text, perl = TRUE))[[1]]

  if (length(match) == 0) {
    return(infer_type(default, description = trimws(text), required = required))
  }

  kind <- tolower(match[2])
  bracket <- match[3]
  description <- trimws(match[4])

  if (nzchar(bracket)) {
    inner <- trimws(substr(bracket, 2, nchar(bracket) - 1))
    if (kind == "enum") {
      return(ellmer::type_enum(
        values = trimws(strsplit(inner, ",")[[1]]),
        description = description,
        required = required
      ))
    }
    return(ellmer::type_array(
      items = scalar_type(kind, ""),
      description = description,
      required = required
    ))
  }

  scalar_type(kind, description, required = required)
}

scalar_type <- function(kind, description, required = TRUE) {
  switch(
    kind,
    integer = ellmer::type_integer(description, required = required),
    number = ellmer::type_number(description, required = required),
    boolean = ellmer::type_boolean(description, required = required),
    ellmer::type_string(description, required = required)
  )
}

infer_type <- function(default, description, required) {
  value <- tryCatch(eval(default), error = function(e) NULL)
  kind <- if (is.logical(value)) {
    "boolean"
  } else if (is.integer(value)) {
    "integer"
  } else if (is.numeric(value)) {
    "number"
  } else {
    "string"
  }
  scalar_type(kind, description, required = required)
}
