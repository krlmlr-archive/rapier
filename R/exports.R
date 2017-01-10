format_exports <- function(x) {
  env <- x$loaded$env
  ns <- env$.__NAMESPACE__.
  export_names <- sort_c(ls(ns$exports, all = TRUE))
  exports <- mget(export_names, env)
  unlist(unname(Map(format_export, names(exports), exports)))
}

format_export <- function(name, value) {
  if (is.function(value)) {
    format_export_fun(name, value)
  } else {
    format_export_data(name, value)
  }
}

format_export_fun <- function(name, fun) {
  paste0(name, "(", format_args(formals(fun)), ")")
}

format_args <- function(args) {
  if (length(args) == 0L) {
    ""
  } else {
    paste0(names(args), vapply(args, format_default, character(1L)), collapse = ", ")
  }
}

format_default <- function(lang) {
  if (identical(as.character(lang), "")) {
    ""
  } else {
    paste0(" = ", paste(deparse(lang, width.cutoff = 500), collapse =""))
  }
}

format_export_data <- function(name, data) {
  warning("NYI: data for ", name)
}
