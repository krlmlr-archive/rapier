extract_api <- function(pkg = ".") {
  loaded <- pkgload::load_all(pkg = pkg, export_all = FALSE)
  structure(list(loaded = loaded), class = "rapier")
}

#' @export
print.rapier <- function(x, ...) {
  cat(format(x), sep = "\n")
}

#' @export
format.rapier <- function(x, ...) {
  c(
    add_caption("Exports", format_exports(x))
  )
}

add_caption <- function(caption, x) {
  if (length(x) == 0L) {
    character()
  } else {
    c(paste0("# ", caption), "", x)
  }
}

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

sort_c <- function(x) {
  withr::with_collate("C", sort(x))
}
