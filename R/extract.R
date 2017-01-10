extract_api <- function(pkg = ".") {
  pkg <- pkgload::as.package(pkg)
  loaded <- pkgload::load_all(pkg = pkg, export_all = FALSE)
  structure(list(loaded = loaded, name = pkg$package), class = "rapier")
}

#' @export
print.rapier <- function(x, ...) {
  cat(format(x), sep = "\n")
}

#' @export
format.rapier <- function(x, ...) {
  c(
    format_caption("API for ", x$name, " package", level = 1),
    add_caption("Exports", format_exports(x))
  )
}

format_caption <- function(..., level = 2) {
  c(paste0(paste(rep("#", level), collapse = ""), " ", ...), "")
}

add_caption <- function(caption, x) {
  if (length(x) == 0L) {
    character()
  } else {
    c(format_caption(caption), x)
  }
}
