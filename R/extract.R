#' @export
extract_api <- function(pkg = ".") {
  pkg <- pkgload::as.package(pkg)
  loaded <- pkgload::load_all(pkg = pkg, export_all = FALSE)
  structure(list(loaded = loaded, name = pkg$package), class = "rapier")
}

#' @export
write_api_to_file <- function(pkg = ".", path = NULL) {
  pkg <- pkgload::as.package(pkg)
  if (is.null(path)) {
    path <- file.path(pkg$path, "API")
  }
  api <- extract_api(pkg)
  writeLines(format(api), path)
  message("API written to ", path)
}

#' @export
print.rapier <- function(x, ...) {
  cat(format(x), sep = "\n")
}

#' @export
format.rapier <- function(x, ...) {
  c(
    format_caption("API for ", x$name, " package", level = 1),
    add_caption("Exports", format_exports(x)),
    add_caption("S3 methods", format_s3_methods(x))
  )
}

format_caption <- function(..., level = 2) {
  c(paste0(paste(rep("#", level), collapse = ""), " ", ...))
}

add_caption <- function(caption, x) {
  if (length(x) == 0L) {
    character()
  } else {
    c("", format_caption(caption), "", x)
  }
}
