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
