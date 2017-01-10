ns_tags <- c('export', 'exportClass', 'exportMethod', 'exportPattern',
  'rawNamespace')

#' Roclet: make API.
#'
#' This roclet automates the production of a `API` file
#' which contains the formals of all exported functions.
#'
#' @export
#' @seealso `vignette("api", package = "rapier")`
#' @aliases export exportClass exportMethod S3method
api_roclet <- function() {
  roclet("api")
}

#' @export
roclet_process.roclet_api <- function(x, parsed, base_path,
                                      global_options = list()) {
  ns <- unlist(lapply(parsed$blocks, block_to_ns)) %||% character()
  sort_c(unique(ns))
}

#' @export
roclet_tags.roclet_api <- function(x) {
  list(
    export = tag_words_line,
    exportClass = tag_words(1),
    exportMethod = tag_words(1),
    exportPattern = tag_words(1),
    rawNamespace = tag_code
  )
}

block_to_ns <- function(block) {
  tags <- intersect(names(block), ns_tags)
  lapply(tags, ns_process_tag, block = block)
}

ns_process_tag <- function(tag_name, block) {
  f <- get(paste0("ns_", tag_name), mode = "function")
  tags <- block[names(block) == tag_name]

  lapply(tags, f, block = block)
}

#' @export
roclet_output.roclet_api <- function(x, results, base_path, ...) {
  API <- file.path(base_path, "API")
  results <- c(made_by("#"), results)

  # Always check for roxygen2 header before overwriting API (#436),
  # even when running for the first time
  write_if_different(API, results, check = TRUE)

  API
}

#' @export
roclet_clean.roclet_api <- function(x, base_path) {
  API <- file.path(base_path, "API")
  if (made_by_roxygen(API)) {
    unlink(API)
  }
}

# Functions that take complete block and return API lines
ns_export <- function(tag, block) {
  if (identical(tag, "")) {
    # FIXME: check for empty exports (i.e. no name)
    default_export(block$object, block)
  } else {
    export(tag)
  }
}
default_export <- function(x, block) UseMethod("default_export")
#' @export
default_export.s4class   <- function(x, block) export_class(x$value@className)
#' @export
default_export.s4generic  <- function(x, block) export(x$value@generic)
#' @export
default_export.s4method  <- function(x, block) export_s4_method(x$value@generic)
#' @export
default_export.s3method  <- function(x, block) export_s3_method(attr(x$value, "s3method"))
#' @export
default_export.rcclass   <- function(x, block) export_class(x$value@className)
#' @export
default_export.default   <- function(x, block) export(x$alias)
#' @export
default_export.NULL      <- function(x, block) export(block$name)

ns_S3method          <- function(tag, block) {
  block_warning(block, "@S3method is deprecated. Please use @export instead")
  export_s3_method(tag)
}
ns_exportClass       <- function(tag, block) export_class(tag)
ns_exportMethod      <- function(tag, block) export_s4_method(tag)
ns_exportPattern     <- function(tag, block) one_per_line("exportPattern", tag)
ns_import            <- function(tag, block) one_per_line("import", tag)
ns_importFrom        <- function(tag, block) repeat_first("importFrom", tag)
ns_importClassesFrom <- function(tag, block) repeat_first("importClassesFrom", tag)
ns_importMethodsFrom <- function(tag, block) repeat_first("importMethodsFrom", tag)
ns_useDynLib         <- function(tag, block) {
  if (length(tag) == 1) {
    return(paste0("useDynLib(", quote_if_needed(tag), ")"))
  }

  if (any(grepl(",", tag))) {
    # If there's a comma in list, don't quote output. This makes it possible
    # for roxygen2 to support other API forms not otherwise mapped
    args <- paste0(tag, collapse = " ")
    paste0("useDynLib(", args, ")")
  } else {
    repeat_first("useDynLib", tag)
  }
}
ns_rawNamespace       <- function(tag, block) tag

# Functions used by both default_export and ns_* functions
export           <- function(x) one_per_line("export", x)
export_class     <- function(x) one_per_line("exportClasses", x)
export_s4_method <- function(x) one_per_line("exportMethods", x)
export_s3_method <- function(x) fun_args("S3method", x)

one_per_line <- function(name, x) {
  paste0(name, "(", quote_if_needed(x), ")")
}
repeat_first <- function(name, x) {
  paste0(name, "(", quote_if_needed(x[1]), ",", quote_if_needed(x[-1]), ")")
}
fun_args <- function(name, x) {
  if (any(grepl(",", x))) {
    # If there's a comma in list, don't quote output. This makes it possible
    # for roxygen2 to support other API forms not otherwise mapped
    args <- paste0(x, collapse = ", ")
  } else {
    args <- paste0(quote_if_needed(x), collapse = ",")
  }

  paste0(name, "(", args, ")")
}

quote_if_needed <- function(x) {
  needs_quotes <- !has.quotes(x) & !is.syntactic(x)
  x[needs_quotes] <- paste0('"', str_replace_all(x[needs_quotes], '(["\\\\])', "\\\\\\1"), '"')
  x
}
is.syntactic <- function(x) make.names(x) == x
has.quotes <- function(x) str_detect(x, "^('|\").*\\1$")
