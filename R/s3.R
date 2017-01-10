format_s3_methods <- function(x) {
  env <- x$loaded$env
  s3_table <- env$.__S3MethodsTable__.
  export_names <- sort_c(ls(s3_table, all = TRUE))
  exports <- mget(export_names, env, ifnotfound = rep(list(NULL), length(export_names)))
  exports <- purrr::compact(exports)
  unlist(unname(Map(format_export, names(exports), exports)))
}
