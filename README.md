# PROJECT HAS BEEN MERGED

Development continues in https://github.com/r-pkgs/functionmap2.

<!-- README.md is generated from README.Rmd. Please edit that file -->
rapier
======

The goal of rapier is to create a list of exported functions and methods for a package. This is useful to verify if a package's API has changed between releases.

Example
-------

``` r
rapier::extract_api("../tibble")
#> Loading tibble
#> # API for tibble package
#> 
#> ## Exports
#> 
#> add_column(.data, ..., .before = NULL, .after = NULL)
#> add_row(.data, ..., .before = NULL, .after = NULL)
#> as.tibble(x, ...)
#> as_data_frame(x, ...)
#> as_tibble(x, ...)
#> column_to_rownames(df, var = "rowname")
#> data_frame(...)
#> data_frame_(xs)
#> enframe(x, name = "name", value = "value")
#> frame_data(...)
#> frame_matrix(...)
#> glimpse(x, width = NULL, ...)
#> has_name(x, name)
#> has_rownames(df)
#> is.tibble(x)
#> is_tibble(x)
#> is_vector_s3(x)
#> knit_print.trunc_mat(x, options)
#> lst(...)
#> lst_(xs)
#> obj_sum(x)
#> remove_rownames(df)
#> repair_names(x, prefix = "V", sep)
#> rownames_to_column(df, var = "rowname")
#> tbl_sum(x)
#> tibble(...)
#> tibble_(xs)
#> tribble(...)
#> trunc_mat(x, n = NULL, width = NULL, n_extra = NULL)
#> type_sum(x)
#> 
#> ## S3 methods
#> 
#> as_data_frame.NULL(x, ...)
#> as_data_frame.data.frame(x, validate = TRUE, ...)
#> as_data_frame.default(x, ...)
#> as_data_frame.list(x, validate = TRUE, ...)
#> as_data_frame.matrix(x, ...)
#> as_data_frame.table(x, n = "n", ...)
#> as_data_frame.tbl_df(x, ...)
#> as_tibble.NULL(x, ...)
#> as_tibble.data.frame(x, validate = TRUE, ...)
#> as_tibble.default(x, ...)
#> as_tibble.list(x, validate = TRUE, ...)
#> as_tibble.matrix(x, ...)
#> as_tibble.poly(x, ...)
#> as_tibble.table(x, n = "n", ...)
#> as_tibble.tbl_df(x, ...)
#> as_tibble.ts(x, ...)
#> check_names_before_after.character(j, names)
#> check_names_before_after.default(j, ...)
#> check_names_df.character(j, x)
#> check_names_df.default(j, ...)
#> check_names_df.logical(j, x)
#> check_names_df.numeric(j, x)
#> format_v.character(x)
#> format_v.default(x)
#> format_v.list(x)
#> glimpse.data.frame(x, width = NULL, ...)
#> glimpse.default(x, width = NULL, max.level = 3, ...)
#> glimpse.tbl(x, width = NULL, ...)
#> is_vector_s3.Date(x)
#> is_vector_s3.POSIXct(x)
#> is_vector_s3.data.frame(x)
#> is_vector_s3.default(x)
#> is_vector_s3.difftime(x)
#> is_vector_s3.factor(x)
#> is_vector_s3.ordered(x)
#> obj_sum.POSIXlt(x)
#> obj_sum.default(x)
#> obj_sum.list(x)
#> quote_n.character(x)
#> quote_n.default(x)
#> tbl_sum.default(x)
#> tbl_sum.grouped_df(x)
#> tbl_sum.tbl_df(x)
#> tbl_sum.tbl_sql(x)
#> type_sum.Date(x)
#> type_sum.POSIXt(x)
#> type_sum.data.frame(x)
#> type_sum.default(x)
#> type_sum.difftime(x)
#> type_sum.factor(x)
#> type_sum.ordered(x)
#> type_sum.tbl_df(x)
```

Installation
------------

Install from GitHub via

    # install.packages("remotes")
    remotes::install_github("krlmlr/rapier")
