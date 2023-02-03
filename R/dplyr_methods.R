#' Create a df_list list
#'
#' @param l a list of data frames
#'
#' @return a list of data frames with class df_list
#' @export
#'
#' @examples
#' as_df_list(list(a = mtcars, b = mtcars))
as_df_list <- function(l) {
  if (is.data.frame(l)) {
    return(structure(list(df = l), class = "df_list"))
  }
  if (is.list(l) && all(sapply(l, is.data.frame))) {
    return(structure(l, class = "df_list"))
  }
  stop("l must be a data frame or list of data frames.")
}

#' Filter data frames in a list
#'
#' See \link[dplyr]{filter}
#'
#' @inheritParams dplyr::filter
#' @importFrom dplyr filter
#'
#' @return A list of filtered data frames
#' @export
#'
#' @examples
#' library(dplyr)
#' filter(
#'   as_df_list(list(a = mtcars, b = mtcars)),
#'   cyl == 4
#' )
filter.df_list <- function(.data, ...) {
  as_df_list(lapply(.data, filter, ...))
}

#' Modify or create columns for data frames in a list
#'
#' See \link[dplyr]{mutate}
#'
#' @inheritParams dplyr::mutate
#' @importFrom dplyr mutate
#'
#' @return A list of mutated data frames
#' @export
#'
#' @examples
#' library(dplyr)
#' mutate(
#'   as_df_list(list(a = mtcars, b = mtcars)),
#'   lp100km = 235.214538 / mpg
#' )
mutate.df_list <- function(.data, ...) {
  as_df_list(lapply(.data, mutate, ...))
}

#' @rdname mutate.df_list
#' @importFrom dplyr transmute
#' @export
#' @examples
#' library(dplyr)
#' transmute(
#'   as_df_list(list(a = mtcars, b = mtcars)),
#'   cyl, mpg, lp100km = 235.214538 / mpg
#' )
transmute.df_list <- function(.data, ...) {
  as_df_list(lapply(.data, transmute, ...))
}

#' Select columns from data frames in a list
#'
#' See \link[dplyr]{select}
#'
#' @inheritParams dplyr::select
#' @importFrom dplyr select
#'
#' @return A list of data frames with the selected columns
#' @export
#'
#' @examples
#' library(dplyr)
#' select(
#'   as_df_list(list(a = mtcars, b = mtcars)),
#'   cyl, mpg
#' )
select.df_list <- function(.data, ...) {
  as_df_list(lapply(.data, select, ...))
}

#' Rename columns of data frames in a list
#'
#' See \link[dplyr]{rename}
#'
#' @inheritParams dplyr::rename
#' @importFrom dplyr rename
#'
#' @return A list of data frames with renamed columns
#' @export
#'
#' @examples
#' library(dplyr)
#' rename(
#'   as_df_list(list(a = mtcars, b = mtcars)),
#'   num_cyl = cyl, horsepower = hp
#' )
rename.df_list <- function(.data, ...) {
  as_df_list(lapply(.data, rename, ...))
}
