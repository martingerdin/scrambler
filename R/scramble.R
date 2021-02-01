#' Scramble a dataset
#'
#' This is the workhorse function of the `scramble` package. It takes a dataset
#' as input and returns a scrambled dataset. The scrambled dataset includes the
#' same columns as the original dataset but these have been resampled in such a
#' way that the associations between columns present in the original dataset
#' are lost.
#' @param data Data.frame. The original data. No default.
#' @import assertthat
#' @export
scramble <- function(data) {
  ## Check arguments
  assert_that(is.data.frame(data))
  ## Scramble data
  scrambled.data <- data
  scrambled.data[] <- lapply(data, scramble_column)
  ## Return
  scrambled.data
}

scramble_column <- function(column) {
  n <- length(column)
  scrambled.column <- sample(column, n)
  scrambled.column
}
