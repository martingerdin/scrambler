#' Scramble a dataset
#'
#' This is the workhorse function of the `scramble` package. It takes
#' a dataset as input and returns a scrambled dataset. The scrambled
#' dataset includes the same columns as the original dataset but these
#' have been resampled in such a way that the associations between
#' columns present in the original dataset are lost.
#' @param data Data.frame. The original data. No default.
#' @param check.identical Logical. If TRUE `scramble` checks that no
#'     observation in the original dataset is present in the scrambled
#'     dataset. The probability that this would happen is of course
#'     minimal, but this check is here as an fail-safe. Note that this
#'     operation may take a long time with large datasets, and can
#'     therefore be turned off at your own risk. Defaults to TRUE.
#' @import assertthat
#' @export
scramble <- function(data, check.identical = TRUE) {
  ## Check arguments
  assert_that(is.data.frame(data))
  ## Scramble data
  scrambled.data <- data
  scrambled.data[] <- lapply(data, scramble_column)
  ## Check that no observation is kept unchanged
  if (check.identical) {
    Hmisc::find.matches(scrambled.data, data)
  }

  ## Return
  scrambled.data
}

scramble_column <- function(column) {
  n <- length(column)
  scrambled.column <- sample(column, n)
  scrambled.column
}
