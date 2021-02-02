#' Scramble a dataset
#'
#' This is the workhorse function of the `scramble` package. It takes
#' a dataset as input and returns a scrambled dataset. The scrambled
#' dataset includes the same columns as the original dataset but these
#' have been resampled in such a way that the associations between
#' columns present in the original dataset are lost.
#'
#' `synthesize` makes `scramble` create a syntethic dataset that
#' includes the unique values of each column in the original dataset
#' but in a completely random order. 
#' 
#' @param original.data Data.frame. The original data. No default.
#' @param check.identical Logical. If TRUE `scramble` checks that no
#'     observation in the original dataset is present in the scrambled
#'     dataset. The probability that this would happen is of course
#'     minimal, but this check is here as an fail-safe. Note that this
#'     operation may take a long time with large datasets, and can
#'     therefore be turned off at your own risk. Defaults to TRUE.
#' @param synthesize Logical. If TRUE new data is generated using
#'     unique values present in the dataset. See
#' @return A `data.frame`.
#' @import assertthat
#' @export
scramble <- function(original.data, check.identical = TRUE, synthesize = FALSE ) {
    ## Check arguments
    assert_that(is.data.frame(original.data))
    ## Scramble data
    scrambled.data <- original.data
    scrambled.data[] <- lapply(original.data, scramble_column)
    ## Check that no observation is remains unchanged
    if (check.identical)
        any_identical(original.data, scrambled.data)
    ## Return
    scrambled.data
}

scramble_column <- function(column) {
    n <- length(column)
    scrambled.column <- sample(column, n)
    scrambled.column
}

any_identical <- function(original.data, scrambled.data) {
    matches <- apply(original.data, 1, function(original.row) {
        apply(scrambled.data, 1, function(scrambled.row) {
            if (all(original.row == scrambled.row))
                stop ("Observations from the original data still exist in the scrambled data, try again.")
        })
    })
    invisible(FALSE)
}
