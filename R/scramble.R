#' Scramble a dataset
#'
#' This is the workhorse function of the `scramble` package. It takes
#' a dataset as input and returns a scrambled dataset. The scrambled
#' dataset includes the same columns as the original dataset but these
#' have been resampled in such a way that the associations between
#' columns present in the original dataset are lost.
#'
#' @param original.data Data.frame. The original data. No default.
#' @param only.unique Logical. If TRUE the number of observations in
#'     the returned dataset is equal to the largest number of unique
#'     values across columns. Defaults to FALSE.
#' @param size Numeric or NULL. If not NULL `size` is the number of
#'     observations in the returned dataset. If NULL the number of
#'     observations in the returned dataset will be equal to the
#'     number of observations in the original dataset. Defaults to
#'     NULL.
#' @param check.identical Logical. If TRUE `scramble` checks that no
#'     observation in the original dataset is present in the scrambled
#'     dataset. The probability that this would happen is of course
#'     minimal, but this check is here as an fail-safe. Note that this
#'     operation may take a long time with large datasets, and can
#'     therefore be turned off at your own risk. Defaults to TRUE.
#' @return A `data.frame`.
#' @import assertthat
#' @export
scramble <- function(original.data, only.unique = FALSE, size = NULL, check.identical = TRUE) {
    ## Check arguments
    assert_that(is.data.frame(original.data))
    assert_that(is.logical(only.unique))
    assert_that(is.numeric(size) | is.null(size))
    assert_that(is.logical(check.identical))
    ## Synthesize data
    unique.values <- lapply(original.data, function(column) as.character(unique(column)))
    max.unique.n <- max(sapply(unique.values, length))
    scrambled.data <- synthesize_data(original.data,
                                      only.unique = only.unique,
                                      unique.values = unique.values,
                                      max.unique.n = max.unique.n)
    ## Scramble data
    scrambled.data[] <- lapply(scrambled.data, scramble_column)
    ## Modify size
    if (!is.null(size)) {
        if (size > nrow(scrambled.data)) {
            message("size is larger than the number of observations in the data, draws will be made with replacement")
            scrambled.data <- rbind(scrambled.data,
                                    scrambled.data[sample(1:nrow(scrambled.data),
                                                          size - nrow(scrambled.data),
                                                          replace = TRUE)])
        }
    }
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

synthesize_data <- function(original.data, only.unique, unique.values, max.unique.n) {
    n <- nrow(original.data)
    if (only.unique)
        n <- max.unique.n
    classes <- lapply(original.data, class)
    synthesized.data <- as.data.frame(do.call(cbind, lapply(unique.values, rep, length.out = n)))
    synthesized.data[] <- lapply(seq_along(classes), function(i) {
        match.fun(paste0("as.", classes[[i]]))(synthesized.data[, i])
    })
    synthesized.data
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
