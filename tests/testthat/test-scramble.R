context("Scramble")
library(scrambler)

test_that("scramble returns data.frame", {
  expect_s3_class(scramble(test.data, check.identical = FALSE), "data.frame")
})

test_that("scramble returns a data.frame that is different from the original", {
  expect_false(identical(scramble(test.data, check.identical = FALSE), test.data))
})

test_that("any_identical correctly detects remaining original observation", {
    original.data <- test.data[1:10,]
    scrambled.data <- original.data
    scrambled.data[] <- lapply(original.data, scramble_column)
    scrambled.data[10, ] <- original.data[10, ]
    expect_error(any_identical(original.data, scrambled.data),
                 "Observations from the original data still exist in the scrambled data, try again.")
})

rm(list = ls())
