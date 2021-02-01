context("Scramble")
library(scrambler)

test_that("scramble returns data.frame", {
  expect_s3_class(scramble(test.data), "data.frame")
})

test_that("scramble returns a data.frame that is different from the original", {
  expect_false(identical(scramble(test.data), test.data))
})
