## Create test data
n <- 10000
test.data <- data.frame(a = rnorm(n),
                        b = rnorm(n),
                        c = as.factor(sample(LETTERS, n, replace = TRUE)),
                        d = as.character(
                          sample(unlist(lapply(combn(LETTERS, 2,
                                                     simplify = FALSE),
                                               paste0,
                                               collapse = "")),
                                 n, replace = TRUE)))
usethis::use_data(test.data, overwrite = TRUE)
