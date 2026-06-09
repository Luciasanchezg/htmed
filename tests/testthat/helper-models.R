# Common functions for testing models
expect_models_equal <- function(actual, expected) {
  expect_equal(lapply(actual, coef),   lapply(expected, coef))
  expect_equal(lapply(actual, fitted), lapply(expected, fitted))
}
