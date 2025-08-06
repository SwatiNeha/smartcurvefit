test_that("fit_model works for normal input", {
  x <- 1:10
  y <- 2 * x^1.5 + rnorm(10)
  fit <- fit_model(x, y)
  expect_s3_class(fit, "smartFit")
  expect_length(fit$coefficients, 2)
})

test_that("fit_model errors for non-numeric input", {
  expect_error(fit_model("a", "b"), "numeric")
})

test_that("fit_model errors for length mismatch", {
  expect_error(fit_model(1:3, 1:2), "same length")
})

test_that("fit_model errors for NAs in x or y", {
  expect_error(fit_model(c(1, NA), c(2, 3)), "Missing values")
})

test_that("fit_model errors for too few data points", {
  expect_error(fit_model(1, 2), "at least two data points")
})

test_that("fit_model errors for constant x", {
  x <- rep(5, 10)
  y <- 1:10
  expect_error(fit_model(x, y), "vary")
})

test_that("fit_model errors for constant y", {
  x <- 1:10
  y <- rep(5, 10)
  expect_error(fit_model(x, y), "vary")
})

test_that("fit_model errors for unsupported model type", {
  x <- 1:10
  y <- 1:10
  expect_error(fit_model(x, y, model_type = "unknown"), "Unsupported model type")
})
