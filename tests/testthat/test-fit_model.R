test_that("fit_model works correctly for power_law", {
  set.seed(42)
  x <- 1:30
  y <- 2 * x^1.5 + rnorm(length(x))
  fit <- fit_model(x, y, "power_law", metric = "rmse")

  # ---- structure ----
  expect_s3_class(fit, "smartFit")
  expect_equal(fit$model, "power_law")
  expect_length(fit$coefficients, 2)

  # ---- residuals ----
  expect_true(all(is.finite(fit$residuals)))
  expect_equal(length(fit$residuals), length(x))

  # ---- metrics ----
  expect_true(is.numeric(fit$rmse) && is.finite(fit$rmse) && fit$rmse >= 0)
  expect_true(is.numeric(fit$mae)  && is.finite(fit$mae)  && fit$mae  >= 0)
  expect_true(is.numeric(fit$mse)  && is.finite(fit$mse)  && fit$mse  >= 0)

  # ---- CV results ----
  expect_s3_class(fit$cv_results, "data.frame")
  expect_setequal(names(fit$cv_results),
                  c("loss","mean_rmse","sd_rmse","mean_mae","sd_mae"))
  expect_true(all(is.finite(unlist(fit$cv_results[, -1]))))

  # ---- all_results ----
  expect_s3_class(fit$all_results, "data.frame")
  expect_setequal(names(fit$all_results),
                  c("loss","a","b","mse","rmse","mae"))
  expect_true(all(is.finite(unlist(fit$all_results[, -1]))))

  # ---- selection logic ----
  idx_rmse_min <- which.min(fit$cv_results$mean_rmse)
  expect_equal(fit$selection_score, fit$cv_results$mean_rmse[idx_rmse_min])
  expect_equal(fit$chosen_loss, as.character(fit$cv_results$loss[idx_rmse_min]))

  # CV-MAE path
  fit_mae <- fit_model(x, y, "power_law", metric = "mae")
  idx_mae_min <- which.min(fit_mae$cv_results$mean_mae)
  expect_equal(fit_mae$selection_score, fit_mae$cv_results$mean_mae[idx_mae_min])
  expect_equal(fit_mae$chosen_loss, as.character(fit_mae$cv_results$loss[idx_mae_min]))
})


test_that("fit_model works correctly for exponential", {
  set.seed(123)
  x <- 1:30
  y <- 3 * exp(0.9 * x) + rnorm(length(x))
  fit <- fit_model(x, y, "exponential", metric = "rmse")

  # ---- structure ----
  expect_s3_class(fit, "smartFit")
  expect_equal(fit$model, "exponential")
  expect_length(fit$coefficients, 2)

  # ---- residuals ----
  expect_true(all(is.finite(fit$residuals)))
  expect_equal(length(fit$residuals), length(x))

  # ---- metrics ----
  expect_true(is.numeric(fit$rmse) && is.finite(fit$rmse) && fit$rmse >= 0)
  expect_true(is.numeric(fit$mae)  && is.finite(fit$mae)  && fit$mae  >= 0)
  expect_true(is.numeric(fit$mse)  && is.finite(fit$mse)  && fit$mse  >= 0)

  # ---- CV results ----
  expect_s3_class(fit$cv_results, "data.frame")
  expect_setequal(names(fit$cv_results),
                  c("loss","mean_rmse","sd_rmse","mean_mae","sd_mae"))

  # ---- all_results ----
  expect_s3_class(fit$all_results, "data.frame")
  expect_setequal(names(fit$all_results),
                  c("loss","a","b","mse","rmse","mae"))

  # ---- selection logic ----
  idx_rmse_min <- which.min(fit$cv_results$mean_rmse)
  expect_equal(fit$selection_score, fit$cv_results$mean_rmse[idx_rmse_min])
  expect_equal(fit$chosen_loss, as.character(fit$cv_results$loss[idx_rmse_min]))

  # CV-MAE path
  fit_mae <- fit_model(x, y, "exponential", metric = "mae")
  idx_mae_min <- which.min(fit_mae$cv_results$mean_mae)
  expect_equal(fit_mae$selection_score, fit_mae$cv_results$mean_mae[idx_mae_min])
  expect_equal(fit_mae$chosen_loss, as.character(fit_mae$cv_results$loss[idx_mae_min]))

})

test_that("fit_model works correctly for logarithmic", {
  set.seed(321)
  x <- 1:50
  y <- 3 + 2*log(x) + rnorm(length(x))
  fit <- fit_model(x, y, "logarithmic", metric = "rmse")

  # ---- structure ----
  expect_s3_class(fit, "smartFit")
  expect_equal(fit$model, "logarithmic")
  expect_length(fit$coefficients, 2)

  # ---- residuals ----
  expect_true(all(is.finite(fit$residuals)))
  expect_equal(length(fit$residuals), length(x))

  # ---- metrics ----
  expect_true(is.numeric(fit$rmse) && is.finite(fit$rmse) && fit$rmse >= 0)
  expect_true(is.numeric(fit$mae)  && is.finite(fit$mae)  && fit$mae  >= 0)
  expect_true(is.numeric(fit$mse)  && is.finite(fit$mse)  && fit$mse  >= 0)

  # ---- CV results ----
  expect_s3_class(fit$cv_results, "data.frame")
  expect_setequal(names(fit$cv_results),
                  c("loss","mean_rmse","sd_rmse","mean_mae","sd_mae"))

  # ---- all_results ----
  expect_s3_class(fit$all_results, "data.frame")
  expect_setequal(names(fit$all_results),
                  c("loss","a","b","mse","rmse","mae"))

  # ---- selection logic ----
  idx_rmse_min <- which.min(fit$cv_results$mean_rmse)
  expect_equal(fit$selection_score, fit$cv_results$mean_rmse[idx_rmse_min])
  expect_equal(fit$chosen_loss, as.character(fit$cv_results$loss[idx_rmse_min]))

  # CV-MAE path
  fit_mae <- fit_model(x, y, "logarithmic", metric = "mae")
  idx_mae_min <- which.min(fit_mae$cv_results$mean_mae)
  expect_equal(fit_mae$selection_score, fit_mae$cv_results$mean_mae[idx_mae_min])
  expect_equal(fit_mae$chosen_loss, as.character(fit_mae$cv_results$loss[idx_mae_min]))
})

test_that("input validation works", {
  e <- expect_error(fit_model("a", "b"), "numeric")              # Non-numeric
  print(e$message)

  e <- expect_error(fit_model(1:3, 1:2), "same length")          # Length mismatch
  print(e$message)

  e <- expect_error(fit_model(c(1, NA), c(2, 3)), "Missing")     # NA in x
  print(e$message)

  e <- expect_error(fit_model(1:3, c(1, NA, 2)), "Missing")      # NA in y
  print(e$message)

  e <- expect_error(fit_model(c(1, NaN), c(2, 3)), "Missing")    # NaN in x
  print(e$message)

  e <- expect_error(fit_model(1:3, c(1, NaN, 2)), "Missing")     # NaN in y
  print(e$message)

  e <- expect_error(fit_model(c(1, Inf, 3), c(2, 3, 4)), "Infinite") # Inf in x
  print(e$message)

  e <- expect_error(fit_model(1:3, c(1, 2, Inf)), "Infinite")        # Inf in y
  print(e$message)

  e <- expect_error(fit_model(1, 2), "three points")             # Too few points
  print(e$message)

  e <- expect_error(fit_model(1:10, 1:10, model_type = "foo"), "Unsupported") # Bad model
  print(e$message)

  e <- expect_error(fit_model(rep(5, 10), 1:10), "vary")         # Constant x
  print(e$message)

  e <- expect_error(fit_model(1:10, rep(5, 10)), "vary")         # Constant y
  print(e$message)

  # ---- invalid arguments ----
  w <- capture_warnings(fit_model(1:10, 1:10, foo = "bar"))
  expect_true(any(grepl("Unused arguments", w)))
  expect_true(any(grepl("defaulting to 'power_law'", w)))

  # ---- extreme warnings ----
  w <- expect_warning(fit_model(c(1e120, 2e120, 3e120), c(1,2,3), "exponential"),
                      "overflow")
  print(w$message)

  w <- expect_warning(fit_model(c(1e-120, 2e-120, 3e-120), c(1,2,3), "exponential"),
                      "underflow")
  print(w$message)

  ## ---------- Precision input ----------
  expect_s3_class(
    fit_model(c(1.23456789012345678901, 2, 3), c(1, 2, 3), "exponential"),
    "smartFit"
  )

  ## ---------- Large dataset warning ----------
  w <- expect_warning(
    .check_input_size_fit(1:10, 1:10, n_threshold = 5),
    "Large dataset"
  )
  print(w$message)

  expect_silent(.check_input_size_fit(1:3, 1:3, n_threshold = 10))

  ## ---------- No model provided ----------
  set.seed(1234)
  x <- 1:10; y <- 2 * x + rnorm(10)
  w <- expect_warning(
    fit_model(x, y),   # no model_type given
    "defaulting to 'power_law'"
  )
  print(w$message)


  # ---- domain rules power law ----
  e <- expect_error(fit_model(c(0,1,2), c(1,2,3), "power_law"), "strictly positive")
  print(e$message)

  e <- expect_error(fit_model(c(-1,2,3), c(1,2,3), "power_law"), "strictly positive")
  print(e$message)

  w <- expect_warning(fit_model(c(0.5,1.2,2), c(2,4,6), "power_law"), "unstable")
  print(w$message)


  # ---- domain rules logarithmic ----
  e <- expect_error(fit_model(c(0,1,2), c(1,2,3), "logarithmic"), "strictly positive")
  print(e$message)

  e <- expect_error(fit_model(c(-1,2,3), c(1,2,3), "logarithmic"), "strictly positive")
  print(e$message)
})

# ---- Print methods ----
test_that("print.smartFit behaves correctly", {
  set.seed(1)
  x <- 1:20; y <- 2 * x + rnorm(20)
  fit <- fit_model(x, y, "power_law")

  # returns invisibly
  expect_invisible(print(fit))

  # warns on unused args
  w <- expect_warning(print(fit, foo = "bar"), "Unused")
  cat(w$message, "\n")

  # wrong class
  bad_obj <- list(a = 1, b = 2)
  w <- expect_error(print.smartFit(bad_obj), "can only be used on objects of class 'smartFit'")
  cat(w$message, "\n")

  # missing required fields
  bad_obj <- list(model = "power_law")
  class(bad_obj) <- "smartFit"
  w <- expect_error(print.smartFit(bad_obj), "missing field")
  cat(w$message, "\n")
})


# ---- Summary methods ----
test_that("summary.smartFit behaves correctly", {
  set.seed(1)
  x <- 1:20; y <- 2 * x + rnorm(20)
  fit <- fit_model(x, y, "power_law")

  # returns invisibly
  expect_invisible(summary(fit))

  # warns on invalid args
  w <- expect_warning(summary(fit, bar = 123), "Invalid")
  cat(w$message, "\n")

  # wrong class
  bad_obj <- list(model = "power_law")
  w <- expect_error(summary.smartFit(bad_obj), "can only be used on objects of class 'smartFit'")
  cat(w$message, "\n")

  # missing coefficients
  bad_obj <- list(model = "power_law", chosen_loss = "L1")
  class(bad_obj) <- "smartFit"
  w <- expect_error(summary.smartFit(bad_obj), "missing field")
  cat(w$message, "\n")
})


test_that("plot.smartFit behaves correctly", {
  set.seed(1)
  x <- 1:20; y <- 2 * x + rnorm(20)
  fit <- fit_model(x, y, "power_law")

  # wrong class
  bad_obj <- list(x = 1:10, y = 1:10)
  w <- expect_error(plot.smartFit(bad_obj), "can only be used on objects of class 'smartFit'")
  cat(w$message, "\n")

  # valid plotting calls (silently succeed)
  expect_invisible(plot(fit))
  expect_invisible(plot(fit, show_residuals = TRUE))
  expect_invisible(plot(fit, col = "blue", pch = 17))

  # warns on invalid graphical args
  w <- expect_warning(plot(fit, badarg = 99), "Invalid graphical")
  cat(w$message, "\n")
})


test_that("predict.smartFit behaves correctly", {
  set.seed(99)
  x <- 1:30
  y <- 2 * x^1.2 + rnorm(length(x))
  fit <- fit_model(x, y, "power_law")

  # ---- predict on training data ----
  preds_train <- predict(fit)
  expect_equal(length(preds_train), length(x))
  expect_true(is.numeric(preds_train))

  # ---- predict on new data ----
  newx <- seq(1, 40, by = 2)
  preds_new <- predict(fit, newdata = newx)
  expect_equal(length(preds_new), length(newx))
  expect_true(all(is.finite(preds_new)))

  # ---- unused arguments trigger warning ----
  w <- expect_warning(predict(fit, newdata = 1:5, foo = "bar"), "Unused")
  cat(w$message, "\n")

  # ---- invalid newdata (non-numeric) ----
  e <- expect_error(predict(fit, newdata = letters[1:3]), "numeric")
  cat(e$message, "\n")

  # ---- empty newdata ----
  e <- expect_error(predict(fit, newdata = numeric(0)), "length >= 1")
  cat(e$message, "\n")

  # ---- newdata with NA ----
  e <- expect_error(predict(fit, newdata = c(1, NA, 3)), "NA/NaN")
  cat(e$message, "\n")

  # ---- newdata with NaN ----
  e <- expect_error(predict(fit, newdata = c(1, NaN, 3)), "NA/NaN")
  cat(e$message, "\n")

  # ---- newdata with Inf ----
  e <- expect_error(predict(fit, newdata = c(1, Inf, 3)), "infinite")
  cat(e$message, "\n")

  # ---- domain rules ----
  e <- expect_error(predict(fit, newdata = c(-1, 0, 2)), "strictly positive")
  cat(e$message, "\n")

  # ---- large dataset warning ----
  w <- expect_warning(
    .check_input_size_predict(1:10, n_threshold = 5),
    "Large prediction dataset"
  )
  cat(w$message, "\n")

  # Silent when under threshold
  expect_silent(.check_input_size_predict(1:3, n_threshold = 10))

  # ---- wrong object class ----
  bad_obj <- list(model = "power_law", coefficients = c(a = 2, b = 1))
  e <- expect_error(predict.smartFit(bad_obj, 1:5), "can only be used on objects of class 'smartFit'")
  cat(e$message, "\n")

  # ---- missing required fields ----
  bad_obj <- list(model = "power_law")
  class(bad_obj) <- "smartFit"
  e <- expect_error(predict.smartFit(bad_obj, 1:5), "missing field")
  cat(e$message, "\n")
})

