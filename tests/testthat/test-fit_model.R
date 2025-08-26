test_that("fit_model (CV) works and validates inputs", {
  ## ---------- Normal fit (power_law, select by CV-RMSE) ----------
  set.seed(42)
  x <- 1:30
  y <- 2 * x^1.5 + rnorm(length(x))

  fit <- fit_model(x, y, model_type = "power_law", metric = "rmse")

  # structure
  expect_s3_class(fit, "smartFit")
  expect_named(
    fit,
    c("model","chosen_loss","coefficients","fitted.values",
      "rmse","mae","mse",
      "selection_metric","selection_score",
      "cv_results","all_results"),
    ignore.order = TRUE
  )

  # coefficients
  expect_length(fit$coefficients, 2)
  expect_type(fit$coefficients, "double")
  expect_true(all(is.finite(unname(fit$coefficients))))

  # fitted values
  expect_type(fit$fitted.values, "double")
  expect_length(fit$fitted.values, length(x))
  expect_true(all(is.finite(fit$fitted.values)))

  # metrics
  expect_true(is.numeric(fit$rmse) && is.finite(fit$rmse) && fit$rmse >= 0)
  expect_true(is.numeric(fit$mae)  && is.finite(fit$mae)  && fit$mae  >= 0)
  expect_true(is.numeric(fit$mse)  && is.finite(fit$mse)  && fit$mse  >= 0)

  # cv_results table: two rows (L1, L2) with expected columns
  expect_s3_class(fit$cv_results, "data.frame")
  expect_equal(nrow(fit$cv_results), 2L)
  expect_setequal(
    names(fit$cv_results),
    c("loss","mean_rmse","sd_rmse","mean_mae","sd_mae")
  )
  expect_setequal(fit$cv_results$loss, c("L1","L2"))
  expect_true(all(is.finite(unlist(fit$cv_results[, -1]))))

  # all_results table: full-data refit of both losses
  expect_s3_class(fit$all_results, "data.frame")
  expect_equal(nrow(fit$all_results), 2L)
  expect_setequal(names(fit$all_results), c("loss","a","b","mse","rmse","mae"))
  expect_setequal(fit$all_results$loss, c("L1","L2"))
  expect_true(all(is.finite(unlist(fit$all_results[, -1]))))

  # selection logic consistent with CV metric (RMSE)
  idx_rmse_min <- which.min(fit$cv_results$mean_rmse)
  expect_equal(fit$selection_score, fit$cv_results$mean_rmse[idx_rmse_min])
  expect_equal(fit$chosen_loss, fit$cv_results$loss[idx_rmse_min])
  expect_match(fit$selection_metric, "CV-RMSE", ignore.case = TRUE)

  ## ---------- Select by CV-MAE path ----------
  fit_mae <- fit_model(x, y, model_type = "power_law", metric = "mae")
  idx_mae_min <- which.min(fit_mae$cv_results$mean_mae)
  expect_equal(fit_mae$selection_score, fit_mae$cv_results$mean_mae[idx_mae_min])
  expect_equal(fit_mae$chosen_loss, fit_mae$cv_results$loss[idx_mae_min])
  expect_match(fit_mae$selection_metric, "CV-MAE", ignore.case = TRUE)

  ## ---------- Core input validation ----------
  # Non-numeric input
  expect_error(fit_model("a", "b"), "numeric")

  # Length mismatch
  expect_error(fit_model(1:3, 1:2), "same length")

  # Missing values
  expect_error(fit_model(c(1, NA), c(2, 3)), "Missing values")

  # Too few points for CV
  expect_error(fit_model(1, 2), "three points")

  # Unsupported model type
  expect_error(fit_model(1:10, 1:10, model_type = "unknown"), "Unsupported model type")

  # Constant x
  xx <- rep(5, 10); yy <- 1:10
  expect_error(fit_model(xx, yy), "vary")

  # Constant y
  xx <- 1:10; yy <- rep(5, 10)
  expect_error(fit_model(xx, yy), "vary")

  # Power law domain: non-positive x
  expect_error(fit_model(c(1, 2, 0, 4), c(1, 2, 3, 4), model_type = "power_law"), "strictly positive")
  expect_error(fit_model(c(1, -2, 3, 4), c(1, 2, 3, 4), model_type = "power_law"), "strictly positive")

  # Power law warning for small x values (x < 1 but > 0)
  expect_warning(fit_model(c(0.5, 1.2, 2), c(2, 4, 6), model_type = "power_law"), "unstable")
})
