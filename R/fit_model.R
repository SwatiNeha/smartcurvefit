#' Fit with cross-validated loss selection (C++ CV)
#'
#' Runs 5-fold cross-validation in C++ to compare L1 (MAE-trained) vs L2 (MSE-trained)
#' for the chosen model, selects the winner by mean CV error ("rmse" or "mae"),
#' refits on the full data with the winning loss, and returns a `smartFit`.
#'
#' @param x Numeric predictor
#' @param y Numeric response
#' @param model_type "power_law" or "exponential". If omitted/NULL, warns and defaults to "power_law".
#' @param metric Selection metric across folds: "rmse" (default) or "mae"
#' @return S3 object of class 'smartFit'
#' @useDynLib smartcurvefit, .registration = TRUE
#' @importFrom Rcpp evalCpp
#' @export
fit_model <- function(x, y, model_type = NULL, metric = c("rmse","mae")) {
  metric <- match.arg(metric)

  # front-end checks + defaulting
  if (!is.numeric(x) || !is.numeric(y))
    stop("Error: x and y must be numeric vectors.")
  if (length(x) != length(y))
    stop("Error: x and y must have the same length.")
  if (any(is.na(x)) || any(is.na(y)))
    stop("Error: Missing values detected in x or y. Please remove or impute NAs.")
  if (length(x) < 3)
    stop("Error: Need at least three points for cross-validation.")
  if (sd(x) == 0 || sd(y) == 0)
    stop("Error: x and y must vary (not be constant) to fit a model.")

  if (is.null(model_type)) {
    warning("Warning: No model selected; defaulting to 'power_law'.")
    model_type <- "power_law"
  } else if (!model_type %in% c("power_law","exponential")) {
    stop("Error: Unsupported model type. Use 'power_law' or 'exponential'.")
  }
  if (model_type == "power_law" && any(x <= 0))
    stop("Error: For power_law, all x values must be strictly positive (> 0).")
  if (model_type == "power_law" && any(x < 1))
    warning("Warning: Some x values are less than 1; power-law fits may be unstable.")

  out <- fit_model_cv_cpp(x, y, model_type, metric)

  structure(list(
    model            = out$model,
    chosen_loss      = out$chosen_loss,
    coefficients     = out$coefficients,
    fitted.values    = out$fitted.values,
    rmse             = out$rmse,
    mae              = out$mae,
    mse              = out$mse,
    selection_metric = out$selection_metric,
    selection_score  = out$selection_score,
    cv_results       = out$cv_results,
    all_results      = out$all_results
  ), class = "smartFit")
}

#' @export
print.smartFit <- function(x, ...) {
  cat(sprintf("smartCurveFit — Model: %s | Selected by %s: %s (score = %g)\n",
              x$model, x$selection_metric, x$chosen_loss, x$selection_score))
  cat("Coefficients:\n"); print(x$coefficients)
  if (!is.null(x$rmse))
    cat(sprintf("Full-data metrics — RMSE: %.6g   MAE: %.6g   MSE: %.6g\n",
                x$rmse, x$mae, x$mse))

  if (!is.null(x$cv_results)) {
    cat("\nCross-validated errors (mean ± sd across folds):\n")
    cr <- x$cv_results
    fmt <- function(m, s) sprintf("%.6g ± %.6g", m, s)
    show <- data.frame(
      loss = cr$loss,
      `CV-RMSE` = c(fmt(cr$mean_rmse[1], cr$sd_rmse[1]),
                    fmt(cr$mean_rmse[2], cr$sd_rmse[2])),
      `CV-MAE`  = c(fmt(cr$mean_mae[1],  cr$sd_mae[1]),
                    fmt(cr$mean_mae[2],  cr$sd_mae[2]))
    )
    print(show, row.names = FALSE)
  }

  if (!is.null(x$all_results)) {
    cat("\nFull-data refit (both losses):\n")
    print(x$all_results)
  }
  invisible(x)
}
