#' Fit Nonlinear Models with Cross-Validated Loss Selection
#'
#' Fits nonlinear models of type **power-law**, **exponential**, or **logarithmic**
#' to paired data `(x,y)` using an efficient C++ backend. The function automatically
#' estimates the model coefficients `a` and `b`; only the data needs to be provided.
#'
#' Functional forms:
#' - **Power law:**     y ~ a * x^b
#' - **Exponential:**   y ~ a * exp(b * x)
#' - **Logarithmic:**   y ~ a + b * log(x)
#'
#' Model fitting is robustified via 5-fold cross-validation comparing
#' L1 (MAE), L2 (MSE), and Huber losses. The best loss is chosen according
#' to the selection metric (`"rmse"` (default) or `"mae"`), and the model is refit on the
#' full data under that loss.
#'
#' @details
#' The function performs extensive input validation:
#' - `x` and `y` must be numeric vectors of equal length with at least 3 points.
#' - Missing (`NA`/`NaN`) or infinite values are not allowed.
#' - For `"power_law"` and `"logarithmic"`, `x` must be strictly positive.
#' - Warnings may be issued for:
#'   - unstable fits when `x < 1` in a power-law model
#'   - very large or small values that risk overflow/underflow
#'   - datasets exceeding recommended size (performance warning)
#'   - omitting `model_type` (defaults to `"power_law"`)
#'   - unused arguments passed via `...`
#'
#' @param x Numeric predictor vector.
#' @param y Numeric response vector.
#' @param model_type Character string, one of `"power_law"`, `"exponential"`,
#'   or `"logarithmic"`. If omitted or NULL, defaults to `"power_law"`.
#' @param metric Selection criterion across CV folds: `"rmse"` (default) or `"mae"`.
#' @param ... Currently unused. Included for forward/backward compatibility with
#'   other generics. Supplying extra arguments will trigger a warning.
#'
#' @return
#' An S3 object of class `"smartFit"`, containing:
#' - estimated coefficients `a` and `b`
#' - fitted values and residuals
#' - cross-validated errors for each loss
#' - selection results (chosen loss + score)
#' - full-data refits under all losses
#'
#' @examples
#' set.seed(42)
#' x <- 1:30
#'
#' # --- Power-law example ---
#' y_power <- 2.3 + x^0.9 + rnorm(length(x))
#' fit_power <- fit_model(x, y_power, model_type = "power_law")
#' print(fit_power)
#' summary(fit_power)
#' plot(fit_power, show_residuals = TRUE)
#'
#' # Predict on new data
#' predict(fit_power, newdata = c(5, 10, 15))
#'
#' # --- Exponential example ---
#' y_exp <- 2.6 + exp(0.08 * x) + rnorm(length(x))
#' fit_exp <- fit_model(x, y_exp, model_type = "exponential")
#' summary(fit_exp)
#' plot(fit_exp)
#'
#' # Predict on new data
#' predict(fit_exp, newdata = c(1, 2, 3))
#'
#' # --- Logarithmic example ---
#' y_log <- 3 + 2 * log(x) + rnorm(length(x))
#' fit_log <- fit_model(x, y_log, model_type = "logarithmic")
#' summary(fit_log)
#' plot(fit_log, col_points = "darkgreen")
#'
#' # Predict on new data
#' predict(fit_log, newdata = c(2, 5, 10))
#'
#' @useDynLib smartcurvefit, .registration = TRUE
#' @importFrom Rcpp evalCpp
#' @importFrom stats sd
#' @export
fit_model <- function(x, y, model_type = NULL, metric = "rmse", ...) {

  old_opts <- options(warn = 1)
  on.exit(options(old_opts), add = TRUE)

  if (!metric %in% c("rmse", "mae")) {
    stop("Error: metric must be either 'rmse' or 'mae'.")
  }

  # ---- unused args check ----
  dots <- list(...)
  if (length(dots) > 0) {
    warning("Warning: Unused arguments ignored in fit_model: ",
            paste(names(dots), collapse = ", "))
  }

  # ---- basic checks ----
  if (!is.numeric(x) || !is.numeric(y))
    stop("Error: x and y must be numeric vectors.")
  if (length(x) != length(y))
    stop("Error: x and y must have the same length.")

  # missing/invalid values
  if (any(is.na(x)) || any(is.na(y)))
    stop("Error: Missing values detected in x or y (NA/NaN). Please remove or impute.")
  if (any(is.infinite(x)) || any(is.infinite(y)))
    stop("Error: Infinite values detected in x or y. Please clean your data.")

  # vector length constraints
  if (length(x) < 3)
    stop("Error: Need at least three points for cross-validation.")

  .check_input_size_fit(x, y)

  # variance checks
  if (sd(x) == 0 || sd(y) == 0)
    stop("Error: x and y must vary (not be constant) to fit a model.")

  # magnitude checks (overflow/underflow risk)
  if (any(abs(x) > 1e100) || any(abs(y) > 1e100))
    warning("Warning: Extremely large values detected; may cause overflow.")
  if (any(abs(x) < 1e-100 & x != 0) || any(abs(y) < 1e-100 & y != 0))
    warning("Warning: Extremely small values detected; may cause underflow to zero.")

  # ---- model type validation ----
  if (is.null(model_type)) {
    warning("Warning: No model selected; defaulting to 'power_law'.")
    model_type <- "power_law"
  } else if (!model_type %in% c("power_law","exponential","logarithmic")) {
    stop("Error: Unsupported model type. Use 'power_law', 'exponential', or 'logarithmic'.")
  }
  if (model_type %in% c("power_law","logarithmic") && any(x <= 0))
    stop("Error: For power_law/logarithmic, all x values must be strictly positive (> 0).")
  if (model_type == "power_law" && any(x < 1))
    warning("Warning: Some x values are less than 1; power-law fits may be unstable.")

  out <- fit_model_cv_cpp(x, y, model_type, metric)

  structure(list(
    model            = out$model,
    chosen_loss      = out$chosen_loss,
    coefficients     = out$coefficients,
    fitted.values    = out$fitted.values,
    residuals        = y - out$fitted.values,
    x                = x,
    y                = y,
    rmse             = out$rmse,
    mae              = out$mae,
    mse              = out$mse,
    selection_metric = out$selection_metric,
    selection_score  = out$selection_score,
    cv_results       = out$cv_results,
    all_results      = out$all_results
  ), class = "smartFit")
}
#' Print a SmartCurveFit Model
#'
#' Provides a concise textual summary of a fitted `"smartFit"` object.
#' The output includes:
#' - model type
#' - chosen loss and selection metric
#' - estimated coefficients
#' - overall fit metrics (RMSE, MAE, MSE)
#' - compact cross-validation results
#'
#' @details
#' This method is intended for quick inspection of a model. It is more concise
#' than `summary.smartFit()`, which provides extended diagnostics.
#'
#' If extra arguments are supplied via `...`, they are ignored with a warning
#' to alert the user.
#'
#' @param x An object of class `"smartFit"`.
#' @param ... Arguments passed for compatibility with the generic
#'   [base::print()]. They are ignored by `print.smartFit()`; supplying
#'   additional arguments will trigger a warning.
#'
#' @return Invisibly returns the input `"smartFit"` object.
#'
#' @examples
#' set.seed(1)
#' x <- 1:20; y <- 2 * x + rnorm(20)
#' fit <- fit_model(x, y, "power_law")
#' print(fit)
#' print(fit, foo = "bar") # triggers unused-argument warning
#'
#' @export
print.smartFit <- function(x, ...) {
  # ---- validation ----
  if (!inherits(x, "smartFit")) {
    stop("Error: print.smartFit can only be used on objects of class 'smartFit'.")
  }
  required <- c("model", "chosen_loss", "coefficients")
  missing <- setdiff(required, names(x))
  if (length(missing) > 0) {
    stop("Error: Invalid smartFit object: missing field(s): ",
         paste(missing, collapse = ", "))
  }

  # ---- unused args ----
  dots <- list(...)
  if (length(dots) > 0) {
    warning("Warning: Unused arguments ignored in print.smartFit: ",
            paste(names(dots), collapse = ", "))
  }

  # ---- concise output ----
  cat("smartCurveFit object\n")
  cat(sprintf("  Model type   : %s\n", x$model))
  cat(sprintf("  Chosen loss  : %s (selection by %s, score = %.4g)\n",
              x$chosen_loss, x$selection_metric, x$selection_score))

  if (!is.null(x$coefficients)) {
    cat("  Coefficients : ")
    cf <- signif(x$coefficients, 4)
    cat(paste(names(cf), "=", cf, collapse = ", "), "\n")

    # ---- show model equation ----
    a <- unname(x$coefficients[1])
    b <- unname(x$coefficients[2])
    if (x$model == "power_law") {
      cat(sprintf("  Formula      : y ~ %.3f * x^%.3f\n", a, b))
    } else if (x$model == "exponential") {
      cat(sprintf("  Formula      : y ~ %.3f * exp(%.3f * x)\n", a, b))
    } else if (x$model == "logarithmic") {
      cat(sprintf("  Formula      : y ~ %.3f + %.3f * log(x)\n", a, b))
    }
  }

  if (!is.null(x$rmse)) {
    cat(sprintf("  RMSE = %.4g | MAE = %.4g | MSE = %.4g\n",
                x$rmse, x$mae, x$mse))
  }

  # ---- compact CV results ----
  if (!is.null(x$cv_results)) {
    cat("\nCross-validated errors (mean +/- sd across folds):\n")
    cr <- x$cv_results
    fmt <- function(m, s) sprintf("%.4g +/- %.2g", m, s)
    show <- data.frame(
      loss    = cr$loss,
      `CV-RMSE` = mapply(fmt, cr$mean_rmse, cr$sd_rmse),
      `CV-MAE`  = mapply(fmt, cr$mean_mae,  cr$sd_mae)
    )
    print(show, row.names = FALSE)
  }

  invisible(x)
}

#' Extended Summary of a SmartCurveFit Model
#'
#' Produces a detailed report of a fitted `"smartFit"` object, including:
#' - model type, chosen loss, and selection metric
#' - estimated coefficients
#' - fit metrics (RMSE, MAE, MSE)
#' - residual distribution summary
#' - full cross-validation results
#' - full-data refits under all candidate losses
#'
#' @details
#' Unlike `print.smartFit()`, this method is verbose and intended for
#' model diagnostics. It is especially useful for checking residual
#' behaviour, loss function comparisons, and coefficient stability.
#'
#' Any extra arguments passed via `...` are ignored with a warning.
#'
#' @param object An object of class `"smartFit"`.
#' @param ... Arguments passed for compatibility with the generic
#'   [base::summary()]. They are ignored by `summary.smartFit()`; supplying
#'   additional arguments will trigger a warning.
#'
#' @return Invisibly returns the input `"smartFit"` object.
#'
#' @examples
#' set.seed(123)
#' x <- 1:30; y <- 3 * exp(0.2 * x) + rnorm(30)
#' fit <- fit_model(x, y, "exponential")
#' summary(fit)
#' summary(fit, extra = TRUE) # triggers unused-argument warning
#'
#' @export
summary.smartFit <- function(object, ...) {
  # ---- validation ----
  if (!inherits(object, "smartFit")) {
    stop("Error: summary.smartFit can only be used on objects of class 'smartFit'.")
  }
  required <- c("model", "chosen_loss", "coefficients")
  missing <- setdiff(required, names(object))
  if (length(missing) > 0) {
    stop("Error: Invalid smartFit object: missing field(s): ",
         paste(missing, collapse = ", "))
  }

  # ---- unused args check ----
  dots <- list(...)
  if (length(dots) > 0) {
    warning("Warning: Invalid arguments ignored in summary.smartFit: ",
            paste(names(dots), collapse = ", "))
  }

  # ---- summary output ----
  cat("=== smartCurveFit Summary ===\n")
  cat("Model type     :", object$model, "\n")
  cat("Chosen loss    :", object$chosen_loss, "\n")
  cat("Selection rule :", object$selection_metric,
      sprintf("(score = %.6g)\n", object$selection_score))

  if (!is.null(object$coefficients)) {
    cat("\nCoefficients:\n")
    print(object$coefficients)
  }
  if (!is.null(object$rmse)) {
    cat(sprintf("\nFit metrics on full data:\n  RMSE = %.6g   MAE = %.6g   MSE = %.6g\n",
                object$rmse, object$mae, object$mse))
  }
  if (!is.null(object$residuals)) {
    cat("\nResidual summary:\n")
    print(summary(object$residuals))
  }
  if (!is.null(object$cv_results)) {
    cat("\nCross-validated errors (mean +/- sd across folds):\n")
    print(object$cv_results)
  }
  if (!is.null(object$all_results)) {
    cat("\nFull-data refit under all losses:\n")
    print(object$all_results)
  }

  invisible(object)
}

#' Plot a SmartCurveFit Model
#'
#' Visualises observed data points and the fitted curve from a `"smartFit"`
#' object. Optionally, a residuals plot can also be drawn.
#'
#' @details
#' - By default, the upper plot shows the observed `(x, y)` values and the
#'   fitted curve under the chosen model/loss.
#' - If `show_residuals = TRUE`, an additional panel displays residuals
#'   versus fitted values.
#' - Graphical arguments like `col` and `pch` are mapped to control point
#'   appearance. Invalid arguments are ignored with a warning.
#'
#' @param x An object of class `"smartFit"`.
#' @param y Ignored; included for consistency with the generic `plot()`.
#' @param show_residuals Logical; if `TRUE`, draws residuals vs fitted values
#'   in a second panel.
#' @param col_points Color for observed data points (default `"steelblue"`).
#' @param pch_points Plot symbol for observed data (default `19`).
#' @param col_line Color for fitted curve (default `"red"`).
#' @param lwd_line Line width for fitted curve (default `2`).
#' @param ... Additional graphical arguments passed to [graphics::plot()].
#'   Invalid arguments are ignored with a warning. Included for compatibility
#'   with the base `plot()` generic.
#'
#' @return Invisibly returns the input `"smartFit"` object.
#'
#' @examples
#' set.seed(321)
#' x <- 1:50; y <- 3 + 2 * log(x) + rnorm(50)
#' fit <- fit_model(x, y, "logarithmic")
#'
#' # Default plot
#' plot(fit)
#'
#' # With residuals
#' plot(fit, show_residuals = TRUE)
#'
#' # Custom styling
#' plot(fit, col_points = "darkgreen", col_line = "orange", lwd_line = 3)
#'
#' # Invalid argument triggers warning
#' plot(fit, badarg = 99)
#'
#' @importFrom graphics par lines abline grid
#' @export
plot.smartFit <- function(x, y, ...,
                          show_residuals = FALSE,
                          col_points = "steelblue", pch_points = 19,
                          col_line = "red", lwd_line = 2) {
  if (!inherits(x, "smartFit")) {
    stop("Error: plot.smartFit can only be used on objects of class 'smartFit'.")
  }

  # collect user args
  dots <- list(...)

  # --- map base-style args ---
  if (!is.null(dots$col)) {
    col_points <- dots$col   # map `col` -> points color
    dots$col <- NULL
  }
  if (!is.null(dots$pch)) {
    pch_points <- dots$pch   # map `pch` -> points symbol
    dots$pch <- NULL
  }

  # validate remaining args against plot.default
  valid_args <- names(formals(graphics::plot.default))
  invalid <- setdiff(names(dots), valid_args)
  if (length(invalid) > 0) {
    warning("Warning: Invalid graphical arguments ignored in plot.smartFit: ",
            paste(invalid, collapse = ", "))
    dots[invalid] <- NULL
  }

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  if (show_residuals) par(mfrow = c(2, 1))

  # --- scatter with fitted curve ---
  do.call(plot, c(list(x$x, x$y,
                       col = col_points, pch = pch_points,
                       main = sprintf("smartCurveFit: %s model (loss = %s)",
                                      x$model, x$chosen_loss),
                       xlab = "x", ylab = "y"), dots))

  ord <- order(x$x)
  lines(x$x[ord], x$fitted.values[ord], col = col_line, lwd = lwd_line)
  grid(col = "lightgray", lty = "dotted")

  # --- residual plot ---
  if (show_residuals && !is.null(x$residuals)) {
    plot(x$fitted.values, x$residuals,
         col = "darkorange", pch = 19,
         main = "Residuals vs Fitted",
         xlab = "Fitted values", ylab = "Residuals")
    abline(h = 0, lty = 2, col = "gray40")
    grid(col = "lightgray", lty = "dotted")
  }

  invisible(x)
}



#' Predict from a SmartCurveFit Model
#'
#' Generates predictions from a fitted `"smartFit"` object for new values of `x`.
#'
#' @details
#' The prediction formula depends on the model type:
#' - **Power law:**     \eqn{y = a * x^b}
#' - **Exponential:**   \eqn{y = a * exp(b * x)}
#' - **Logarithmic:**   \eqn{y = a + b * log(x)}
#'
#' @param object An object of class `"smartFit"`, returned by [fit_model()].
#' @param newdata Numeric vector of predictor values at which to compute predictions.
#'   If omitted, predictions are returned for the training data used to fit the model.
#' @param ... Arguments passed for compatibility with the generic [stats::predict()].
#'   Extra arguments are ignored with a warning.
#'
#' @return A numeric vector of predicted values corresponding to `newdata` (or training data if `newdata` is missing).
#'
#' @examples
#' set.seed(101)
#' x <- 1:20
#' y <- 2 * x^1.2 + rnorm(20)
#' fit <- fit_model(x, y, model_type = "power_law")
#'
#' # Predict on training data
#' preds_train <- predict(fit)
#' head(preds_train)
#'
#' # Predict on new data
#' new_x <- seq(1, 25, by = 0.5)
#' preds_new <- predict(fit, newdata = new_x)
#' head(preds_new)
#'
#' @export
predict.smartFit <- function(object, newdata = NULL, ...) {
  # ---- validation ----
  if (!inherits(object, "smartFit")) {
    stop("Error: predict.smartFit can only be used on objects of class 'smartFit'.")
  }
  required <- c("model", "coefficients")
  missing <- setdiff(required, names(object))
  if (length(missing) > 0) {
    stop("Error: Invalid smartFit object: missing field(s): ",
         paste(missing, collapse = ", "))
  }

  # ---- unused args ----
  dots <- list(...)
  if (length(dots) > 0) {
    warning("Warning: Unused arguments ignored in predict.smartFit: ",
            paste(names(dots), collapse = ", "))
  }

  # ---- data checks ----
  if (is.null(newdata)) {
    newx <- object$x
  } else {
    if (!is.numeric(newdata)) {
      stop("Error: newdata must be a numeric vector.")
    }
    if (length(newdata) == 0) {
      stop("Error: newdata must have length >= 1.")
    }
    if (any(is.na(newdata)) || any(is.nan(newdata))) {
      stop("Error: newdata contains NA/NaN values. Please remove or impute.")
    }
    if (any(is.infinite(newdata))) {
      stop("Error: newdata contains infinite values. Please clean your input.")
    }
    if (object$model %in% c("power_law", "logarithmic") && any(newdata <= 0)) {
      stop("Error: For power_law/logarithmic, all newdata values must be strictly positive (> 0).")
    }

    # 🔹 Large dataset warning
    .check_input_size_predict(newdata)

    newx <- newdata
  }

  # ---- prediction ----
  a <- unname(object$coefficients[1])
  b <- unname(object$coefficients[2])

  preds <- switch(object$model,
                  power_law   = a * newx^b,
                  exponential = a * exp(b * newx),
                  logarithmic = a + b * log(newx),
                  stop("Error: Unsupported model type in smartFit object.")
  )

  preds
}



