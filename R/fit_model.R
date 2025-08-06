#' Fit a nonlinear model using C++ (power law, L2 loss only)
#'
#' @param x Numeric predictor
#' @param y Numeric response
#' @param model_type Only "power_law" is supported for now
#' @param start_params Starting values for model parameters (default: c(1,1))
#' @return S3 object of class 'smartFit'
#' @useDynLib smartcurvefit, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @export
fit_model <- function(x, y, model_type = "power_law", start_params = c(1, 1)) {
  out <- fit_model_cpp(x, y, model_type, start_params)
  structure(
    list(
      model = model_type,
      coefficients = out$coefficients,
      fitted.values = out$fitted.values,
      loss_value = out$loss  # always L2 for now
    ),
    class = "smartFit"
  )
}

#' @export
print.smartFit <- function(x, ...) {
  cat("smartCurveFit Model:", x$model, "with", x$loss, "loss\n")
  cat("Coefficients:\n")
  print(x$coefficients)
  invisible(x)
}
