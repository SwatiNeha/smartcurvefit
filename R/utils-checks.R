#' Internal: Check dataset size
#'
#' Issues a warning if the input vectors exceed `n_threshold`.
#'
#' @keywords internal
#' @noRd
.check_input_size_fit <- function(x, y, n_threshold = 5000) {
  if (length(x) >= n_threshold || length(y) >= n_threshold) {
    warning(sprintf(
      "Warning: Large dataset detected (n = %d >= %d). Cross-validation may be slow.",
      max(length(x), length(y)), n_threshold
    ))
  }
  invisible(TRUE)
}

.check_input_size_predict <- function(x, n_threshold = 2e8) {
  if (length(x) >= n_threshold) {
    warning(sprintf(
      "Warning: Large prediction dataset detected (n = %d >= %d). Prediction may be slow or memory-intensive.",
      length(x), n_threshold
    ))
  }
  invisible(TRUE)
}
