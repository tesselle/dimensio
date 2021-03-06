# HELPERS
NULL


#' Weighted Column Means and Standard Deviations
#'
#' @param x A [`numeric`] matrix.
#' @param w An [`numeric`] vector.
#' @return A [`numeric`] vector.
#' @keywords internal
#' @noRd
weighted_mean <- function(x, w) {
  as.vector(crossprod(w, x))
}
weighted_sd <- function(x, w) {
  sqrt(as.vector(crossprod(w, x^2)))
}

#' @param index A [`numeric`] vector.
#' @param n An [`integer`] value.
#' @return A [`logical`] vector.
#' @keywords internal
#' @noRd
is_supplementary <- function(index, n) {
  x <- logical(n)

  if (is.null(index)) {
    return(x)
  } else {
    if (is.numeric(index)) {
      x[index] <- TRUE
      return(x)
    }
    if (is.logical(index)) {
      return(index)
    }
    arg <- deparse(substitute(index))
    msg <- sprintf("%s must be a numeric vector of indices.", sQuote(arg))
    stop(msg, call. = FALSE)
  }
}
