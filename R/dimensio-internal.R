# HELPERS

`%||%` <- function(x, y) {
  if (!is.null(x)) x else y
}

recycle <- function(x, n, verbose = getOption("dimensio.verbose")) {
  if (length(x) >= n) return(x[seq_len(n)])

  if (verbose && length(x) > 1) {
    arg <- deparse(substitute(x))
    msg <- sprintf("Note that %s was recycled to length %d.", sQuote(arg), n)
    message(msg)
  }
  x <- rep_len(x, length.out = n)
  x
}

#' Rescale Continuous Vector
#'
#' Rescales continuous vector to have specified minimum and maximum.
#' @param x A continuous `vector` of values to manipulate.
#' @param to A length-two [`numeric`] vector specifying the output range.
#' @param from A length-two [`numeric`] vector specifying the input range.
#' @return A [`numeric`] vector.
#' @keywords internal
#' @noRd
scale_range <- function(x, to = c(0, 1),
                        from = range(x, na.rm = TRUE, finite = TRUE)) {
  (x - from[1L]) / diff(from) * diff(to) + to[1L]
}

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

has_length <- function(x, n = NULL) {
  if (is.null(n)) length(x) > 0 else length(x) == n
}
assert_length <- function(x, expected, empty = FALSE) {
  arg <- deparse(substitute(x))
  if (!(empty & length(x) == 0) && !has_length(x, n = expected)) {
    msg <- sprintf("%s must be of length %d; not %s.", sQuote(arg),
                   expected, length(x))
    stop(msg, call. = FALSE)
  }
  invisible(x)
}

