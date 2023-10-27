# HELPERS

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
