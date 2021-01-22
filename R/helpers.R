# HELPERS
NULL

#' @param index A \code{\link{numeric}} vector.
#' @param n An \code{\link{integer}} value.
#' @return A \code{\link{logical}} vector.
#' @keywords internal
#' @noRd
is_supplementary <- function(index, n) {
  x <- rep(FALSE, times = n)

  if (!is.null(index) && (!is.numeric(index) & !is.logical(index))) {
    arg <- deparse(substitute(index))
    msg <- sprintf("%s must be a numeric vector of indices.", sQuote(arg))
    stop(msg, call. = FALSE)
  } else {
    x[index] <- TRUE
  }

  x
}
