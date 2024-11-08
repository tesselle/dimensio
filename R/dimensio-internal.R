# HELPERS

## https://michaelchirico.github.io/potools/articles/developers.html
tr_ <- function(...) {
  enc2utf8(gettext(paste0(...), domain = "R-dimensio"))
}

recycle <- function(x, n) {
  if (length(x) == 1) rep(x, n) else x
}

print_variance <- function(object, axis) {
  v <- get_variance(object, digits = 1) # Get percentage of variance
  sprintf("%s (%g%%)", names(v)[[axis]], v[[axis]])
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

#' Column Index
#'
#' @param index A [`numeric`] vector.
#' @param n An [`integer`] value.
#' @param names A [`character`] vector.
#' @return A [`logical`] vector.
#' @keywords internal
#' @noRd
find_variable <- function(index, n, names = NULL) {
  x <- logical(n)

  if (is.null(index)) return(x)

  if (is.logical(index)) {
    arkhe::assert_length(index, n)
    return(index)
  }

  if (is.character(index)) {
    index <- match(index, names)
    index <- index[!is.na(index)]
    if (length(index) == 0) return(x)
  }

  if (is.numeric(index)) {
    x[index] <- TRUE
    return(x)
  }

  arkhe::assert_type(index, "numeric")
}

#' Remove Columns Using a Predicate
#'
#' @param x A [`data.frame`].
#' @param what A predicate [`function`].
#' @param negate A [`logical`] scalar: should the negation of `f` be used
#'  instead of `f`?
#' @param sup A `vector` specifying the indices of the supplementary columns.
#' @param extra A `vector` specifying the indices of the extra columns.
#' @param what A [`character`] string to be used in the message.
#' @param verbose A [`logical`] scalar: should \R report extra information on
#'  progress?
#' @details
#'  Side effect: move `sup` and `extra` columns at the end of `x`.
#' @return A `list` with the following elements: `data` (a `data.frame`),
#'  `sup` (an `integer` vector) and `extra` (a `data.frame` or `NULL`).
#' @keywords internal
#' @noRd
drop_variable <- function(x, f, negate = FALSE, sup = NULL, extra = NULL,
                          what = "extra", verbose = getOption("dimensio.verbose")) {
  if (negate) f <- Negate(f)
  not_ok <- vapply(x, FUN = f, FUN.VALUE = logical(1))

  if (any(not_ok)) {
    is_extra <- find_variable(extra, ncol(x), names = colnames(x))
    is_sup <- find_variable(sup, ncol(x), names = colnames(x))

    old <- x
    x <- x[, !(not_ok | is_sup | is_extra), drop = FALSE]

    if (any(is_sup)) {
      ## Move supplementary variables at the end
      sup <- seq_len(sum(is_sup)) + ncol(x)
      x <- cbind(x, old[, is_sup, drop = FALSE])
    }
    if (any(is_extra)) {
      ## Remove extra variable
      extra <- old[, is_extra, drop = FALSE]
    }

    # Generate message
    not_ok[is_sup | is_extra] <- FALSE
    if (any(not_ok) && verbose) {
      tot <- sum(not_ok)
      msg <- ngettext(tot, "%d %s variable was removed: %s.",
                      "%d %s variables were removed: %s.")
      col <- paste(colnames(old)[not_ok], collapse = ", ")
      message(sprintf(msg, tot, what, col))
    }
  }

  list(
    data = x,
    sup = sup,
    extra = extra
  )
}
