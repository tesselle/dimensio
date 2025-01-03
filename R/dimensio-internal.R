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
#' @param f A predicate [`function`].
#' @param negate A [`logical`] scalar: should the negation of `f` be used
#'  instead of `f`?
#' @param sup A `vector` specifying the indices of the supplementary columns.
#' @param extra A `vector` specifying the indices of the extra columns.
#' @param auto A [`logical`] scalar: should invalid variables be automatically
#'  removed?
#' @param what A [`character`] string to be used in the message.
#' @param verbose A [`logical`] scalar: should \R report extra information on
#'  progress?
#' @details
#'  Side effect: move `sup` and `extra` columns at the end of `x`.
#' @return A `list` with the following elements: `data` (a `matrix`),
#'  `sup` (an `integer` vector) and `extra` (a `data.frame` or `NULL`).
#' @keywords internal
#' @noRd
drop_variable <- function(x, f, negate = FALSE, sup = NULL, extra = NULL,
                          auto = TRUE, what = "extra",
                          verbose = getOption("dimensio.verbose")) {
  ## Check variables
  if (negate) f <- Negate(f)
  not_ok <- vapply(x, FUN = f, FUN.VALUE = logical(1))

  ## Get extra variables
  is_extra <- find_variable(extra, ncol(x), names = colnames(x))
  is_sup <- find_variable(sup, ncol(x), names = colnames(x))

  ## Quit
  if (!auto && any(not_ok & !is_extra)) {
    msg <- tr_("Some variables are invalid: %s.")
    col <- paste(colnames(x)[not_ok & !is_extra], collapse = ", ")
    stop(sprintf(msg, col), call. = FALSE)
  }

  ## Extract extra variables, if any
  if (any(is_extra)) {
    extra <- x[, is_extra, drop = FALSE]
  }

  ## Remove supplementary/extra variables, if any
  tmp <- x
  x <- x[, !(not_ok | is_sup | is_extra), drop = FALSE]

  ## Move supplementary variables at the end, if any
  is_sup_ok <- is_sup & !not_ok
  if (any(is_sup_ok)) {
    sup <- seq_len(sum(is_sup_ok)) + ncol(x)
    x <- cbind(x, tmp[, is_sup_ok, drop = FALSE])
  } else {
    # warning("!", call. = FALSE)
    sup <- NULL
  }

  ## Generate message
  if (any(not_ok)) {
    not_ok[is_sup | is_extra] <- FALSE
    if (any(not_ok) && verbose) {
      tot <- sum(not_ok)
      msg <- ngettext(tot, "%d %s variable was removed: %s.",
                      "%d %s variables were removed: %s.")
      col <- paste(colnames(tmp)[not_ok], collapse = ", ")
      message(sprintf(msg, tot, what, col))
    }
  }

  list(
    data = as.matrix(x),
    sup = sup,
    extra = extra
  )
}
