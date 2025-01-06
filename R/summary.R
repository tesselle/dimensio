# SUMMARY
#' @include AllGenerics.R
NULL

# CA ===========================================================================
#' @export
#' @method summary CA
summary.CA <- function(object, ..., axes = c(1, 2), margin = 1,
                       active = TRUE, sup = TRUE, rank = NULL) {
  ## Get data
  values <- build_summary(object, axes = axes, margin = margin, rank = rank,
                          active = active, sup = sup)

  .SummaryCA(
    data = object@data,
    eigenvalues = values$eigenvalues,
    results = values$results,
    supplement = values$supplement,
    margin = as.integer(margin)
  )
}

#' @export
#' @rdname summary
#' @aliases summary,CA-method
setMethod("summary", c(object = "CA"), summary.CA)

#' @export
#' @rdname describe
#' @aliases describe,CA-method
setMethod(
  f = "describe",
  signature = signature(x = "CA"),
  definition = function(x, ...) {
    row_sup <- x@rows@supplement
    col_sup <- x@columns@supplement

    sup_txt <- tr_(" (+ %d supplementary)")
    row_txt <- if (any(row_sup)) sprintf(sup_txt, sum(row_sup)) else ""
    col_txt <- if (any(col_sup)) sprintf(sup_txt, sum(col_sup)) else ""

    cat(
      sprintf(tr_("* Row variable: %d categories%s."), sum(!row_sup), row_txt),
      sprintf(tr_("* Column variable: %d categories%s."), sum(!col_sup), col_txt),
      ...,
      sep = "\n"
    )
    invisible(x)
  }
)

# PCA ==========================================================================
#' @export
#' @method summary PCA
summary.PCA <- function(object, ..., axes = c(1, 2), margin = 1,
                        active = TRUE, sup = TRUE, rank = NULL) {
  ## Get data
  values <- build_summary(object, axes = axes, margin = margin, rank = rank,
                          active = active, sup = sup)

  .SummaryPCA(
    data = object@data,
    eigenvalues = values$eigenvalues,
    results = values$results,
    supplement = values$supplement,
    margin = as.integer(margin)
  )
}

#' @export
#' @rdname summary
#' @aliases summary,PCA-method
setMethod("summary", c(object = "PCA"), summary.PCA)

#' @export
#' @rdname describe
#' @aliases describe,PCA-method
setMethod(
  f = "describe",
  signature = signature(x = "PCA"),
  definition = function(x, ...) {
    row_sup <- x@rows@supplement
    col_sup <- x@columns@supplement

    sup_txt <- tr_(" (+ %d supplementary)")
    row_txt <- if (any(row_sup)) sprintf(sup_txt, sum(row_sup)) else ""
    col_txt <- if (any(col_sup)) sprintf(sup_txt, sum(col_sup)) else ""

    if (is_centered(x)) {
      var_center <- tr_("* Variables were shifted to be zero centered.")
    } else {
      var_center <- tr_("* Variables were NOT shifted to be zero centered.")
    }
    if (is_scaled(x)) {
      var_scale <- tr_("* Variables were scaled to unit variance.")
    } else {
      var_scale <- tr_("* Variables were NOT scaled to unit variance.")
    }

    cat(
      sprintf(tr_("* %d individuals%s."), sum(!row_sup), row_txt),
      sprintf(tr_("* %d variables%s."), sum(!col_sup), col_txt),
      var_center,
      var_scale,
      ...,
      sep = "\n"
    )
    invisible(x)
  }
)

# Helpers ======================================================================
build_summary <- function(object, axes, margin, rank = NULL,
                          active = TRUE, sup = TRUE,
                          prefix = "F") {
  ## Validation
  arkhe::assert_filled(axes)
  arkhe::assert_type(axes, "numeric")

  ## /!\ Backward compatibility /!\
  if (!is.null(rank)) {
    axes <- seq_len(rank)
    msg <- "'rank' argument is deprecated, use 'axes' instead."
    warning(msg, call. = FALSE)
  }

  ## Get data
  eig <- get_eigenvalues(object)
  inertia <- get_distances(object, margin = margin)
  coord <- get_coordinates(object, margin = margin)
  contrib <- get_contributions(object, margin = margin)
  cos2 <- get_cos2(object, margin = margin)

  if (inherits(object, "CA")) inertia <- inertia * 1000

  ## Fix lengths
  n <- nrow(coord)
  m <- nrow(contrib)
  if (n > m) {
    length(inertia) <- n
    contrib[seq(m + 1, n, 1), ] <- NA
  }

  ## Bind columns
  values <- vector(mode = "list", length = length(axes))
  for (j in axes) {
    v <- cbind(coord[[j]], contrib[[j]], cos2[[j]])
    colnames(v) <- paste0(prefix, j, c("_coord", "_contrib", "_cos2"))
    values[[j]] <- v
  }
  values <- do.call(cbind, values)
  values <- cbind(inertia = inertia, values)
  if (inherits(object, "PCA")) colnames(values)[1] <- "dist"
  rownames(values) <- rownames(coord)

  ## Remove data
  is_sup <- coord$.sup
  if (!active && !sup) active <- TRUE
  if (!active) {
    values <- values[is_sup, , drop = FALSE]
    is_sup <- is_sup[is_sup]
  }
  if (!sup) {
    values <- values[!is_sup, , drop = FALSE]
    is_sup <- is_sup[!is_sup]
  }

  list(
    eigenvalues = as.matrix(eig),
    results = as.matrix(values),
    supplement = is_sup
  )
}
