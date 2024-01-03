# SUMMARY
#' @include AllGenerics.R
NULL

# CA ===========================================================================
#' @export
#' @method summary CA
summary.CA <- function(object, ..., margin = 1, active = TRUE, sup = TRUE,
                       rank = 3) {
  ## Get data
  values <- build_summary(object, margin = margin, rank = rank,
                          active = active, sup = sup)

  .SummaryCA(
    data = object@data,
    eigenvalues = as.matrix(values$eigenvalues),
    results = as.matrix(values$results),
    supplement = values$supplement,
    margin = as.integer(margin)
  )
}

#' @export
#' @rdname summary
#' @aliases summary,CA-method
setMethod("summary", c(object = "CA"), summary.CA)

# PCA ==========================================================================
#' @export
#' @method summary PCA
summary.PCA <- function(object, ..., margin = 1, active = TRUE, sup = TRUE,
                        rank = 3) {
  ## Get data
  values <- build_summary(object, margin = margin, rank = rank,
                          active = active, sup = sup)

  .SummaryPCA(
    data = object@data,
    eigenvalues = as.matrix(values$eigenvalues),
    results = as.matrix(values$results),
    supplement = values$supplement,
    margin = as.integer(margin)
  )
}

#' @export
#' @rdname summary
#' @aliases summary,PCA-method
setMethod("summary", c(object = "PCA"), summary.PCA)

build_summary <- function(object, margin, rank = 3,
                          active = TRUE, sup = TRUE,
                          prefix = "F") {
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
  rank <- min(rank, dim(object))
  dim_keep <- seq_len(rank)
  values <- vector(mode = "list", length = rank)
  for (j in dim_keep) {
    v <- data.frame(coord[[j]], contrib[[j]], cos2[[j]])
    names(v) <- paste0(prefix, j, c("_coord", "_contrib", "_cos2"))
    values[[j]] <- v
  }
  values <- data.frame(inertia = inertia, values)
  if (inherits(object, "PCA")) colnames(values)[1] <- "dist"
  rownames(values) <- rownames(coord)

  ## Remove data
  is_sup <- coord$.sup
  if (!active && !sup) active <- TRUE
  if (!active) {
    values <- values[is_sup, ]
    is_sup <- is_sup[is_sup]
  }
  if (!sup) {
    values <- values[!is_sup, ]
    is_sup <- is_sup[!is_sup]
  }

  list(eigenvalues = eig, results = values, supplement = is_sup)
}
