# SUMMARY
#' @include AllClasses.R
NULL

# Correspondence Analysis ======================================================
#' @export
#' @rdname summary
#' @aliases summary,CA-method
setMethod(
  f = "summary",
  signature = signature(object = "CA"),
  definition = function(object, margin = 1, active = TRUE, sup = TRUE,
                        rank = 3) {
    ## Eigenvalues
    eig <- get_eigenvalues(object)

    ## Results
    inert <- get_inertia(object, margin = margin) * 1000
    coord <- get_coordinates(object, margin = margin, sup = TRUE)
    contrib <- get_contributions(object, margin = margin)
    cos2 <- get_cos2(object, margin = margin, sup = TRUE)

    values <- build_summary(inertia = inert, coord = coord, contrib = contrib,
                            cos2 = cos2, rank = rank, active = active,
                            sup = sup, prefix = "CA")

    ## Remove data
    is_sup <- coord$.sup
    if (!active) {
      values <- values[is_sup, ]
      is_sup <- is_sup[is_sup]
    }
    if (!sup) {
      values <- values[!is_sup, ]
      is_sup <- is_sup[!is_sup]
    }

    .SummaryCA(
      data = object@data,
      eigenvalues = as.matrix(eig),
      results = as.matrix(values),
      supplement = is_sup,
      margin = as.integer(margin)
    )
  }
)

# Principal Components Analysis ================================================
#' @export
#' @rdname summary
#' @aliases summary,PCA-method
setMethod(
  f = "summary",
  signature = signature(object = "PCA"),
  definition = function(object, margin = 1, active = TRUE, sup = TRUE,
                        rank = 3) {
    ## Eigenvalues
    eig <- get_eigenvalues(object)

    ## Results
    inert <- get_distances(object, margin = margin)
    coord <- get_coordinates(object, margin = margin, sup = TRUE)
    contrib <- get_contributions(object, margin = margin)
    cos2 <- get_cos2(object, margin = margin, sup = TRUE)

    values <- build_summary(inertia = inert, coord = coord, contrib = contrib,
                            cos2 = cos2, rank = rank, active = active,
                            sup = sup, prefix = "PC")

    ## Remove data
    is_sup <- coord$.sup
    if (!active) {
      values <- values[is_sup, ]
      is_sup <- is_sup[is_sup]
    }
    if (!sup) {
      values <- values[!is_sup, ]
      is_sup <- is_sup[!is_sup]
    }

    .SummaryPCA(
      data = object@data,
      eigenvalues = as.matrix(eig),
      results = as.matrix(values),
      supplement = is_sup,
      margin = as.integer(margin)
    )
  }
)

build_summary <- function(inertia, coord, contrib, cos2,
                          rank = 3, active = TRUE, sup = TRUE,
                          prefix = "PC") {
  ## Fix lengths
  n <- nrow(coord)
  m <- nrow(contrib)
  if (n > m) {
    length(inertia) <- n
    contrib[seq(m + 1, n, 1), ] <- NA
  }

  ## Bind columns
  dim_keep <- seq_len(rank)
  values <- vector(mode = "list", length = rank)
  for (j in dim_keep) {
    v <- data.frame(coord[[j]], contrib[[j]], cos2[[j]])
    names(v) <- paste0(prefix, j, c("_coord", "_contrib", "_cos2"))
    values[[j]] <- v
  }
  values <- data.frame(inertia = inertia, values)
  if (prefix == "PC") colnames(values)[1] <- "dist"
  rownames(values) <- rownames(coord)

  values
}
