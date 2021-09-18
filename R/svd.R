# SVD
#' @include AllClasses.R AllGenerics.R
NULL

#' Singular Value Decomposition of a Matrix
#'
#' @param x A \eqn{m \times p}{m x p} numeric [`matrix`].
#' @param rank An [`integer`] value specifying the maximal number of components
#'  to be kept in the results.
#' @return
#'  A [`list`] with the following elements:
#'  \describe{
#'   \item{`d`}{A vector containing the singular values of `x`, of length
#'   `rank`, sorted decreasingly.}
#'   \item{`u`}{A matrix whose columns contain the left singular vectors of
#'   `x`. Dimension `c(m, rank)`.}
#'   \item{`v`}{A matrix whose columns contain the right singular vectors of
#'   `x`. Dimension `c(p, rank)`.}
#'  }
#' @keywords internal
#' @noRd
svd2 <- function(x, rank = Inf) {
  D <- svd(x, nu = rank, nv = rank)

  keep <- seq_len(rank)
  sv <- D$d[keep]

  U <- D$u
  V <- D$v

  # Fix sign for consistency with FactoMineR
  if (rank > 1) {
    mult <- sign(as.vector(crossprod(rep(1, nrow(V)), as.matrix(V))))
    mult[mult == 0] <- 1

    # Build matrix
    # matrix * vector is faster (!) than:
    # matrix %*% t(vector)
    # t(t(matrix) * vector)
    mult_U <- matrix(mult, nrow = nrow(U), ncol = rank, byrow = TRUE)
    mult_V <- matrix(mult, nrow = nrow(V), ncol = rank, byrow = TRUE)

    U <- U * mult_U
    V <- V * mult_V
  }

  names(sv) <- paste0("F", keep)
  list(
    d = sv,
    u = U,
    v = V
  )
}
