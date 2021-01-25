# JOINT
NULL

#' Joint
#'
#' @param object A \linkS4class{CA} or \linkS4class{PCA} object.
#' @param margin A length-one \code{\link{numeric}} vector giving the subscript
#'  which the data will be returned: \code{1} indicates individuals/rows (the
#'  default), \code{2} indicates variables/columns.
#' @param axes A length-two \code{\link{numeric}} vector giving the dimensions
#'  to be for which to compute results.
#' @param sup A \code{\link{logical}} scalar: should supplementary points be
#'  returned?
#' @seealso \link[=mutator]{get_*()}
#' @example inst/examples/ex-joint.R
#' @author N. Frerebeau
#' @docType methods
#' @family summary
#' @name joint
#' @rdname joint
#' @noRd
NULL

joint_coordinates <- function(object, margin = 1, axes = c(1, 2), sup = TRUE) {
  axes <- axes[c(1, 2)]
  coord <- get_coordinates(object, margin = margin, sup = sup)
  rowSums(coord[, axes]^2)
}

joint_contributions <- function(object, margin = 1, axes = c(1, 2)) {
  axes <- axes[c(1, 2)]
  contrib <- get_contributions(object, margin = margin)
  eig <- matrix(
    data = object@singular_values[axes]^2,
    nrow = nrow(contrib),
    ncol = 2,
    byrow = TRUE
  )
  rowSums(contrib[, axes] * eig)
}

joint_cos2 <- function(object, margin = 1, axes = c(1, 2), sup = TRUE) {
  axes <- axes[c(1, 2)]
  cos2 <- get_cos2(object, margin = margin, sup = sup)
  rowSums(cos2[, axes])
}

joint_distances <- function(object, margin = 1, axes = c(1, 2), sup = TRUE) {
  axes <- axes[c(1, 2)]
  d <- get_distances(object, margin = margin, sup = sup)
  rowSums(d[, axes])
}
