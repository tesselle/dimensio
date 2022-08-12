# TIDY DATA
#' @include AllClasses.R
NULL

# Coordinates ==================================================================
#' @export
#' @rdname tidy
#' @aliases tidy,MultivariateAnalysis-method
setMethod(
  f = "tidy",
  signature = c(x = "MultivariateAnalysis"),
  definition = function(x, margin = 1, axes = c(1, 2), principal = TRUE, ...) {
    ## Validation
    assert_length(margin, 1)
    assert_length(axes, 2)

    ## Get data
    coords <- get_coordinates(x, margin = margin, principal = principal)
    weight <- data.frame(
      mass = get_masses(x, margin = margin)
    )
    contrib <- data.frame(
      contribution = joint_contributions(x, margin = margin, axes = axes)
    )

    data.frame(
      coords[, axes, drop = FALSE],
      labels = rownames(coords),
      supplementary = coords$.sup,
      mass = weight[match(rownames(coords), rownames(weight)), ],
      sum = joint_coordinates(x, margin = margin, axes = axes),
      contribution = contrib[match(rownames(coords), rownames(contrib)), ],
      cos2 = joint_cos2(x, margin = margin, axes = axes),
      row.names = NULL
    )
  }
)

#' Joint
#'
#' @param object A [`CA-class`] or [`PCA-class`] object.
#' @param what A [`character`] string.
#' @param margin A length-one [`numeric`] vector giving the subscript
#'  which the data will be returned: `1` indicates individuals/rows (the
#'  default), `2` indicates variables/columns.
#' @param axes A length-two [`numeric`] vector giving the dimensions
#'  to be for which to compute results.
#' @param sup A [`logical`] scalar: should supplementary points be
#'  returned?
#' @param ... Extra parameters to be passed to internal methods.
#' @seealso \link[=mutator]{get_*()}
#' @example inst/examples/ex-joint.R
#' @author N. Frerebeau
#' @docType methods
#' @family summary
#' @name joint
#' @rdname joint
#' @noRd
NULL

joint <- function(object, what, ...) {
  choices <- c("coordinates", "contributions", "cos2")
  what <- match.arg(what, choices = choices, several.ok = FALSE)

  fun <- switch (
    what,
    coordinates = joint_coordinates,
    contributions = joint_contributions,
    cos2 = joint_cos2
  )

  fun(object, ...)
}

joint_coordinates <- function(object, margin = 1, axes = c(1, 2), ...) {
  axes <- axes[c(1, 2)]
  coord <- get_coordinates(object, margin = margin)
  rowSums(coord[, axes]^2)
}

joint_contributions <- function(object, margin = 1, axes = c(1, 2), ...) {
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

joint_cos2 <- function(object, margin = 1, axes = c(1, 2), ...) {
  axes <- axes[c(1, 2)]
  cos2 <- get_cos2(object, margin = margin)
  rowSums(cos2[, axes])
}
