# CLASSES DEFINITION

# MultivariateAnalysis =========================================================

# CA ===========================================================================
#' Correspondence Analysis Results
#'
#' An S4 class to store the results of a simple correspondence analysis.
#' @slot data A \code{\link{numeric}} \code{\link{matrix}}
#' @slot dimension An \code{\link{integer}} giving the dimension of the
#'  solution.
#' @slot row_names A \code{\link{character}} vector specifying the row names.
#' @slot row_coordinates A \code{\link{numeric}} \code{\link{matrix}}
#'  giving the row standard coordinates.
#' @slot row_distances A \code{\link{numeric}} vector giving the row chi-square
#'  distances to centroid.
#' @slot row_inertias A \code{\link{numeric}} vector giving the row inertias.
#' @slot row_masses A \code{\link{numeric}} vector giving the row masses.
#' @slot row_supplement A \code{\link{logical}} vector specifying if a row is a
#'  supplementary point.
#' @slot column_names A \code{\link{character}} vector specifying the column
#'  names.
#' @slot column_coordinates A \code{\link{numeric}} \code{\link{matrix}}
#'  giving the column standard coordinates.
#' @slot column_distances A \code{\link{numeric}} vector giving the column
#'  chi-square distances to centroid.
#' @slot column_inertias A \code{\link{numeric}} vector giving the column
#'  inertias.
#' @slot column_masses A \code{\link{numeric}} vector giving the column masses.
#' @slot column_supplement A \code{\link{logical}} vector specifying if a column
#'  is a supplementary point.
#' @slot singular_values A \code{\link{numeric}} vector giving the singular
#'  values.
#' @section Subset:
#'  In the code snippets below, \code{x} is a \code{CA} object.
#'  \describe{
#'   \item{\code{x[[i]]}}{Extracts informations from a slot selected by
#'   subscript \code{i}. \code{i} is a length-one \code{\link{character}}
#'   vector. Returns the corresponding slot values.}
#'  }
#' @example inst/examples/ex-ca.R
#' @author N. Frerebeau
#' @family multivariate analysis
#' @docType class
#' @export .CA
#' @exportClass CA
#' @aliases CA-class
.CA <- setClass(
  Class = "CA",
  slots = c(
    data = "matrix",
    dimension = "integer",
    row_names = "character",
    row_coordinates = "matrix",
    row_distances = "numeric",
    row_inertias = "numeric",
    row_masses = "numeric",
    row_supplement = "logical",
    column_names = "character",
    column_coordinates = "matrix",
    column_distances = "numeric",
    column_inertias = "numeric",
    column_masses = "numeric",
    column_supplement = "logical",
    singular_values = "numeric"
  )
)

# PCA ==========================================================================


