# CLASSES DEFINITION

# MultivariateAnalysis =========================================================
#' Multivariate Data Analysis Results
#'
#' An S4 class to store the results of a multivariate data analysis.
#' @slot data A \code{\link{numeric}} \code{\link{matrix}}
#' @slot dimension An \code{\link{integer}} giving the dimension of the
#'  solution.
#' @slot row_names A \code{\link{character}} vector specifying the row names.
#' @slot row_coordinates A \code{\link{numeric}} \code{\link{matrix}}
#'  giving the individual/row principal coordinates.
#' @slot row_standard A \code{\link{numeric}} \code{\link{matrix}}
#'  giving the individual/row standard coordinates.
#' @slot row_distances A \code{\link{numeric}} vector giving the individual/row
#'  chi-square distances to centroid.
#' @slot row_cosine A \code{\link{numeric}} \code{\link{matrix}}
#'  giving the individual/row \eqn{cos^2}{cos2} values.
#' @slot row_weights A \code{\link{numeric}} vector giving the row masses/
#'  individual weights.
#' @slot row_supplement A \code{\link{logical}} vector specifying if a row is a
#'  supplementary point.
#' @slot column_names A \code{\link{character}} vector specifying the column
#'  names.
#' @slot column_coordinates A \code{\link{numeric}} \code{\link{matrix}}
#'  giving the variable/column principal coordinates.
#' @slot column_standard A \code{\link{numeric}} \code{\link{matrix}}
#'  giving the variable/column standard coordinates.
#' @slot column_distances A \code{\link{numeric}} vector giving the
#'  variable/column chi-square distances to centroid.
#' @slot column_cosine A \code{\link{numeric}} \code{\link{matrix}}
#'  giving the variable/column \eqn{cos^2}{cos2} values.
#' @slot column_weights A \code{\link{numeric}} vector giving the column masses/
#'  variable weights.
#' @slot column_supplement A \code{\link{logical}} vector specifying if a column
#'  is a supplementary point.
#' @slot singular_values A \code{\link{numeric}} vector giving the singular
#'  values.
#' @section Subset:
#'  In the code snippets below, \code{x} is a \code{MultivariateAnalysis}
#'  object.
#'  \describe{
#'   \item{\code{x[[i]]}}{Extracts information from a slot selected by
#'   subscript \code{i}. \code{i} is a length-one \code{\link{character}}
#'   vector.}
#'  }
#' @author N. Frerebeau
#' @family multivariate analysis
#' @docType class
#' @aliases MultivariateAnalysis-class
.MultivariateAnalysis <- setClass(
  Class = "MultivariateAnalysis",
  slots = c(
    data = "matrix",
    dimension = "integer",
    row_names = "character",
    row_coordinates = "matrix",
    row_standard = "matrix",
    row_distances = "numeric",
    row_cosine = "matrix",
    row_inertias = "numeric",
    row_weights = "numeric",
    row_supplement = "logical",
    column_names = "character",
    column_coordinates = "matrix",
    column_standard = "matrix",
    column_distances = "numeric",
    column_cosine = "matrix",
    column_inertias = "numeric",
    column_weights = "numeric",
    column_supplement = "logical",
    singular_values = "numeric"
  )
)

# CA ===========================================================================
#' CA Results
#'
#' An S4 class to store the results of a simple correspondence analysis.
#' @example inst/examples/ex-ca.R
#' @author N. Frerebeau
#' @family multivariate analysis
#' @docType class
#' @export .CA
#' @exportClass CA
#' @aliases CA-class
.CA <- setClass(
  Class = "CA",
  contains = "MultivariateAnalysis"
)

# PCA ==========================================================================
#' PCA Results
#'
#' An S4 class to store the results of a principal components analysis.
#' @slot center A \code{\link{numeric}} vector giving the column mean of the
#'  initial dataset (active individuals only).
#' @slot standard_deviation A \code{\link{numeric}} vector giving the column
#'  standard deviations of the initial dataset (active individuals only).
#' @example inst/examples/ex-pca.R
#' @author N. Frerebeau
#' @family multivariate analysis
#' @docType class
#' @export .PCA
#' @exportClass PCA
#' @aliases PCA-class
.PCA <- setClass(
  Class = "PCA",
  slots = c(
    center = "numeric",
    standard_deviation = "numeric"
  ),
  contains = "MultivariateAnalysis"
)
