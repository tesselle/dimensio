# CLASSES DEFINITION

# MultivariateAnalysis =========================================================
#' Multivariate Data Analysis Results
#'
#' An S4 class to store the results of a multivariate data analysis.
#' @slot names A \code{\link{character}} vector specifying the row names.
#' @slot coordinates A \code{\link{numeric}} \code{\link{matrix}}
#'  giving the principal coordinates.
#' @slot standard A \code{\link{numeric}} \code{\link{matrix}}
#'  giving the standard coordinates.
#' @slot contributions A \code{\link{numeric}} \code{\link{matrix}}
#'  giving the contributions to the definition of the dimensions.
#' @slot cosine A \code{\link{numeric}} \code{\link{matrix}}
#'  giving the \eqn{cos^2}{cos2} values.
#' @slot distances A \code{\link{numeric}} vector giving the distances to
#'  centroid.
#' @slot weights A \code{\link{numeric}} vector giving the masses/weights.
#' @slot supplement A \code{\link{logical}} vector specifying the supplementary
#'  points.
#' @slot order An \code{\link{integer}} vector giving the original indices
#'  of the data (computation moves all supplementary points at the end of the
#'  results).
#' @author N. Frerebeau
#' @family multivariate analysis
#' @docType class
#' @aliases MultivariateResults-class
.MultivariateResults <- setClass(
  Class = "MultivariateResults",
  slots = c(
    names = "character",
    coordinates = "matrix",
    standard = "matrix",
    contributions = "matrix",
    cosine = "matrix",
    distances = "numeric",
    weights = "numeric",
    supplement = "logical",
    order = "integer"
  )
)

#' Output of Multivariate Data Analysis
#'
#' An S4 class to store the output of a multivariate data analysis.
#' @slot data A \code{\link{numeric}} \code{\link{matrix}}
#' @slot dimension An \code{\link{integer}} giving the dimension of the
#'  solution.
#' @slot singular_values A \code{\link{numeric}} vector giving the singular
#'  values.
#' @slot rows A \linkS4class{MultivariateResults} object.
#' @slot columns A \linkS4class{MultivariateResults} object.
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
    singular_values = "numeric",
    rows = "MultivariateResults",
    columns = "MultivariateResults"
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

# Initialize ===================================================================
setMethod(
  f = "initialize",
  signature = "MultivariateResults",
  definition = function(.Object, names, coordinates, standard, contributions,
                        distances, cosine, weights, supplement, ...,
                        prefix = "PC") {

    ## /!\ Reorder active/supplementary points /!\
    ## Computation moves all supplementary points at the end of the results
    new_i <- seq_len(nrow(coordinates))
    if (any(supplement)) {
      new_i <- c(new_i[!supplement], new_i[supplement])
      names <- names[new_i]
    }

    ## Prepare names
    col_names <- paste0(prefix, seq_len(ncol(coordinates)))
    dim_names0 <- list(names[!supplement], col_names)
    dim_names1 <- list(names, col_names)

    ## Set names
    dimnames(coordinates) <- dimnames(cosine) <- dim_names1
    dimnames(standard) <- dimnames(contributions) <- dim_names0
    names(distances) <- names
    names(weights) <- names[!supplement]

    .Object <- methods::callNextMethod(
      .Object,
      names = names,
      coordinates = coordinates,
      standard = standard,
      contributions = contributions,
      cosine = cosine,
      distances = distances,
      weights = weights,
      supplement = sort(supplement),
      order = new_i
    )
    validObject(.Object)
    .Object
  }
)
