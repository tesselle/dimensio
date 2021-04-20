# CLASSES DEFINITION

# MultivariateAnalysis =========================================================
## Results ---------------------------------------------------------------------
#' Multivariate Data Analysis Results
#'
#' An S4 class to store the results of a multivariate data analysis.
#' @slot names A [`character`] vector specifying the row names.
#' @slot principal A [`numeric`] [`matrix`] giving the principal coordinates.
#' @slot standard A [`numeric`] [`matrix`] giving the standard coordinates.
#' @slot contributions A [`numeric`] [`matrix`] giving the contributions to the
#'  definition of the dimensions.
#' @slot cosine A [`numeric`] [`matrix`] giving the \eqn{cos^2}{cos2} values.
#' @slot distances A [`numeric`] vector giving the distances to centroid.
#' @slot weights A [`numeric`] vector giving the masses/weights.
#' @slot supplement A [`logical`] vector specifying the supplementary points.
#' @slot order An [`integer`] vector giving the original indices of the data
#'  (computation moves all supplementary points at the end of the results).
#' @slot groups A [`character`] vector specifying the class for each
#'  observation.
#' @author N. Frerebeau
#' @family class
#' @docType class
#' @name MultivariateResults
#' @aliases MultivariateResults-class
.MultivariateResults <- setClass(
  Class = "MultivariateResults",
  slots = c(
    names = "character",
    principal = "matrix",
    standard = "matrix",
    contributions = "matrix",
    cosine = "matrix",
    distances = "numeric",
    weights = "numeric",
    supplement = "logical",
    order = "integer",
    groups = "character"
  )
)

## Output ----------------------------------------------------------------------
#' Output of Multivariate Data Analysis
#'
#' A virtual S4 class to store the output of a multivariate data analysis.
#' @slot data A [`numeric`] [`matrix`].
#' @slot dimension An [`integer`] giving the dimension of the solution.
#' @slot singular_values A [`numeric`] vector giving the singular values.
#' @slot rows A [`MultivariateResults-class`] object.
#' @slot columns A [`MultivariateResults-class`] object.
#' @section Subset:
#'  In the code snippets below, `x` is a `MultivariateAnalysis` object.
#'  \describe{
#'   \item{`x[[i]]`}{Extracts information from a slot selected by subscript `i`.
#'   `i` is a length-one [`character`] vector.}
#'  }
#' @author N. Frerebeau
#' @family class
#' @docType class
#' @name MultivariateAnalysis
#' @aliases MultivariateAnalysis-class
.MultivariateAnalysis <- setClass(
  Class = "MultivariateAnalysis",
  slots = c(
    data = "matrix",
    dimension = "integer",
    singular_values = "numeric",
    rows = "MultivariateResults",
    columns = "MultivariateResults"
  ),
  contains = "VIRTUAL"
)

## Bootstrap -------------------------------------------------------------------
#' Output of Bootstrap Replications
#'
#' A virtual S4 class to store the output of a bootstrap analysis.
#' @slot replications An [`integer`] giving the number of bootstrap
#'  replications.
#' @author N. Frerebeau
#' @family class
#' @docType class
#' @name MultivariateBootstrap
#' @aliases MultivariateBootstrap-class
.MultivariateBootstrap <- setClass(
  Class = "MultivariateBootstrap",
  slots = c(
    replications = "integer"
  ),
  contains = "VIRTUAL"
)

## Summary ---------------------------------------------------------------------
#' Summary of Multivariate Data Analysis
#'
#' A virtual S4 class to store the summary of a multivariate data analysis.
#' @slot data A [`numeric`] [`matrix`].
#' @slot eigenvalues A [`numeric`] [`matrix`].
#' @slot results A [`numeric`] [`matrix`].
#' @slot supplement A [`logical`] vector specifying the supplementary points.
#' @slot margin An [`integer`].
#' @author N. Frerebeau
#' @family class
#' @docType class
#' @name MultivariateSummary
#' @aliases MultivariateSummary-class
.MultivariateSummary <- setClass(
  Class = "MultivariateSummary",
  slots = c(
    data = "matrix",
    eigenvalues = "matrix",
    results = "matrix",
    supplement = "logical",
    margin = "integer"
  ),
  contains = "VIRTUAL"
)

#' @rdname MultivariateSummary
#' @aliases SummaryCA-class
.SummaryCA <- setClass(
  Class = "SummaryCA",
  contains = "MultivariateSummary"
)

#' @rdname MultivariateSummary
#' @aliases SummaryPCA-class
.SummaryPCA <- setClass(
  Class = "SummaryPCA",
  contains = "MultivariateSummary"
)

# CA ===========================================================================
#' CA Results
#'
#' An S4 class to store the results of a simple correspondence analysis.
#' @example inst/examples/ex-ca.R
#' @author N. Frerebeau
#' @family class
#' @docType class
#' @exportClass CA
#' @aliases CA-class
.CA <- setClass(
  Class = "CA",
  contains = "MultivariateAnalysis"
)

#' @rdname CA-class
#' @aliases BootstrapCA-class
.BootstrapCA <- setClass(
  Class = "BootstrapCA",
  contains = c("MultivariateBootstrap", "CA")
)

# PCA ==========================================================================
#' PCA Results
#'
#' An S4 class to store the results of a principal components analysis.
#' @slot center A [`numeric`] vector giving the column mean of the initial
#'  dataset (active individuals only).
#' @slot scale A [`numeric`] vector giving the column standard deviations of the
#'  initial dataset (active individuals only).
#' @example inst/examples/ex-pca.R
#' @author N. Frerebeau
#' @family class
#' @docType class
#' @exportClass PCA
#' @aliases PCA-class
.PCA <- setClass(
  Class = "PCA",
  slots = c(
    center = "numeric",
    scale = "numeric"
  ),
  contains = "MultivariateAnalysis"
)

#' @rdname PCA-class
#' @aliases BootstrapPCA-class
.BootstrapPCA <- setClass(
  Class = "BootstrapPCA",
  contains = c("MultivariateBootstrap", "PCA")
)

# Initialize ===================================================================
build_results <- function(names, principal, standard, contributions,
                          distances, cosine, weights, supplement) {
  ## /!\ Reorder active/supplementary points /!\
  ## Computation moves all supplementary points at the end of the results
  new_i <- seq_len(nrow(principal))
  if (any(supplement)) {
    new_i <- c(new_i[!supplement], new_i[supplement])
    names <- names[new_i]
  }

  ## Prepare names
  # names <- rep(names, length.out = length(supplement))
  col_names <- paste0("F", seq_len(ncol(principal)))
  dim_names0 <- list(names[!supplement], col_names)
  dim_names1 <- list(names, col_names)

  ## Set names
  dimnames(principal) <- dimnames(cosine) <- dim_names1
  dimnames(standard) <- dimnames(contributions) <- dim_names0
  names(distances) <- names
  names(weights) <- names[!supplement]

  .MultivariateResults(
    names = names,
    principal = principal,
    standard = standard,
    contributions = contributions,
    cosine = cosine,
    distances = distances,
    weights = weights,
    supplement = sort(supplement),
    order = new_i
  )
}
