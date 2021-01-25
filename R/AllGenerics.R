# GENERIC METHODS
#' @include AllClasses.R
NULL

# S4 dispatch to base S3 generic ===============================================
setGeneric("loadings")

# Extract ======================================================================
## Get -------------------------------------------------------------------------
#' Get Results
#'
#' Getters to retrieve parts of an object.
#' @param x An object from which to get element(s) (a \linkS4class{CA} or
#'  \linkS4class{PCA} object).
#' @param margin A length-one \code{\link{numeric}} vector giving the subscript
#'  which the data will be returned: \code{1} indicates individuals/rows (the
#'  default), \code{2} indicates variables/columns.
#' @param sup A \code{\link{logical}} scalar: should supplementary points be
#'  returned?
#' @param sup_name A \code{\link{character}} string specifying the name of the
#'  column to create for supplementary points attribution (see below).
#' @param digits An \code{\link{integer}} indicating the number of decimal
#'  places to be used.
#' @param ... Currently not used.
#' @details
#'  \code{get_data()} returns a \code{data.frame} of original data.
#'
#'  \code{get_contributions()} returns a \code{data.frame} of contributions to
#'  the definition of the principal dimensions.
#'
#'  \code{get_coordinates()} returns a \code{data.frame} of coordinates. If
#'  \code{sup} is \code{TRUE}, an extra column (named after \code{sup_name}) is
#'  added specifying whether an observation is a supplementary point or not.
#'
#'  \code{get_correlations()} returns a \code{data.frame} of correlations
#'  between variables and dimensions (\code{PCA}).
#'
#'  \code{get_cos2()} returns a \code{data.frame} of \eqn{cos^2}{cos2} values
#'  (i.e. quality of the representation of the points on the factor map).
#'
#'  \code{get_eigenvalues()} returns a \code{data.frame} with the following
#'  columns: \code{eigenvalues}, \code{variance} (percentage of variance) and
#'  \code{cumulative} (cumulative percentage of variance).
#'
#'  \code{get_variance} returns a \code{numeric} vector giving the percentage
#'  of explained variance of each dimension.
#'
#'  \code{loadings} returns variable loadings (i.e. the coefficients of the
#'  linear combination of the original variables).
#' @return
#'  \code{get_*()} returns a \code{\link{numeric}} vector or matrix or a
#'  \code{\link{data.frame}}.
#'
#'  \code{loadings()} returns of a \code{\link{matrix}} of class
#'  \code{\link[stats]{loadings}}.
# @example inst/examples/ex-extract.R
#' @author N. Frerebeau
#' @docType methods
#' @family mutator
#' @name mutator
#' @rdname mutator
#' @aliases get set
NULL

#' @rdname mutator
#' @aliases get_coordinates-method
setGeneric(
  name = "get_coordinates",
  def = function(x, ...) standardGeneric("get_coordinates"),
  valueClass = "data.frame"
)

#' @rdname mutator
#' @aliases get_contributions-method
setGeneric(
  name = "get_contributions",
  def = function(x, ...) standardGeneric("get_contributions"),
  valueClass = "data.frame"
)

#' @rdname mutator
#' @aliases get_correlations-method
setGeneric(
  name = "get_correlations",
  def = function(x, ...) standardGeneric("get_correlations"),
  valueClass = "data.frame"
)

#' @rdname mutator
#' @aliases get_cos2-method
setGeneric(
  name = "get_cos2",
  def = function(x, ...) standardGeneric("get_cos2"),
  valueClass = "data.frame"
)

#' @rdname mutator
#' @aliases get_data-method
setGeneric(
  name = "get_data",
  def = function(x, ...) standardGeneric("get_data"),
  valueClass = "data.frame"
)

#' @rdname mutator
#' @aliases get_distances-method
setGeneric(
  name = "get_distances",
  def = function(x, ...) standardGeneric("get_distances"),
  valueClass = "numeric"
)

#' @rdname mutator
#' @aliases get_eigenvalues-method
setGeneric(
  name = "get_eigenvalues",
  def = function(x) standardGeneric("get_eigenvalues"),
  valueClass = "data.frame"
)

#' @rdname mutator
#' @aliases get_inertia-method
setGeneric(
  name = "get_inertia",
  def = function(x, ...) standardGeneric("get_inertia"),
  valueClass = "numeric"
)

#' @rdname mutator
#' @aliases get_variance-method
setGeneric(
  name = "get_variance",
  def = function(x, ...) standardGeneric("get_variance"),
  valueClass = "numeric"
)

## Subset ----------------------------------------------------------------------
#' Extract Parts of an Object
#'
#' Operators acting on objects to extract parts.
#' @param x An object from which to extract element(s) or in which to replace
#'  element(s).
#' @param i A \code{\link{character}} string specifying elements to extract.
#'  Any unambiguous substring can be given (see details).
#' @details
#'  If \code{i} is "\code{data}", returns a list with the following elements:
#'  \describe{
#'   \item{\code{data}}{A \code{\link{numeric}} matrix of raw data.}
#'   \item{\code{mean}}{A \code{\link{numeric}} vector giving the variables
#'   means (\code{PCA}).}
#'   \item{\code{sd}}{A \code{\link{numeric}} vector giving the variables
#'   standard deviations (\code{PCA}).}
#'  }
#'
#'  If \code{i} is "\code{rows}" (\code{CA}) or "\code{individuals}"
#'  (\code{PCA}), returns a list with the following elements:
#'  \describe{
#'   \item{\code{coord}}{A \code{\link{numeric}} matrix of rows/individuals
#'   coordinates.}
#'   \item{\code{cos2}}{A \code{\link{numeric}} matrix of rows/individuals
#'   squared cosine.}
#'   \item{\code{masses}}{A \code{\link{numeric}} vector giving the rows masses
#'   (\code{CA}).}
#'   \item{\code{weights}}{A \code{\link{numeric}} vector giving the individuals
#'   weights (\code{PCA}).}
#'   \item{\code{sup}}{A \code{\link{logical}} vector specifying whether a
#'   point is a supplementary observation or not.}
#'  }
#'
#'  If \code{i} is "\code{columns}" (\code{CA}) or "\code{variables}"
#'  (\code{PCA}), returns a list with the following elements:
#'  \describe{
#'   \item{\code{coord}}{A \code{\link{numeric}} matrix of columns/variables
#'   coordinates.}
#'   \item{\code{cor}}{A \code{\link{numeric}} matrix of correlation between
#'   variables and the dimensions (\code{PCA}).}
#'   \item{\code{cos2}}{A \code{\link{numeric}} matrix of columns/variables
#'   squared cosine.}
#'   \item{\code{masses}}{A \code{\link{numeric}} vector giving the columns
#'   masses (\code{CA}).}
#'   \item{\code{weights}}{A \code{\link{numeric}} vector giving the variables
#'   weights (\code{PCA}).}
#'   \item{\code{sup}}{A \code{\link{logical}} vector specifying whether a
#'   point is a supplementary observation or not.}
#'  }
#'
#'  If \code{i} is "\code{eigenvalues}", returns a \code{\link{numeric}} vector
#'  of eigenvalues.
#' @return
#'  A \code{\link{list}}.
#' @example inst/examples/ex-subset.R
#' @author N. Frerebeau
#' @docType methods
#' @family mutator
#' @name subset
#' @rdname subset
NULL

# CA ===========================================================================
#' Correspondence Analysis
#'
#' Computes a simple correspondence analysis based on the singular value
#' decomposition.
#' @param object A \eqn{m \times p}{m x p} numeric \code{\link{matrix}} or a
#'  \code{\link{data.frame}}.
#' @param n An \code{\link{integer}} value specifying the number of dimensions
#'  to be kept in the results. If \code{NULL} (the default),
#'  \eqn{min(m, p) - 1} dimensions will be returned.
#' @param sup_row A \code{\link{numeric}} or \code{\link{logical}} vector
#'  specifying the indices of the supplementary rows.
#' @param sup_col A \code{\link{numeric}} or \code{\link{logical}} vector
#'  specifying the indices of the supplementary columns.
#' @param ... Currently not used.
#' @return
#'  A \linkS4class{CA} object.
#' @example inst/examples/ex-ca.R
#' @seealso \link{mutator}, \link{predict}, \link{svd}
#' @author N. Frerebeau
#' @docType methods
#' @family multivariate analysis
#' @name ca
#' @rdname ca
NULL

#' @rdname ca
#' @aliases ca-method
setGeneric(
  name = "ca",
  def = function(object, ...) standardGeneric("ca"),
  valueClass = "CA"
)

# PCA ==========================================================================
#' Principal Components Analysis
#'
#' Computes a principal components analysis based on the singular value
#' decomposition.
#' @param object A \eqn{m \times p}{m x p} numeric \code{\link{matrix}} or a
#'  \code{\link{data.frame}}.
#' @param scale A \code{\link{logical}} scalar: should data be scaled to unit
#'  variance?
#' @param n An \code{\link{integer}} value specifying the number of dimensions
#'  to be kept in the results. If \code{NULL} (the default),
#'  \eqn{p - 1} dimensions will be returned.
#' @param sup_ind A \code{\link{numeric}} or \code{\link{logical}} vector
#'  specifying the indices of the supplementary individuals.
#' @param sup_var A \code{\link{numeric}} or \code{\link{logical}} vector
#'  specifying the indices of the supplementary variables.
#' @param weight_ind A \code{\link{numeric}} vector specifying the active
#'  individual weights. If \code{NULL} (the default), no weights are used.
#' @param weight_var A \code{\link{numeric}} vector specifying the active
#'  individual weights. If \code{NULL} (the default), no weights are used.
#' @param ... Currently not used.
#' @return
#'  A \linkS4class{PCA} object.
#' @example inst/examples/ex-pca.R
#' @seealso \link{mutator}, \link{predict}, \link{svd}
#' @author N. Frerebeau
#' @docType methods
#' @family multivariate analysis
#' @name pca
#' @rdname pca
NULL

#' @rdname pca
#' @aliases pca-method
setGeneric(
  name = "pca",
  def = function(object, ...) standardGeneric("pca"),
  valueClass = "PCA"
)

# Predict ======================================================================
#' Predict New Coordinates
#'
#' Predict the projection of new individuals/rows or variables/columns.
#' @param object A \linkS4class{CA} or \linkS4class{PCA} object.
#' @param newdata An object of supplementary points coercible to a
#'  \code{\link{matrix}} for which to compute principal coordinates.
#' @param margin A length-one \code{\link{numeric}} vector giving the subscript
#'  which the data will be returned: \code{1} indicates individuals/rows (the
#'  default), \code{2} indicates variables/columns.
#' @return
#'  A \code{\link{data.frame}} of coordinates.
#' @example inst/examples/ex-predict.R
#' @author N. Frerebeau
#' @docType methods
#' @family multivariate analysis
#' @name predict
#' @rdname predict
NULL

# Plot =========================================================================
## Coordinates -----------------------------------------------------------------
#' Visualize Factor Map
#'
#' Plots factor map.
#' @param object A \linkS4class{CA} or \linkS4class{PCA} object.
#' @param margin A length-one \code{\link{numeric}} vector giving the subscript
#'  which the data will be returned: \code{1} indicates individuals/rows (the
#'  default), \code{2} indicates variables/columns, \code{c(1, 2)} indicates
#'  rows and columns (\code{CA}).
#' @param axes A length-two \code{\link{numeric}} vector giving the dimensions
#'  to be plotted.
#' @param active A \code{\link{logical}} scalar: should the active
#'  observations be plotted?
#' @param sup A \code{\link{logical}} scalar: should the supplementary
#'  observations be plotted?
#' @param select NULL
#' @param group  A vector of categories specifying the categorical
#'  variable from which to color the individuals (see details).
#' @param ... Currently not used.
#' @details
#'  Point shapes and line types are set whether an observation is a
#'  row/individual or a column/variable and is active or supplementary.
#'
#'  Colours are set according to \code{group} (if \code{NULL}, the same rule as
#'  for shapes is used).
#' @example inst/examples/ex-plot.R
#' @author N. Frerebeau
#' @docType methods
#' @family visualization
#' @name plot_coordinates
#' @rdname plot_coordinates
NULL

#' @rdname plot_coordinates
#' @aliases plot_coordinates-method
setGeneric(
  name = "plot_coordinates",
  def = function(object, ...) standardGeneric("plot_coordinates")
)

#' @rdname plot_coordinates
#' @aliases plot_individuals-method
setGeneric(
  name = "plot_individuals",
  def = function(object, ...) standardGeneric("plot_individuals")
)

#' @rdname plot_coordinates
#' @aliases plot_variables-method
setGeneric(
  name = "plot_variables",
  def = function(object, ...) standardGeneric("plot_variables")
)

## Eigenvalues -----------------------------------------------------------------
#' Visualize Eigenvalues
#'
#' Plot eigenvalues or variances histogram.
#' @param object A \linkS4class{CA} or \linkS4class{PCA} object.
#' @param variance A \code{\link{logical}} scalar: should percentages of
#'  variance be plotted instead of eigenvalues?
#' @param cumulative A \code{\link{logical}} scalar: should cumulative
#'  percentages of variance be plotted ? Only used if \code{variance} is
#'  \code{TRUE}.
#' @param fill,border A \code{\link{character}} string specifying the bars
#'  infilling and border colours.
#' @param colour A \code{\link{character}} string specifying the line colour.
#' @param ... Currently not used.
#' @example inst/examples/ex-plot.R
#' @author N. Frerebeau
#' @docType methods
#' @family visualization
#' @name plot_eigenvalues
#' @rdname plot_eigenvalues
NULL

#' @rdname plot_eigenvalues
#' @aliases plot_eigenvalues-method
setGeneric(
  name = "plot_eigenvalues",
  def = function(object, ...) standardGeneric("plot_eigenvalues")
)

#' @rdname plot_eigenvalues
#' @aliases plot_variance-method
setGeneric(
  name = "plot_variance",
  def = function(object, ...) standardGeneric("plot_variance")
)

## Contributions ---------------------------------------------------------------
#' Visualize Contributions and cos2
#'
#' Plots eigenvalues and variances.
#' @param object A \linkS4class{CA} or \linkS4class{PCA} object.
#' @param margin A length-one \code{\link{numeric}} vector giving the subscript
#'  which the data will be returned: \code{1} indicates individuals/rows (the
#'  default), \code{2} indicates variables/columns.
#' @param axes A length-one \code{\link{numeric}} vector giving the dimensions
#'  to be plotted.
#' @param sup A \code{\link{logical}} scalar: should the supplementary
#'  observations be plotted instead of active observations?
#' @param sort A \code{\link{logical}} scalar: should the data be sorted?
#' @param decreasing A \code{\link{logical}} scalar: should the sort order be
#'  decreasing? Only used if \code{sort} is \code{TRUE}.
#' @param limit An \code{\link{integer}} specifying the number of top elements
#'  to be displayed.
#' @param active A \code{\link{logical}} scalar: should the active
#'  observations be plotted?
#' @param sup A \code{\link{logical}} scalar: should the supplementary
#'  observations be plotted?
#' @param fill,border A \code{\link{character}} string specifying the bars
#'  infilling and border colours.
#' @param ... Currently not used.
#' @example inst/examples/ex-plot.R
#' @author N. Frerebeau
#' @docType methods
#' @family visualization
#' @name plot_contributions
#' @rdname plot_contributions
NULL

#' @rdname plot_contributions
#' @aliases plot_contributions-method
setGeneric(
  name = "plot_contributions",
  def = function(object, ...) standardGeneric("plot_contributions")
)

#' @rdname plot_contributions
#' @aliases plot_cos2-method
setGeneric(
  name = "plot_cos2",
  def = function(object, ...) standardGeneric("plot_cos2")
)
