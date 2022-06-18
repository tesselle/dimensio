# GENERIC METHODS
#' @include AllClasses.R
NULL

# S4 dispatch to base S3 generic ===============================================
setGeneric("rownames")
setGeneric("colnames")
setGeneric("dimnames")
setGeneric("loadings")

# Extract ======================================================================
## Get -------------------------------------------------------------------------
#' Get Results
#'
#' Getters to retrieve parts of an object.
#' @param x An object from which to get element(s) (a [`CA-class`] or
#'  [`PCA-class`] object).
#' @return
#'  `loadings()` returns variable loadings (i.e. the coefficients of the linear
#'  combination of the original variables). `loadings()` is only implemented for
#'  consistency with \pkg{[stats][stats::loadings]}.
# @example inst/examples/ex-extract.R
#' @author N. Frerebeau
#' @docType methods
#' @family mutators
#' @name mutators
#' @rdname mutators
NULL

## Subset ----------------------------------------------------------------------
#' Extract Parts of an Object
#'
#' Operators acting on objects to extract parts.
#' @param x An object from which to extract element(s) or in which to replace
#'  element(s).
#' @param i A [`character`] string specifying elements to extract.
#'  Any unambiguous substring can be given (see details).
#' @details
#'  If `i` is "`data`", returns a list with the following elements:
#'  \describe{
#'   \item{`data`}{A [`numeric`] matrix of raw data.}
#'   \item{`mean`}{A [`numeric`] vector giving the variables means (`PCA`).}
#'   \item{`sd`}{A [`numeric`] vector giving the variables standard deviations
#'   (`PCA`).}
#'  }
#'
#'  If `i` is "`rows`", returns a list with the following elements:
#'  \describe{
#'   \item{`coord`}{A [`numeric`] matrix of rows/individuals coordinates.}
#'   \item{`cos2`}{A [`numeric`] matrix of rows/individuals squared cosine.}
#'   \item{`masses`}{A [`numeric`] vector giving the rows masses/individual
#'   weights.}
#'   \item{`sup`}{A [`logical`] vector specifying whether a point is a
#'   supplementary observation or not.}
#'  }
#'
#'  If `i` is "`columns`", returns a list with the following elements:
#'  \describe{
#'   \item{`coord`}{A [`numeric`] matrix of columns/variables coordinates.}
#'   \item{\code{cor}}{A [`numeric`] matrix of correlation between variables and
#'   the dimensions (`PCA`).}
#'   \item{`cos2`}{A [`numeric`] matrix of columns/variables squared cosine.}
#'   \item{`masses`}{A [`numeric`] vector giving the columns masses/variable
#'   weights.}
#'   \item{`sup`}{A [`logical`] vector specifying whether a point is a
#'   supplementary observation or not.}
#'  }
#'
#'  If `i` is "`eigenvalues`", returns a [`numeric`] vector of eigenvalues.
#' @return
#'  A [`list`].
#' @example inst/examples/ex-subset.R
#' @author N. Frerebeau
#' @docType methods
#' @family mutators
#' @name subset
#' @rdname subset
NULL

# CA ===========================================================================
#' Correspondence Analysis
#'
#' Computes a simple correspondence analysis based on the singular value
#' decomposition.
#' @param object A \eqn{m \times p}{m x p} numeric [`matrix`] or a
#'  [`data.frame`].
#' @param rank An [`integer`] value specifying the maximal number of
#'  components to be kept in the results. If `NULL` (the default),
#'  \eqn{min(m, p) - 1} components will be returned.
#' @param sup_row A [`numeric`] or [`logical`] vector specifying the indices of
#'  the supplementary rows.
#' @param sup_col A [`numeric`] or [`logical`] vector specifying the indices of
#'  the supplementary columns.
#' @param ... Currently not used.
#' @return
#'  A [`CA-class`] object.
#' @example inst/examples/ex-ca.R
#' @seealso [svd()]
#' @references
#'  Greenacre, Michael J. *Theory and Applications of Correspondence
#'  Analysis*. London: Academic Press, 1984.
#'
#'  Lebart, L., Piron, M. and Morineau, A. *Statistique exploratoire
#'  multidimensionnelle: visualisation et inférence en fouille de données*.
#'  Paris: Dunod, 2006.
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
#' @param object A \eqn{m \times p}{m x p} numeric [`matrix`] or a
#'  [`data.frame`].
#' @param center A [`logical`] scalar: should the variables be shifted to be
#'  zero centered?
#' @param scale A [`logical`] scalar: should the variables be scaled to unit
#'  variance?
#' @param rank An [`integer`] value specifying the maximal number of components
#'  to be kept in the results. If `NULL` (the default), \eqn{p - 1} components
#'  will be returned.
#' @param sup_row A [`numeric`] or [`logical`] vector specifying the indices of
#'  the supplementary rows (individuals).
#' @param sup_col A [`numeric`] or [`logical`] vector specifying the indices of
#'  the supplementary columns (variables).
#' @param weight_row A [`numeric`] vector specifying the active row (individual)
#'  weights. If `NULL` (the default), no weights are used.
#' @param weight_col A [`numeric`] vector specifying the active column
#'  (variable) weights. If `NULL` (the default), no weights are used.
#' @param ... Currently not used.
#' @return
#'  A [`PCA-class`] object.
#' @example inst/examples/ex-pca.R
#' @seealso [svd()]
#' @references
#'  Lebart, L., Piron, M. and Morineau, A. *Statistique exploratoire
#'  multidimensionnelle: visualisation et inférence en fouille de données*.
#'  Paris: Dunod, 2006.
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
#' @param object A [`CA-class`] or [`PCA-class`] object.
#' @param newdata An object of supplementary points coercible to a
#'  [`matrix`] for which to compute principal coordinates.
#' @param margin A length-one [`numeric`] vector giving the subscript which the
#'  data will be predicted: `1` indicates individuals/rows (the default), `2`
#'  indicates variables/columns.
#' @return
#'  A [`data.frame`] of coordinates.
#' @example inst/examples/ex-predict.R
#' @author N. Frerebeau
#' @docType methods
#' @family multivariate analysis
#' @name predict
#' @rdname predict
NULL

# Bootstrap ====================================================================
#' Partial Bootstrap Analysis
#'
#' Checks analysis with partial bootstrap resampling.
#' @param object A [`CA-class`] or [`PCA-class`] object.
#' @param n A non-negative [`integer`] giving the number of bootstrap
#'  replications.
#' @return#'
#'  Returns a [`BootstrapCA-class`] or a [`BootstrapPCA-class`] object.
#' @param ... Currently not used.
#' @example inst/examples/ex-bootstrap.R
#' @references
#'  Greenacre, Michael J. *Theory and Applications of Correspondence
#'  Analysis*. London: Academic Press, 1984.
#'
#'  Lebart, L., Piron, M. and Morineau, A. *Statistique exploratoire
#'  multidimensionnelle: visualisation et inférence en fouille de données*.
#'  Paris: Dunod, 2006.
#' @author N. Frerebeau
#' @docType methods
#' @family resampling methods
#' @name bootstrap
#' @rdname bootstrap
NULL

#' @rdname bootstrap
#' @aliases bootstrap-method
setGeneric(
  name = "bootstrap",
  def = function(object, ...) standardGeneric("bootstrap")
)

# Results ======================================================================
### Data -----------------------------------------------------------------------
#' Get Original Data
#'
#' @param x An object from which to get element(s) (a [`CA-class`] or
#'  [`PCA-class`] object).
#' @param ... Currently not used.
#' @return
#'  Returns a [`data.frame`] of original data.
#' @author N. Frerebeau
#' @docType methods
#' @family mutators
#' @name get_data
#' @rdname get_data
NULL

#' @rdname get_data
#' @aliases get_data-method
setGeneric(
  name = "get_data",
  def = function(x, ...) standardGeneric("get_data"),
  valueClass = "data.frame"
)

## Coordinates -----------------------------------------------------------------
#' Get Coordinates
#'
#' @param x An object from which to get element(s) (a [`CA-class`] or
#'  [`PCA-class`] object).
#' @param margin A length-one [`numeric`] vector giving the subscript which the
#'  data will be returned: `1` indicates individuals/rows (the default), `2`
#'  indicates variables/columns.
#' @param sup_name A [`character`] string specifying the name of the column to
#'  create for supplementary points attribution (see below).
#' @param ... Currently not used.
#' @return
#'  * `get_coordinates()` returns a [`data.frame`] of coordinates. An extra
#'    column (named after `sup_name`) is added specifying whether an observation
#'    is a supplementary point or not.
#'  * `get_replications()` returns an [`array`] of coordinates.
#' @author N. Frerebeau
#' @docType methods
#' @family mutators
#' @name get_coordinates
#' @rdname get_coordinates
NULL

#' @rdname get_coordinates
#' @aliases get_coordinates-method
setGeneric(
  name = "get_coordinates",
  def = function(x, ...) standardGeneric("get_coordinates"),
  valueClass = "data.frame"
)

#' @rdname get_coordinates
#' @aliases get_replications-method
setGeneric(
  name = "get_replications",
  def = function(x, ...) standardGeneric("get_replications"),
  valueClass = "array"
)

#' Visualize Factor Map
#'
#' Plots factor map.
#' @param object,x A [`CA-class`] or [`PCA-class`] object.
#' @param margin A length-one [`numeric`] vector giving the subscript which the
#'  data will be returned: `1` indicates individuals/rows (the default), `2`
#'  indicates variables/columns, `c(1, 2)` indicates rows and columns (`CA`).
#' @param axes A length-two [`numeric`] vector giving the dimensions to be
#'  plotted.
#' @param active A [`logical`] scalar: should the active observations be
#'  plotted?
#' @param sup A [`logical`] scalar: should the supplementary observations be
#'  plotted?
#' @param highlight A [`character`] string giving XXX.
#'  It must be one of "`coordinates`", "`contributions`" or "`cos2`".
#'  Any unambiguous substring can be given.
#'  If `NULL` (the default), no highlighting is applied.
#' @param group A vector of categories specifying the categorical variable from
#'  which to color the individuals (only used if `highlight` is `NULL`; see
#'  below).
#' @param ... Currently not used.
#' @section Aesthetic:
#'  Point shapes and line types are set whether an observation is a
#'  row/individual or a column/variable and is active or supplementary.
#'
#'  Colors are set according to `highlight` and `group`:
#'  \itemize{
#'   \item{If `highlight` is not `NULL`, the color gradient will vary according
#'   to the value of the selected parameter.}
#'   \item{If `group` is a `numeric` vector, the color gradient and size will
#'   vary by the value of `group`.}
#'   \item{If `group` is not a `numeric` vector, the colors will be mapped to
#'   the levels of `group`.}
#'   \item{If both are `NULL` (the default), then the same rule as for shapes is
#'   used.}
#'  }
#' @seealso [ggplot2::ggplot()]
#' @example inst/examples/ex-plot.R
#' @author N. Frerebeau
#' @docType methods
#' @family plot
#' @name plot_coordinates
#' @rdname plot_coordinates
NULL

#' @rdname plot_coordinates
#' @aliases plot_rows-method
setGeneric(
  name = "plot_rows",
  def = function(object, ...) standardGeneric("plot_rows")
)

#' @rdname plot_coordinates
#' @aliases plot_columns-method
setGeneric(
  name = "plot_columns",
  def = function(object, ...) standardGeneric("plot_columns")
)

## Eigenvalues -----------------------------------------------------------------
#' Get Eigenvalues
#'
#' @param x An object from which to get element(s) (a [`CA-class`] or
#'  [`PCA-class`] object).
#' @param margin A length-one [`numeric`] vector giving the subscript which the
#'  data will be returned: `1` indicates individuals/rows (the default), `2`
#'  indicates variables/columns.
#' @param digits An [`integer`] indicating the number of decimal places to be
#'  used.
#' @param ... Currently not used.
#' @return
#'  * `get_eigenvalues()` returns a [`data.frame`] with the following columns:
#'    `eigenvalues`, `variance` (percentage of variance) and `cumulative`
#'    (cumulative percentage of variance).
#'  * `get_variance()` returns a [`numeric`] vector giving the percentage of
#'    explained variance of each dimension.
#'  * `get_inertia()` returns a [`numeric`] vector.
#' @author N. Frerebeau
#' @docType methods
#' @family mutators
#' @name get_eigenvalues
#' @rdname get_eigenvalues
NULL

#' @rdname get_eigenvalues
#' @aliases get_eigenvalues-method
setGeneric(
  name = "get_eigenvalues",
  def = function(x) standardGeneric("get_eigenvalues"),
  valueClass = "data.frame"
)

#' @rdname get_eigenvalues
#' @aliases get_inertia-method
setGeneric(
  name = "get_inertia",
  def = function(x, ...) standardGeneric("get_inertia"),
  valueClass = "numeric"
)

#' @rdname get_eigenvalues
#' @aliases get_variance-method
setGeneric(
  name = "get_variance",
  def = function(x, ...) standardGeneric("get_variance"),
  valueClass = "numeric"
)

#' Visualize Eigenvalues
#'
#' Plot eigenvalues or variances histogram.
#' @param object A [`CA-class`] or [`PCA-class`] object.
#' @param variance A [`logical`] scalar: should the percentages of variance be
#'  plotted instead of the eigenvalues?
#' @param cumulative A [`logical`] scalar: should the cumulative percentages of
#'  variance be plotted?
#' @param fill,border A [`character`] string specifying the bars infilling and
#'  border colors.
#' @param color A [`character`] string specifying the line color.
#' @param ... Currently not used.
#' @seealso [ggplot2::ggplot()]
#' @example inst/examples/ex-plot.R
#' @author N. Frerebeau
#' @docType methods
#' @family plot
#' @name plot_eigenvalues
#' @rdname plot_eigenvalues
NULL

#' @rdname plot_eigenvalues
#' @aliases plot_variance-method
setGeneric(
  name = "plot_variance",
  def = function(object, ...) standardGeneric("plot_variance")
)

## Contributions ---------------------------------------------------------------
#' Get Contributions
#'
#' @param x An object from which to get element(s) (a [`CA-class`] or
#'  [`PCA-class`] object).
#' @param margin A length-one [`numeric`] vector giving the subscript which the
#'  data will be returned: `1` indicates individuals/rows (the default), `2`
#'  indicates variables/columns.
#' @param sup_name A [`character`] string specifying the name of the column to
#'  create for supplementary points attribution (see below).
#' @param ... Currently not used.
#' @return
#'  * `get_contributions()` returns a [`data.frame`] of contributions to the
#'    definition of the principal dimensions.
#'  * `get_correlations()` returns a [`data.frame`] of correlations between
#'    variables and dimensions (`PCA`). An extra column (named after `sup_name`)
#'    is added specifying whether an observation is a supplementary point or
#'    not.
#'  * `get_cos2()` returns a [`data.frame`] of \eqn{cos^2}{cos2} values (i.e.
#'    quality of the representation of the points on the factor map). An extra
#'    column (named after `sup_name`) is added specifying whether an observation
#'    is a supplementary point or not.
#' @author N. Frerebeau
#' @docType methods
#' @family mutators
#' @name get_contributions
#' @rdname get_contributions
NULL

#' @rdname get_contributions
#' @aliases get_contributions-method
setGeneric(
  name = "get_contributions",
  def = function(x, ...) standardGeneric("get_contributions"),
  valueClass = "data.frame"
)

#' @rdname get_contributions
#' @aliases get_correlations-method
setGeneric(
  name = "get_correlations",
  def = function(x, ...) standardGeneric("get_correlations"),
  valueClass = "data.frame"
)

#' @rdname get_contributions
#' @aliases get_cos2-method
setGeneric(
  name = "get_cos2",
  def = function(x, ...) standardGeneric("get_cos2"),
  valueClass = "data.frame"
)

#' Visualize Contributions and cos2
#'
#' Plots contributions histogram and \eqn{cos^2}{cos2} scatterplot.
#' @param object A [`CA-class`] or [`PCA-class`] object.
#' @param margin A length-one [`numeric`] vector giving the subscript which the
#'  data will be returned: `1` indicates individuals/rows (the default), `2`
#'  indicates variables/columns.
#' @param axes A length-one [`numeric`] vector giving the dimensions to be
#'  plotted.
#' @param active A [`logical`] scalar: should the active observations be
#'  plotted?
#' @param sup A [`logical`] scalar: should the supplementary observations be
#'  plotted?
#' @param sort A [`logical`] scalar: should the data be sorted?
#' @param decreasing A [`logical`] scalar: should the sort order be decreasing?
#'  Only used if `sort` is `TRUE`.
#' @param limit An [`integer`] specifying the number of top elements to be
#'  displayed.
#' @param fill,border A [`character`] string specifying the bars infilling and
#'  border colors.
#' @param ... Currently not used.
#' @seealso [ggplot2::ggplot()]
#' @example inst/examples/ex-plot.R
#' @author N. Frerebeau
#' @docType methods
#' @family plot
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

## Distances -------------------------------------------------------------------
#' Get Distances
#'
#' @param x An object from which to get element(s) (a [`CA-class`] or
#'  [`PCA-class`] object).
#' @param margin A length-one [`numeric`] vector giving the subscript which the
#'  data will be returned: `1` indicates individuals/rows (the default), `2`
#'  indicates variables/columns.
#' @param ... Currently not used.
#' @return
#'  * `get_distances()` returns a [`numeric`] vector.
#' @author N. Frerebeau
#' @docType methods
#' @family mutators
#' @name get_distances
#' @rdname get_distances
NULL

#' @rdname get_distances
#' @aliases get_distances-method
setGeneric(
  name = "get_distances",
  def = function(x, ...) standardGeneric("get_distances"),
  valueClass = "numeric"
)

# Summarize ====================================================================
#' Object Summaries
#'
#' Provides a summary of the results of a multivariate data analysis.
#' @param object A [`CA-class`] or [`PCA-class`] object.
#' @param margin A length-one [`numeric`] vector giving the subscript which the
#'  data will be summarized: `1` indicates individuals/rows (the default), `2`
#'  indicates variables/columns.
#' @param rank An [`integer`] value specifying the maximal number of components
#'  to be kept in the results.
#' @param active A [`logical`] scalar: should the active observations be
#'  summarized?
#' @param sup A [`logical`] scalar: should the supplementary observations be
#'  summarized?
#' @example inst/examples/ex-summary.R
#' @author N. Frerebeau
#' @docType methods
#' @family summary
#' @name summary
#' @rdname summary
NULL
