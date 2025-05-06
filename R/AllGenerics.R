# GENERIC METHODS
#' @include AllClasses.R
NULL

# S4 dispatch to base S3 generic ===============================================
setGeneric("rownames")
setGeneric("colnames")
setGeneric("dimnames")
setGeneric("loadings")
setGeneric("biplot")

# Import S4 generics ===========================================================
#' @importMethodsFrom arkhe bootstrap
#' @importMethodsFrom arkhe describe
NULL

# Extract ======================================================================
## Dimnames --------------------------------------------------------------------
#' Dimnames of an Object
#'
#' Retrieve or set the dimnames of an object.
#' @param x An object from which to retrieve the row or column names
#'  (a [`CA-class`] or [`PCA-class`] object).
#' @param do.NULL A [`logical`] scalar. If `FALSE` and names are `NULL`, names
#'  are created.
#' @param prefix A [`character`] string specifying the prefix for created names.
#' @author N. Frerebeau
#' @docType methods
#' @family mutators
#' @name dimnames
#' @rdname dimnames
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
#' @param object A \eqn{m \times p}{m x p} `numeric` [`matrix`] or a
#'  [`data.frame`].
#' @param rank An [`integer`] value specifying the maximal number of
#'  components to be kept in the results. If `NULL` (the default),
#'  \eqn{min(m, p) - 1} components will be returned.
#' @param sup_row A `vector` specifying the indices of the supplementary rows.
#' @param sup_col A `vector` specifying the indices of the supplementary columns.
#' @param sup_quali A `vector` specifying the indices of the supplementary
#'  qualitative columns.
#' @param autodetect A [`logical`] scalar: should non-numeric variables be
#'  automatically removed?
#' @param ... Currently not used.
#' @return
#'  A [`CA-class`] object.
#' @example inst/examples/ex-ca.R
#' @seealso [svd()]
#' @references
#'  Greenacre, M. J. *Theory and Applications of Correspondence Analysis*.
#'  London: Academic Press, 1984.
#'
#'  Greenacre, M. J. *Correspondence Analysis in Practice*. Seconde edition.
#'  Interdisciplinary Statistics Series. Boca Raton: Chapman & Hall/CRC, 2007.
#'
#'  Lebart, L., Piron, M. and Morineau, A. *Statistique exploratoire
#'  multidimensionnelle: visualisation et inférence en fouille de données*.
#'  Paris: Dunod, 2006.
#' @author N. Frerebeau
#' @docType methods
#' @family multivariate analysis
#' @aliases ca-method
setGeneric(
  name = "ca",
  def = function(object, ...) standardGeneric("ca"),
  valueClass = "CA"
)

# MCA ==========================================================================
#' Multiple Correspondence Analysis
#'
#' Computes a multiple correspondence analysis.
#' @param object A \eqn{m \times p}{m x p} `numeric` [`matrix`] or a
#'  [`data.frame`].
#' @param rank An [`integer`] value specifying the maximal number of
#'  components to be kept in the results. If `NULL` (the default),
#'  \eqn{min(m, p) - 1} components will be returned.
#' @param sup_row A `vector` specifying the indices of the supplementary rows.
#' @param sup_col A `vector` specifying the indices of the supplementary
#'  categorical columns.
#' @param sup_quanti A `vector` specifying the indices of the supplementary
#'  quantitative columns.
#' @param autodetect A [`logical`] scalar: should numeric variables be
#'  automatically removed (except `sup_quanti`)?
#' @param ... Currently not used.
#' @return
#'  A [`MCA-class`] object.
# @example inst/examples/ex-mca.R
#' @seealso [svd()], [cdt()]
#' @references
#'  Lebart, L., Piron, M. and Morineau, A. *Statistique exploratoire
#'  multidimensionnelle: visualisation et inférence en fouille de données*.
#'  Paris: Dunod, 2006.
#' @author N. Frerebeau
#' @docType methods
#' @family multivariate analysis
#' @aliases mca-method
setGeneric(
  name = "mca",
  def = function(object, ...) standardGeneric("mca"),
  valueClass = "MCA"
)

# PCA ==========================================================================
#' Principal Components Analysis
#'
#' Computes a principal components analysis based on the singular value
#' decomposition.
#' @param object A \eqn{m \times p}{m x p} `numeric` [`matrix`] or a
#'  [`data.frame`].
#' @param center A [`logical`] scalar: should the variables be shifted to be
#'  zero centered?
#' @param scale A [`logical`] scalar: should the variables be scaled to unit
#'  variance?
#' @param rank An [`integer`] value specifying the maximal number of components
#'  to be kept in the results. If `NULL` (the default), \eqn{p - 1} components
#'  will be returned.
#' @param sup_row A `vector` specifying the indices of the supplementary rows.
#' @param sup_col A `vector` specifying the indices of the supplementary columns.
#' @param sup_quali A `vector` specifying the indices of the supplementary
#'  qualitative columns.
#' @param weight_row A [`numeric`] vector specifying the active row (individual)
#'  weights. If `NULL` (the default), uniform weights are used. Row weights are
#'  internally normalized to sum 1
#' @param weight_col A [`numeric`] vector specifying the active column
#'  (variable) weights. If `NULL` (the default), uniform weights (1) are
#'  used.
#' @param autodetect A [`logical`] scalar: should non-numeric variables be
#'  automatically removed (except `sup_quali`)?
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
#' @aliases pca-method
setGeneric(
  name = "pca",
  def = function(object, ...) standardGeneric("pca"),
  valueClass = "PCA"
)

# PCoA =========================================================================
#' Principal Coordinates Analysis
#'
#' Computes classical (metric) multidimensional scaling.
#' @param object A [distance structure][stats::dist()].
#' @param rank An [`integer`] value specifying the maximal number dimension of
#'  the space which the data are to be represented in.
#' @param ... Currently not used.
#' @return
#'  A [`PCOA-class`] object.
#' @references
#'  Gower, J. C. (1966). Some Distance Properties of Latent Root and Vector
#'  Methods Used in Multivariate Analysis. *Biometrika*, 53(3‑4): 325-338.
#'  \doi{10.1093/biomet/53.3-4.325}.
#' @example inst/examples/ex-pcoa.R
#' @seealso [stats::cmdscale()]
#' @author N. Frerebeau
#' @docType methods
#' @family multivariate analysis
#' @aliases pcoa-method
setGeneric(
  name = "pcoa",
  def = function(object, ...) standardGeneric("pcoa"),
  valueClass = "PCOA"
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
#' @return
#'  Returns a [`BootstrapCA-class`] or a [`BootstrapPCA-class`] object.
#' @example inst/examples/ex-bootstrap.R
#' @references
#'  Greenacre, Michael J. *Theory and Applications of Correspondence
#'  Analysis*. London: Academic Press, 1984.
#'
#'  Lebart, L., Piron, M. and Morineau, A. *Statistique exploratoire
#'  multidimensionnelle: visualisation et inférence en fouille de données*.
#'  Paris: Dunod, 2006.
#'
#'  Lockyear, K. (2013). Applying Bootstrapped Correspondence Analysis to
#'  Archaeological Data. *Journal of Archaeological Science*, 40(12): 4744-4753.
#'  \doi{10.1016/j.jas.2012.08.035}.
#'
#'  Ringrose, T. J. (1992). Bootstrapping and Correspondence Analysis in
#'  Archaeology. *Journal of Archaeological Science*, 19(6): 615-629.
#'  \doi{10.1016/0305-4403(92)90032-X}.
#' @author N. Frerebeau
#' @docType methods
#' @family resampling methods
#' @name bootstrap
#' @rdname bootstrap
NULL

# Results ======================================================================
#' Export Results
#'
#' Creates a Zip archive of all results in CSV format.
#' @param object A [`CA-class`], [`MCA-class`] or [`PCA-class`] object.
#' @param file A [`character`] string specifying the pathname of the zip file.
#' @param flags A [`character`] string of flags (see [utils::zip()]).
#' @param ... Currently not used.
#' @example inst/examples/ex-export.R
#' @seealso [utils::write.csv()], [utils::zip()]
#' @author N. Frerebeau
#' @docType methods
#' @family getters
#' @aliases export-method
setGeneric(
  name = "export",
  def = function(object, ...) standardGeneric("export")
)

### Data -----------------------------------------------------------------------
#' Get Original Data
#'
#' @param x An object from which to get element(s) (a [`CA-class`],
#'  [`MCA-class`] or [`PCA-class`] object).
#' @param ... Currently not used.
#' @return
#'  Returns a [`data.frame`] of original data.
#' @author N. Frerebeau
#' @docType methods
#' @family getters
#' @aliases get_data-method
setGeneric(
  name = "get_data",
  def = function(x, ...) standardGeneric("get_data"),
  valueClass = "data.frame"
)

## Coordinates -----------------------------------------------------------------
#' Get Coordinates
#'
#' @param x An object from which to get element(s) (a [`CA-class`],
#'  [`MCA-class`] or [`PCA-class`] object).
#' @param margin A length-one [`numeric`] vector giving the subscript which the
#'  data will be returned: `1` indicates individuals/rows (the default), `2`
#'  indicates variables/columns.
#' @param principal A [`logical`] scalar: should principal coordinates be
#'  returned? If `FALSE`, standard coordinates are returned.
#' @param sup_name A [`character`] string specifying the name of the column to
#'  create for supplementary points attribution (see below).
#' @param ... Currently not used.
#' @return
#'  * `get_coordinates()` returns a [`data.frame`] of coordinates. An extra
#'    column (named after `sup_name`) is added specifying whether an observation
#'    is a supplementary point or not.
#'  * `get_replications()` returns an [`array`] of coordinates.
#' @example inst/examples/ex-coordinates.R
#' @author N. Frerebeau
#' @docType methods
#' @family getters
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

## Eigenvalues -----------------------------------------------------------------
#' Get Eigenvalues
#'
#' @param x An object from which to get element(s) (a [`CA-class`],
#'  [`MCA-class`] or [`PCA-class`] object).
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
#'  * `get_variance()` returns a [`numeric`] vector giving the amount of
#'    variance explained by each (principal) component.
#'  * `get_distance()`returns a [`numeric`] vector of squared distance to the
#'    centroid.
#'  * `get_inertia()` returns a [`numeric`] vector giving the inertia (weighted
#'    squared distance to the centroid).
#' @author N. Frerebeau
#' @docType methods
#' @family getters
#' @aliases get_eigenvalues-method
setGeneric(
  name = "get_eigenvalues",
  def = function(x) standardGeneric("get_eigenvalues"),
  valueClass = "data.frame"
)

#' @rdname get_eigenvalues
#' @aliases get_variance-method
setGeneric(
  name = "get_variance",
  def = function(x, ...) standardGeneric("get_variance"),
  valueClass = "numeric"
)

#' @rdname get_eigenvalues
#' @aliases get_distances-method
setGeneric(
  name = "get_distances",
  def = function(x, ...) standardGeneric("get_distances"),
  valueClass = "numeric"
)

#' @rdname get_eigenvalues
#' @aliases get_inertia-method
setGeneric(
  name = "get_inertia",
  def = function(x, ...) standardGeneric("get_inertia"),
  valueClass = "numeric"
)

## Contributions ---------------------------------------------------------------
#' Get Contributions
#'
#' @param x An object from which to get element(s) (a [`CA-class`],
#'  [`MCA-class`] or [`PCA-class`] object).
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
#'    variables and dimensions. An extra column (named after `sup_name`)
#'    is added specifying whether an observation is a supplementary point or
#'    not.
#'  * `get_cos2()` returns a [`data.frame`] of \eqn{cos^2}{cos2} values (i.e.
#'    quality of the representation of the points on the factor map). An extra
#'    column (named after `sup_name`) is added specifying whether an observation
#'    is a supplementary point or not.
#' @author N. Frerebeau
#' @docType methods
#' @family getters
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

# Plot =========================================================================
#' Plot Coordinates
#'
#' @param x An \R object.
#' @param ... Further [graphical parameters][graphics::par].
#' @inheritParams viz_points
#' @author N. Frerebeau
#' @docType methods
#' @family plot methods
#' @name plot
#' @rdname plot
NULL

## Biplot ----------------------------------------------------------------------
#' Biplot
#'
#' @param x A [`CA-class`], [`MCA-class`] or [`PCA-class`] object.
#' @param axes A length-two [`numeric`] vector giving the dimensions to be
#'  plotted.
#' @param type A [`character`] string specifying the biplot to be plotted
#'  (see below). It must be one of "`rows`", "`columns`", "`contribution`" (CA),
#'  "`form`" or "`covariance`" (PCA). Any unambiguous substring can be given.
#' @param labels A [`character`] vector specifying whether
#'  "`rows`"/"`individuals`" and/or "`columns`"/"`variables`" names must be
#'  drawn. Any unambiguous substring can be given.
#' @param col.rows,col.columns A length-two `vector` of color specification for
#'  the active and supplementary rows/columns.
#' @param pch.rows,pch.columns A length-two `vector` of symbol specification for
#'  the active and supplementary rows/columns.
#' @param lty.columns A length-two `vector` of line type specification for
#'  the active and supplementary columns.
#' @param size A length-two [`numeric`] vector giving range of possible sizes
#'  (greater than 0). Only used if `type` is "`contribution`" (CA).
#' @param xlim A length-two [`numeric`] vector giving the x limits of the plot.
#'  The default value, `NULL`, indicates that the range of the
#'  [finite][is.finite()] values to be plotted should be used.
#' @param ylim A length-two [`numeric`] vector giving the y limits of the plot.
#'  The default value, `NULL`, indicates that the range of the
#'  [finite][is.finite()] values to be plotted should be used.
#' @param main A [`character`] string giving a main title for the plot.
#' @param sub A [`character`] string giving a subtitle for the plot.
#' @param legend A [`list`] of additional arguments to be passed to
#'  [graphics::legend()]; names of the list are used as argument names.
#'  If `NULL`, no legend is displayed.
#' @inheritParams prepare_plot
#' @param ... Currently not used.
#' @details
#'  A biplot is the simultaneous representation of rows and columns of a
#'  rectangular dataset. It is the generalization of a scatterplot to the case
#'  of mutlivariate data: it allows to visualize as much information as possible
#'  in a single graph (Greenacre 2010).
#'
#'  Biplots have the drawbacks of their advantages: they can quickly become
#'  difficult to read as they display a lot of information at once. It may then
#'  be preferable to visualize the results for individuals and variables
#'  separately.
#' @section PCA Biplots:
#'  \describe{
#'   \item{`form` (row-metric-preserving)}{The form biplot favors the
#'   representation of the individuals: the distance between the individuals
#'   approximates the Euclidean distance between rows. In the form biplot the
#'   length of a vector approximates the quality of the representation of the
#'   variable.}
#'   \item{`covariance` (column-metric-preserving)}{The covariance biplot favors
#'   the representation of the variables: the length of a vector approximates
#'   the standard deviation of the variable and the cosine of the angle formed
#'   by two vectors approximates the correlation between the two variables. In
#'   the covariance biplot the distance between the individuals approximates the
#'   Mahalanobis distance between rows.}
#'  }
#' @section CA Biplots:
#'  \describe{
#'   \item{`symetric` (symetric biplot)}{Represents the row and column profiles
#'   simultaneously in a common space: rows and columns are in standard
#'   coordinates. Note that the the inter-distance between any row and column
#'   items is not meaningful (i.e. the proximity between rows and columns cannot
#'   be directly interpreted).}
#'   \item{`rows` (asymetric biplot)}{Row principal biplot (row-metric-preserving)
#'   with rows in principal coordinates and columns in standard coordinates.}
#'   \item{`columns` (asymetric biplot)}{Column principal biplot
#'   (column-metric-preserving) with rows in standard coordinates and columns in
#'   principal coordinates.}
#'   \item{`contribution` (asymetric biplot)}{Contribution biplot with rows in
#'   principal coordinates and columns in standard coordinates multiplied by the
#'   square roots of their masses.}
#'  }
#' @return
#'  `biplot()` is called for its side-effects: it results in a graphic being
#'  displayed. Invisibly returns `x`.
#' @example inst/examples/ex-biplot.R
#' @references
#'  Aitchison, J. and Greenacre, M. J. (2002). Biplots of Compositional Data.
#'  *Journal of the Royal Statistical Society: Series C (Applied Statistics)*,
#'  51(4): 375-92. \doi{10.1111/1467-9876.00275}.
#'
#'  Greenacre, M. J. (2010). *Biplots in Practice*. Bilbao: Fundación BBVA.
#' @author N. Frerebeau
#' @docType methods
#' @family plot methods
#' @name biplot
#' @rdname biplot
NULL

## Coordinates -----------------------------------------------------------------
#' Visualize Individuals Factor Map
#'
#' Plots row/individual principal coordinates.
#' @inheritParams viz_points
#' @param ... Further [graphical parameters][graphics::par].
#' @return
#'  `viz_*()` is called for its side-effects: it results in a graphic
#'  being displayed. Invisibly returns `x`.
#' @example inst/examples/ex-plot.R
#' @author N. Frerebeau
#' @docType methods
#' @family plot methods
#' @aliases viz_individuals-method
setGeneric(
  name = "viz_individuals",
  def = function(x, ...) standardGeneric("viz_individuals")
)

#' @rdname viz_individuals
#' @aliases viz_rows-method
setGeneric(
  name = "viz_rows",
  def = function(x, ...) standardGeneric("viz_rows")
)

#' Visualize Variables Factor Map
#'
#' Plots column/variable principal coordinates.
#' @inheritParams viz_points
#' @param ... Further [graphical parameters][graphics::par].
#' @return
#'  `viz_*()` is called for its side-effects: it results in a graphic
#'  being displayed. Invisibly returns `x`.
#' @example inst/examples/ex-plot.R
#' @author N. Frerebeau
#' @docType methods
#' @family plot methods
#' @aliases viz_variables-method
setGeneric(
  name = "viz_variables",
  def = function(x, ...) standardGeneric("viz_variables")
)

#' @rdname viz_variables
#' @aliases viz_columns-method
setGeneric(
  name = "viz_columns",
  def = function(x, ...) standardGeneric("viz_columns")
)

## Eigenvalues -----------------------------------------------------------------
#' Scree Plot
#'
#' Plot eigenvalues (scree plot) or variances histogram.
#' @param x A [`CA-class`], [`MCA-class`] or [`PCA-class`] object.
#' @param eigenvalues A [`logical`] scalar: should the eigenvalues be plotted
#'  instead of variance/inertia?
#' @param cumulative A [`logical`] scalar: should the cumulative percentages of
#'  variance be plotted?
#' @param labels A [`logical`] scalar: should text labels be drawn on top of
#'  bars?
#' @param limit An [`integer`] specifying the number of top elements to be
#'  displayed.
#' @param col,border A [`character`] string specifying the bars infilling and
#'  border colors.
#' @param col.cumulative A specification for the line color.
#' @param lty.cumulative A specification for the line type.
#' @param lwd.cumulative A specification for the line width.
#' @param ... Extra parameters to be passed to [graphics::barplot()].
#' @return
#'  `screeplot()` is called for its side-effects: it results in a graphic
#'  being displayed. Invisibly returns `x`.
#' @example inst/examples/ex-screeplot.R
#' @author N. Frerebeau
#' @docType methods
#' @family plot methods
#' @aliases screeplot-method
#' @name screeplot
#' @rdname screeplot
NULL

## Contributions ---------------------------------------------------------------
#' Visualize Contributions and cos2
#'
#' Plots contributions histogram and \eqn{cos^2}{cos2} scatterplot.
#' @param x A [`CA-class`], [`MCA-class`] or [`PCA-class`] object.
#' @param margin A length-one [`numeric`] vector giving the subscript which the
#'  data will be returned: `1` indicates individuals/rows (the default), `2`
#'  indicates variables/columns.
#' @param axes A [`numeric`] vector giving the dimensions to be plotted.
#' @param active A [`logical`] scalar: should the active observations be
#'  plotted?
#' @param sup A [`logical`] scalar: should the supplementary observations be
#'  plotted?
#' @param sort A [`logical`] scalar: should the data be sorted?
#' @param decreasing A [`logical`] scalar: should the sort order be decreasing?
#'  Only used if `sort` is `TRUE`.
#' @param limit An [`integer`] specifying the number of top elements to be
#'  displayed.
#' @param horiz A [`logical`] scalar: should the bars be drawn horizontally
#'  with the first at the bottom?
#' @param col,border A [`character`] string specifying the bars infilling and
#'  border colors.
#' @param ... Extra parameters to be passed to [graphics::barplot()].
#' @details
#'  The red dashed line indicates the expected average contribution (variables
#'  with a contribution larger than this cutoff can be considered as important
#'  in contributing to the component).
#' @return
#'  `viz_contributions()` and `viz_cos2()` are called for their side-effects:
#'  they result in a graphic being displayed. Invisibly return `x`.
#' @example inst/examples/ex-contributions.R
#' @author N. Frerebeau
#' @docType methods
#' @family plot methods
#' @aliases viz_contributions-method
setGeneric(
  name = "viz_contributions",
  def = function(x, ...) standardGeneric("viz_contributions")
)

#' @rdname viz_contributions
#' @aliases viz_cos2-method
setGeneric(
  name = "viz_cos2",
  def = function(x, ...) standardGeneric("viz_cos2")
)

# Envelopes ====================================================================
#' Convex Hulls
#'
#' Plots convex hull of a set of observations.
#' @param x,y A [`numeric`] vector. If `y` is missing, `x` must be an object
#'  from which to wrap observations (a [`CA-class`], [`MCA-class`] or
#'  [`PCA-class`] object).
#' @param margin A length-one [`numeric`] vector giving the subscript which the
#'  data will be returned: `1` indicates individuals/rows (the default), `2`
#'  indicates variables/columns.
#' @param axes A length-two [`numeric`] vector giving the dimensions
#'  for which to compute results.
#' @param group A vector specifying the group an observation belongs to.
#' @param color The colors for borders (will be mapped to `group`).
#'  Ignored if set to `FALSE`. If `NULL`, the default color scheme will be used.
#' @param fill The background colors (will be mapped to `group`).
#'  Ignored if set to `FALSE`.
#' @param symbol A vector of symbols (will be mapped to `group`).
#'  Ignored if set to `FALSE`.
#' @param ... Further [graphical parameters][graphics::par] to be passed to
#'  [graphics::polygon()].
#' @return
#'  `wrap_hull()` returns a [`data.frame`] of envelope `x` and `y` coordinates.
#'  An extra column named `group` is added specifying the group an observation
#'  belongs to.
#'
#'  `viz_hull()`is called for its side-effects: it results in a graphic being
#'  displayed. Invisibly returns `x`.
#' @example inst/examples/ex-hull.R
#' @author N. Frerebeau
#' @docType methods
#' @family envelopes
#' @aliases viz_hull-method
setGeneric(
  name = "viz_hull",
  def = function(x, y, ...) standardGeneric("viz_hull")
)

#' @rdname viz_hull
#' @aliases wrap_hull-method
setGeneric(
  name = "wrap_hull",
  def = function(x, y, ...) standardGeneric("wrap_hull")
)

#' Ellipses
#'
#' Plots ellipses.
#' @inheritParams viz_hull
#' @param level A [`numeric`] vector specifying the confidence/tolerance level.
#' @param type A [`character`] string specifying the ellipse to draw.
#'  It must be one of "`tolerance`" or "`confidence`").
#'  Any unambiguous substring can be given.
#' @return
#'  `viz_ellipses()`is called for its side-effects: it results in a graphic
#'  being displayed. Invisibly returns `x`.
#' @example inst/examples/ex-ellipses.R
#' @author N. Frerebeau
#' @docType methods
#' @family envelopes
#' @aliases viz_ellipses-method
setGeneric(
  name = "viz_ellipses",
  def = function(x, y, ...) standardGeneric("viz_ellipses")
)

#' Confidence Ellipses
#'
#' Plots confidence ellipses.
#' @inheritParams viz_ellipses
#' @return
#'  `wrap_confidence()` returns a [`data.frame`] of envelope `x` and `y`
#'  coordinates. An extra column named `group` is added specifying the group an
#'  observation belongs to.
#'
#'  `viz_confidence()`is called for its side-effects: it results in a graphic
#'  being displayed. Invisibly returns `x`.
#' @example inst/examples/ex-confidence.R
#' @author N. Frerebeau
#' @docType methods
#' @family envelopes
#' @aliases viz_confidence-method
setGeneric(
  name = "viz_confidence",
  def = function(x, y, ...) standardGeneric("viz_confidence")
)

#' @rdname viz_confidence
#' @aliases wrap_confidence-method
setGeneric(
  name = "wrap_confidence",
  def = function(x, y, ...) standardGeneric("wrap_confidence")
)

#' Tolerance Ellipses
#'
#' Plots tolerance ellipses.
#' @inheritParams viz_ellipses
#' @return
#'  `wrap_tolerance()` returns a [`data.frame`] of envelope `x` and `y`
#'  coordinates. An extra column named `group` is added specifying the group an
#'  observation belongs to.
#'
#'  `viz_tolerance()`is called for its side-effects: it results in a graphic
#'  being displayed. Invisibly returns `x`.
#' @example inst/examples/ex-tolerance.R
#' @author N. Frerebeau
#' @docType methods
#' @family envelopes
#' @aliases viz_tolerance-method
setGeneric(
  name = "viz_tolerance",
  def = function(x, y, ...) standardGeneric("viz_tolerance")
)

#' @rdname viz_tolerance
#' @aliases wrap_tolerance-method
setGeneric(
  name = "wrap_tolerance",
  def = function(x, y, ...) standardGeneric("wrap_tolerance")
)

# Summarize ====================================================================
#' Object Summaries
#'
#' Provides a summary of the results of a multivariate data analysis.
#' @param object A [`CA-class`], [`MCA-class`] or [`PCA-class`] object.
#' @param axes A length-two [`numeric`] vector giving the dimensions to be
#'  summarized.
#' @param margin A length-one [`numeric`] vector giving the subscript which the
#'  data will be summarized: `1` indicates individuals/rows (the default), `2`
#'  indicates variables/columns.
#' @param rank An [`integer`] value specifying the maximal number of components
#'  to be kept in the results. Deprecated, use `axes` instead.
#' @param active A [`logical`] scalar: should the active observations be
#'  summarized?
#' @param sup A [`logical`] scalar: should the supplementary observations be
#'  summarized?
#' @param x A [`MultivariateSummary-class`] object.
#' @param row.names A [`character`] vector giving the row names for the data
#'  frame, or `NULL`.
#' @param optional A [`logical`] scalar: should the names of the variables in
#'  the data frame be checked? If `FALSE` then the names of the variables in the
#'  data frame are checked to ensure that they are syntactically valid variable
#'  names and are not duplicated.
#' @param ... Currently not used.
#' @example inst/examples/ex-summary.R
#' @author N. Frerebeau
#' @docType methods
#' @family summary
#' @name summary
#' @rdname summary
NULL

#' Object Description
#'
#' @param x A [`CA-class`], [`MCA-class`] or [`PCA-class`] object.
#' @param ... Further parameters to be passed to [cat()].
#' @return
#'  `describe()` is called for its side-effects. Invisibly returns `x`.
#' @example inst/examples/ex-summary.R
#' @author N. Frerebeau
#' @family summary
#' @docType methods
#' @rdname describe
#' @name describe
NULL

#' Tidy Coordinates
#'
#' @param x A [`CA-class`], [`MCA-class`] or [`PCA-class`] object.
#' @param margin A length-one [`numeric`] vector giving the subscript
#'  which the data will be returned: `1` indicates individuals/rows (the
#'  default), `2` indicates variables/columns.
#' @param axes A length-two [`numeric`] vector giving the dimensions
#'  for which to compute results.
#' @param principal A [`logical`] scalar: should principal coordinates be
#'  returned? If `FALSE`, standard coordinates are returned.
#' @param ... Currently not used.
#' @return
#'  `tidy()` returns a long [`data.frame`] with the following columns:
#'    \describe{
#'     \item{`label`}{Row/column names of the original data.}
#'     \item{`component`}{Component.}
#'     \item{`supplementary`}{Whether an observation is active or
#'     supplementary.}
#'     \item{`coordinate`}{Coordinates.}
#'     \item{`contribution`}{Contributions to the definition of the components.}
#'     \item{`cos2`}{\eqn{cos^2}{cos2}.}
#'    }
#'
#'  `augment()` returns a wide [`data.frame`] of the row/column coordinates
#'    along `axes` and the following columns:
#'    \describe{
#'     \item{`label`}{Row/column names of the original data.}
#'     \item{`supplementary`}{Whether an observation is active or
#'     supplementary.}
#'     \item{`mass`}{Weight/mass of each observation.}
#'     \item{`sum`}{Sum of squared coordinates along `axes`.}
#'     \item{`contribution`}{Joint contributions to the definition of `axes`.}
#'     \item{`cos2`}{Joint \eqn{cos^2}{cos2} along `axes`.}
#'    }
#' @example inst/examples/ex-coordinates.R
#' @author N. Frerebeau
#' @docType methods
#' @family summary
#' @aliases tidy-method
setGeneric(
  name = "tidy",
  def = function(x, ...) standardGeneric("tidy"),
  valueClass = "data.frame"
)

#' @rdname tidy
#' @aliases augment-method
setGeneric(
  name = "augment",
  def = function(x, ...) standardGeneric("augment"),
  valueClass = "data.frame"
)

# Tools ========================================================================
#' Complete Disjunctive Table
#'
#' Computes the complete disjunctive table of a factor table.
#' @param object A [`data.frame`].
#' @param exclude A `vector` of values to be excluded when forming the set of
#'  levels (see [factor()]). If `NULL` (the default), will make `NA` an extra
#'  level.
#' @param abbrev A [`logical`] scalar: should the column names be abbreviated?
#'  If `FALSE`, these are of the form 'factor_level' but if `abbrev = TRUE` they
#'  are just 'level' which will suffice if the factors have distinct levels.
#' @param ... Currently not used.
#' @return A [`data.frame`].
#' @example inst/examples/ex-cdt.R
#' @author N. Frerebeau
#' @docType methods
#' @family tools
#' @aliases cdt-method
setGeneric(
  name = "cdt",
  def = function(object, ...) standardGeneric("cdt")
)

#' Burt Table
#'
#' Computes the burt table of a factor table.
#' @param object A [`data.frame`].
#' @inheritParams cdt
#' @param ... Currently not used.
#' @return A symetric [`matrix`].
#' @example inst/examples/ex-cdt.R
#' @author N. Frerebeau
#' @docType methods
#' @family tools
#' @aliases burt-method
setGeneric(
  name = "burt",
  def = function(object, ...) standardGeneric("burt")
)
