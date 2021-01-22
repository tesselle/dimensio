# GENERIC METHODS
#' @include AllClasses.R
NULL

# Extract ======================================================================
#' Get or Set Parts of an Object
#'
#' Getters and setters to retrieve or set parts of an object.
#' @param x An object from which to get or set element(s) (typically a
#'  \linkS4class{CA} object).
#' @param margin A length-one \code{\link{numeric}} vector giving the subscript
#'  which the data will be returned: \code{1} indicates individuals/rows (the
#'  default), \code{2} indicates variables/columns.
#' @param standard A \code{\link{logical}} scalar: should standard coordinates
#'  be returned instead of principal coordinates (the default)?
#' @param sup A \code{\link{logical}} scalar: should supplementary points be
#'  returned?
#' @param sup_name A \code{\link{character}} string specifying the name of the
#'  column to create for supplementary points attribution (see below).
#' @param ... Currently not used.
#' @details
#'  \code{get_eigenvalues()} returns a \code{data.frame} with the following
#'  columns: \code{eigenvalues}, \code{percentage} (percentage of variance) and
#'  \code{cumulative} (cumulative percentage of variance).
#'
#'  \code{get_contribution()} returns a \code{data.frame} of contributions to
#'  the definition of the principal dimensions.
#'
#'  \code{get_coordinates()} returns a \code{data.frame} of coordinates. If
#'  \code{sup} is \code{TRUE}, an extra column (named after \code{sup_name}) is
#'  added specifying whether an observation is a supplementary point or not.
#'
#'  \code{get_cos2()} returns a \code{data.frame} of \eqn{cos^2}{cos2} values
#'  (i.e. quality of the representation of the points on the factor map).
#' @return
#'  \code{get_*()} returns a \code{\link{numeric}} vector or a
#'  \code{\link{data.frame}}.
#'
#'  \code{set_*()} returns an object of the same sort as \code{x} with the new
#'  values assigned.
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
#' @aliases get_cos2-method
setGeneric(
  name = "get_cos2",
  def = function(x, ...) standardGeneric("get_cos2"),
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

# Subset =======================================================================
#' Extract or Replace Parts of an Object
#'
#' Operators acting on objects to extract or replace parts.
#' @param x An object from which to extract element(s) or in which to replace
#'  element(s).
#' @param i A \code{\link{character}} string specifying elements to extract or
#'  replace.
# @param value A possible value for the element(s) of \code{x}.
# @param drop A \code{\link{logical}} scalar: should the result be coerced to
#  the lowest possible dimension? This only works for extracting elements,
#  not for the replacement.
#' @return
#'  A subsetted object of the same sort as \code{x}.
# @example inst/examples/ex-extract.R
#' @author N. Frerebeau
#' @docType methods
#' @family mutator
#' @name subset
#' @rdname subset
NULL

# Correspondence Analysis ======================================================
#' Correspondence Analysis
#'
#' Computes a simple correspondence analysis based on the singular value
#' decomposition.
#' @param object A \code{\link{matrix}} or a \code{\link{data.frame}}.
#' @param n An \code{\link{integer}} value specifying the number of dimensions
#'  to be kept in the results (default to \code{5}).
#' @param sup_rows A \code{\link{numeric}} or \code{\link{logical}} vector
#'  specifying the indices of the supplementary rows.
#' @param sup_columns A \code{\link{numeric}} or \code{\link{logical}} vector
#'  specifying the indices of the supplementary columns.
#' @param ... Currently not used.
#' @return
#'  A \linkS4class{CA} object.
#' @example inst/examples/ex-ca.R
#' @seealso \link{mutator}, \link{predict}
#' @author N. Frerebeau
#' @docType methods
#' @family multivariate analysis
#' @name correspondence
#' @rdname correspondence
NULL

#' @rdname correspondence
#' @aliases ca-method
setGeneric(
  name = "ca",
  def = function(object, ...) standardGeneric("ca"),
  valueClass = "CA"
)

# Predict ======================================================================
#' Predict New Coordinates
#'
#' Operators acting on objects to extract or replace parts.
#' @param object A \linkS4class{CA} object..
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
