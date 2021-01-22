# ACCESSORS
#' @include AllClasses.R
NULL

#' Extract Parts of an Object
#'
#' @inheritParams subset
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
extract_slot <- function(x, i) {
  class_name <- class(x)
  i <- match.arg(
    arg = i,
    choices = methods::slotNames(class_name),
    several.ok = FALSE
  )
  data <- methods::slot(x, i)
  data
}

#' @export
#' @rdname subset
#' @aliases [[,CA,character,missing-method
setMethod(
  f = "[[",
  signature = c(x = "CA", i = "character", j = "missing"),
  definition = extract_slot
)
