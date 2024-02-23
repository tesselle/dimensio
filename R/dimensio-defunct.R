#' Defunct Functions in dimensio
#'
#' These functions are defunct and have been replaced.
#' @param ... Not used.
#' @name dimensio-defunct
#' @rdname dimensio-defunct
#' @keywords internal
NULL

#' @rdname dimensio-defunct
#' @export
plot_rows <- function(...) {
  .Defunct("viz_rows", package = "dimensio")
}

#' @rdname dimensio-defunct
#' @export
plot_individuals <- function(...) {
  .Defunct("viz_individuals", package = "dimensio")
}

#' @rdname dimensio-defunct
#' @export
plot_columns <- function(...) {
  .Defunct("viz_columns", package = "dimensio")
}

#' @rdname dimensio-defunct
#' @export
plot_variables <- function(...) {
  .Defunct("viz_variables", package = "dimensio")
}

#' @rdname dimensio-defunct
#' @export
plot_variance <- function(...) {
  .Defunct("screeplot", package = "dimensio")
}

#' @rdname dimensio-defunct
#' @export
plot_contributions <- function(...) {
  .Defunct("viz_contributions", package = "dimensio")
}

#' @rdname dimensio-defunct
#' @export
plot_cos2 <- function(...) {
  .Defunct("viz_cos2", package = "dimensio")
}
