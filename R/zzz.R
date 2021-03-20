.onLoad <- function(libname, pkgname) {
  op <- options()
  op.dimensio <- list(
    dimensio.verbose = TRUE,
    dimensio.digits = 3,
    dimensio.max.print = 10
  )
  toset <- !(names(op.dimensio) %in% names(op))
  if(any(toset)) options(op.dimensio[toset])

  invisible()
}
