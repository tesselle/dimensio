.onAttach <- function(libname, pkgname) {
  op <- options()
  op.arkhe <- list(
    dimensio.verbose = TRUE,
    dimensio.digits = 3,
    dimensio.max.print = 10
  )
  toset <- !(names(op.arkhe) %in% names(op))
  if(any(toset)) options(op.arkhe[toset])

  invisible()
}
