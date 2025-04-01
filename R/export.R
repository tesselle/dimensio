# EXPORT
#' @include AllGenerics.R
NULL

# The -r9X flags specify that the zip command should recursively search
# sub-directories, use maximum compression, and remove depreciated file fields.
# The -j flag allows the file names to be stored rather than the full file path.

#' @export
#' @rdname export
#' @aliases export,MultivariateAnalysis-method
setMethod(
  f = "export",
  signature = c(object = "MultivariateAnalysis"),
  definition = function(object, file, flags = "-r9Xj", ...) {
    ## Create temporary directory
    dir_path <- tempfile(pattern = "export_")
    dir.create(path = dir_path)
    on.exit(unlink(x = dir_path))

    ## Write results
    utils::write.csv(
      x = get_data(object),
      file = make_file_name(dir_path, "data")
    )
    utils::write.csv(
      x = get_eigenvalues(object),
      file = make_file_name(dir_path, "eigenvalues")
    )
    export_results(object, path = dir_path, margin = 1)
    export_results(object, path = dir_path, margin = 2)

    ## Zip
    status <- utils::zip(zipfile = file, files = dir_path, flags = flags, ...)
    invisible(status)
  }
)

#' @export
#' @rdname export
#' @aliases export,PCOA-method
setMethod(
  f = "export",
  signature = c(object = "PCOA"),
  definition = function(object, file, flags = "-r9Xj", ...) {
    ## Create temporary directory
    dir_path <- tempfile(pattern = "export_")
    dir.create(path = dir_path)
    on.exit(unlink(x = dir_path))

    ## Write results
    utils::write.csv(
      x = get_coordinates(object),
      file = make_file_name(dir_path, "coordinates")
    )
    utils::write.csv(
      x = get_eigenvalues(object),
      file = make_file_name(dir_path, "eigenvalues")
    )

    ## Zip
    status <- utils::zip(zipfile = file, files = dir_path, flags = flags, ...)
    invisible(status)
  }
)

export_results <- function(object, path, margin, sup_name = ".sup") {
  ## Coordinates
  coords <- get_coordinates(
    x = object,
    margin = margin,
    principal = TRUE,
    sup_name = sup_name
  )

  ## Contributions
  contrib <- get_contributions(
    x = object,
    margin = margin
  )

  ## cos2
  cos2 <- get_cos2(
    x = object,
    margin = margin,
    sup_name = sup_name
  )

  ## Write
  utils::write.csv(x = coords, file = make_file_name(path, "coordinates", margin))
  utils::write.csv(x = contrib, file = make_file_name(path, "contributions", margin))
  utils::write.csv(x = cos2, file = make_file_name(path, "cos2", margin))

  invisible(NULL)
}

make_file_name <- function(path, name, margin = NULL) {
  prefix <- ""
  if (!is.null(margin) && margin == 1) prefix <- "row_"
  if (!is.null(margin) && margin == 2) prefix <- "col_"

  file_name <- paste0(prefix, name, ".csv")
  file_path <- file.path(path, file_name)

  file_path
}
