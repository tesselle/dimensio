# CONVEX HULL
#' @include AllGenerics.R
NULL

#' @export
#' @rdname viz_hull
#' @aliases wrap_hull,numeric,numeric-method
setMethod(
  f = "wrap_hull",
  signature = c(x = "numeric", y = "numeric"),
  definition = function(x, y, group = NULL) {
    ## Validation
    n <- length(x)
    arkhe::assert_length(y, n)

    ## Add groups, if any
    if (is.null(group)) group <- rep("", n)
    group <- as.character(group)
    arkhe::assert_length(group, n)

    ## Clean
    ok <- !is.na(x) & !is.na(y) & !is.na(group)
    x <- x[ok]
    y <- y[ok]
    group <- group[ok]

    ## Compute convex hulls
    index <- split(seq_len(n), f = group)
    lapply(
      X = index,
      FUN = function(i) {
        xi <- x[i]
        yi <- y[i]
        if (length(xi) < 3) return(NULL)

        i <- grDevices::chull(xi, yi)
        cbind(xi, yi)[c(i, i[1]), , drop = FALSE]
      }
    )
  }
)

#' @export
#' @rdname viz_hull
#' @aliases wrap_hull,MultivariateAnalysis,missing-method
setMethod(
  f = "wrap_hull",
  signature = c(x = "MultivariateAnalysis", y = "missing"),
  definition = function(x, margin = 1, axes = c(1, 2), group = NULL,
                        principal = TRUE) {
    ## Validation
    arkhe::assert_scalar(margin, "numeric")
    arkhe::assert_type(axes, "numeric")
    arkhe::assert_length(axes, 2)

    ## Get coordinates
    data <- get_coordinates(x, margin = margin, principal = principal)
    data <- data[, axes]

    ## Add groups, if any
    if (length(group) > 1) {
      group <- group[get_order(x, margin = margin)]
    } else if (length(group) == 1) {
      group <- get_extra(x)[[group]]
    } else if (has_groups(x, margin = margin)) {
      group <- get_groups(x, margin = margin)
    }

    ## Compute convex hulls
    methods::callGeneric(x = data[, 1], y = data[, 2], group = group)
  }
)

#' @export
#' @rdname viz_hull
#' @aliases wrap_hull,PCOA,missing-method
setMethod(
  f = "wrap_hull",
  signature = c(x = "PCOA", y = "missing"),
  definition = function(x, axes = c(1, 2), group = NULL) {
    ## Validation
    arkhe::assert_type(axes, "numeric")
    arkhe::assert_length(axes, 2)

    ## Get coordinates
    data <- get_coordinates(x)
    data <- data[, axes]

    ## Compute convex hulls
    methods::callGeneric(x = data[, 1], y = data[, 2], group = group)
  }
)
