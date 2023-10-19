#' Deprecated Functions in dimensio
#'
#' These functions still work but will be removed (defunct) in the next version.
#' @name dimensio-deprecated
#' @keywords internal
NULL

check_package <- function(x) {
  if (!requireNamespace(x, quietly = TRUE)) {
    msg <- "Package %s needed for this function to work. Please install it."
    stop(sprintf(msg, x), call. = FALSE)
  }
  invisible(NULL)
}

# rlang ========================================================================
utils::globalVariables(".data")

# ggplot2 ======================================================================
# https://cran.r-project.org/web/packages/ggplot2/vignettes/extending-ggplot2.html

#' @rdname dimensio-deprecated
#' @export
stat_hull <- function(mapping = NULL, data = NULL, geom = "polygon",
                      position = "identity", na.rm = FALSE, show.legend = NA,
                      inherit.aes = TRUE, ...) {
  check_package("ggplot2")

  StatHull <- ggplot2::ggproto(
    `_class` = "StatHull",
    `_inherit` = ggplot2::Stat,
    compute_group = function(data, scales) {
      i <- chull(data$x, data$y)
      data[c(i, i[1]), , drop = FALSE]
    },
    required_aes = c("x", "y")
  )

  ggplot2::layer(
    stat = StatHull, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

# Plot =========================================================================
## Rows ------------------------------------------------------------------------
#' @rdname dimensio-deprecated
setGeneric(
  name = "plot_rows",
  def = function(object, ...) standardGeneric("plot_rows")
)

#' @export
#' @rdname dimensio-deprecated
#' @aliases plot_rows,MultivariateAnalysis-method
setMethod(
  f = "plot_rows",
  signature = c(object = "MultivariateAnalysis"),
  definition = function(object, axes = c(1, 2), active = TRUE, sup = TRUE,
                        alpha = NULL, colour = NULL, fill = NULL, shape = NULL,
                        size = NULL, group = NULL) {
    .Deprecated(new = "viz_rows()", old = "plot_rows()")
    plot_points(object, margin = 1, axes = axes, active = active, sup = sup,
                alpha = alpha, colour = colour, fill = fill, shape = shape,
                size = size, group = group)
  }
)

## Columns ---------------------------------------------------------------------
#' @rdname dimensio-deprecated
setGeneric(
  name = "plot_columns",
  def = function(object, ...) standardGeneric("plot_columns")
)

#' @export
#' @rdname dimensio-deprecated
#' @aliases plot_columns,MultivariateAnalysis-method
setMethod(
  f = "plot_columns",
  signature = c(object = "MultivariateAnalysis"),
  definition = function(object, axes = c(1, 2), active = TRUE, sup = TRUE,
                        alpha = NULL, colour = NULL, fill = NULL, shape = NULL,
                        size = NULL, group = NULL) {
    .Deprecated(new = "viz_columns()", old = "plot_columns()")
    plot_points(object, margin = 2, axes = axes, active = active, sup = sup,
                alpha = alpha, colour = colour, fill = fill, shape = shape,
                size = size, group = group)
  }
)

#' @export
#' @rdname dimensio-deprecated
#' @aliases plot_columns,BootstrapPCA-method
setMethod(
  f = "plot_columns",
  signature = c(object = "BootstrapPCA"),
  definition = function(object, axes = c(1, 2), active = TRUE, sup = TRUE,
                        alpha = NULL, colour = NULL, fill = NULL, shape = NULL,
                        size = NULL, group = NULL) {
    .Deprecated(new = "viz_columns()", old = "plot_columns()")
    plot_points(object, margin = 2, axes = axes, active = active, sup = sup,
                alpha = alpha, colour = colour, fill = fill, shape = shape,
                size = size, group = group)
  }
)

## Individuals -----------------------------------------------------------------
#' @rdname dimensio-deprecated
setGeneric(
  name = "plot_individuals",
  def = function(object, ...) standardGeneric("plot_individuals")
)

#' @export
#' @rdname dimensio-deprecated
#' @aliases plot_individuals,PCA-method
setMethod(
  f = "plot_individuals",
  signature = c(object = "PCA"),
  definition = function(object, axes = c(1, 2), active = TRUE, sup = TRUE,
                        alpha = NULL, colour = NULL, fill = NULL, shape = NULL,
                        size = NULL, group = NULL) {
    .Deprecated(new = "viz_individuals()", old = "plot_individuals()")
    plot_rows(object, axes = axes, active = active, sup = sup,
              alpha = alpha, colour = colour, fill = fill, shape = shape,
              size = size, group = group)
  }
)

## Arrows ----------------------------------------------------------------------
#' @rdname dimensio-deprecated
setGeneric(
  name = "plot_variables",
  def = function(object, ...) standardGeneric("plot_variables")
)

#' @export
#' @rdname dimensio-deprecated
#' @aliases plot_variables,PCA-method
setMethod(
  f = "plot_variables",
  signature = c(object = "PCA"),
  definition = function(object, axes = c(1, 2), active = TRUE, sup = TRUE,
                        alpha = NULL, colour = NULL, linetype = NULL,
                        size = NULL, group = NULL) {
    .Deprecated(new = "viz_variables()", old = "plot_variables()")
    check_package("ggplot2")

    ## Prepare data
    data <- prepare_coord(object, margin = 2, axes = axes, active = active,
                          sup = sup, group = group)
    data$z <- 0 # Set the origin of arrows

    ## Scaled variables?
    gg_circle <- NULL
    if (is_scaled(object)) {
      circle <- data.frame(
        x = 1 * cos(seq(0, 2 * pi, length = 200)),
        y = 1 * sin(seq(0, 2 * pi, length = 200))
      )
      gg_circle <- ggplot2::geom_path(
        mapping = ggplot2::aes(x = .data$x, y = .data$y),
        data = circle,
        colour = "grey30",
        linewidth = 0.5,
        inherit.aes = FALSE
      )
    }

    ## Aesthetics
    aes_segments <- ggplot2::aes(
      x = .data$x,
      y = .data$y,
      xend = .data$z,
      yend = .data$z,
      label = .data$label
    )
    aes_alpha <- aes_colour <- aes_linetype <- aes_size <- NULL
    choices <- c("observation", "coordinates", "contribution",
                 "cos2", "group", "data")
    if (!is.null(alpha)) {
      alpha <- match.arg(alpha, choices = choices[c(2, 3, 4)])
      aes_alpha <- ggplot2::aes(alpha = .data[[alpha]])
    }
    if (!is.null(colour)) {
      colour <- match.arg(colour, choices = choices)
      aes_colour <- ggplot2::aes(colour = .data[[colour]])
    }
    if (!is.null(linetype)) {
      linetype <- match.arg(linetype, choices = choices[c(1, 5)])
      aes_linetype <- ggplot2::aes(linetype = .data[[linetype]])
    }
    if (!is.null(size)) {
      size <- match.arg(size, choices = choices[c(2, 3, 4)])
      aes_size <- ggplot2::aes(size = .data[[size]])
    }

    aes_group <- ggplot2::aes(group = .data$group)

    ## ggplot2
    ggplot2::ggplot(data = data) +
      aes_segments +
      aes_alpha +
      aes_colour +
      aes_linetype +
      aes_size +
      aes_group +
      ggplot2::geom_vline(xintercept = 0, linewidth = 0.5, linetype = "dashed") +
      ggplot2::geom_hline(yintercept = 0, linewidth = 0.5, linetype = "dashed") +
      gg_circle +
      ggplot2::geom_segment(
        arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm"), ends = "first"),
        linewidth = 0.5
      ) +
      ggplot2::scale_x_continuous(name = print_variance(object, axes[[1]])) +
      ggplot2::scale_y_continuous(name = print_variance(object, axes[[2]])) +
      ggplot2::coord_fixed()
  }
)

# Eigenvalues ==================================================================
#' @rdname dimensio-deprecated
setGeneric(
  name = "plot_variance",
  def = function(object, ...) standardGeneric("plot_variance")
)

#' @export
#' @rdname dimensio-deprecated
#' @aliases plot_variance,MultivariateAnalysis-method
setMethod(
  f = "plot_variance",
  signature = c(object = "MultivariateAnalysis"),
  definition = function(object, select = NULL, variance = TRUE, cumulative = TRUE,
                        fill = "grey30", border = "grey10", colour = "red") {
    .Deprecated(new = "screeplot()", old = "plot_variance()")
    check_package("ggplot2")

    ## Prepare data
    data <- get_eigenvalues(object)
    if (!is.null(select)) {
      data <- data[select, , drop = FALSE]
    }

    data$x <- seq_len(nrow(data))
    data$z <- data[[3L]]

    ## Eigenvalues
    gg_var <- NULL
    gg_scale <- ggplot2::waiver()
    if (variance) {
      data$y <- data[[2L]]
      data$label <- paste0(round(data$y, digits = 1), "%")
      y_name <- if (methods::is(object, "CA")) "Inertia" else "Explained variance (%)"
    } else {
      data$y <- data[[1L]]
      data$label <- round(data$y, digits = 1)
      y_name <- "Eigenvalues"
      gg_scale <- ggplot2::waiver()
    }
    if (cumulative) {
      k <- max(data$y) / max(data$z)
      aes_var <- ggplot2::aes(y = .data$z * k)
      gg_var <- list(
        ggplot2::geom_line(mapping = aes_var, colour = colour),
        ggplot2::geom_point(mapping = aes_var, colour = colour)
      )
      gg_scale <- ggplot2::sec_axis(
        trans = ~ . / k,
        name = if (methods::is(object, "CA")) "Cumulative percentage of inertia" else "Cumulative percentage of variance"
      )
    }

    ## ggplot2
    ggplot2::ggplot(data = data) +
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$label) +
      ggplot2::geom_col(fill = fill, colour = border) +
      gg_var +
      ggplot2::scale_x_continuous(name = "Dimensions") +
      ggplot2::scale_y_continuous(name = y_name, sec.axis = gg_scale)
  }
)

# Contributions ================================================================
#' @rdname dimensio-deprecated
setGeneric(
  name = "plot_contributions",
  def = function(object, ...) standardGeneric("plot_contributions")
)

#' @export
#' @rdname dimensio-deprecated
#' @aliases plot_contributions,MultivariateAnalysis-method
setMethod(
  f = "plot_contributions",
  signature = c(object = "MultivariateAnalysis"),
  definition = function(object, margin = 2, axes = 1,
                        sort = TRUE, decreasing = TRUE, limit = 10,
                        fill = "grey30", border = "grey10") {
    .Deprecated(new = "viz_contributions()", old = "plot_contributions()")
    check_package("ggplot2")

    ## Prepare data
    data <- prepare_contrib(
      object,
      margin = margin,
      axes = axes,
      sort = sort,
      decreasing = decreasing,
      limit = limit
    )

    y_name <- sprintf("Contributions to %s (%%)",
                      paste0("F", axes, collapse = "-"))

    ## ggplot2
    ggplot2::ggplot(data = data) +
      ggplot2::aes(
        x = .data$x,
        y = .data$y,
        label = paste0(.data$label, "%")
      ) +
      ggplot2::geom_col(fill = fill, colour = border) +
      ggplot2::scale_x_discrete(name = "") +
      ggplot2::scale_y_continuous(name = y_name)
  }
)

# Cos2 =========================================================================
#' @rdname dimensio-deprecated
setGeneric(
  name = "plot_cos2",
  def = function(object, ...) standardGeneric("plot_cos2")
)

#' @export
#' @rdname dimensio-deprecated
#' @aliases plot_cos2,MultivariateAnalysis-method
setMethod(
  f = "plot_cos2",
  signature = c(object = "MultivariateAnalysis"),
  definition = function(object, margin = 2, axes = c(1, 2), active = TRUE,
                        sup = TRUE, sort = TRUE, decreasing = TRUE,
                        limit = 10, fill = "grey30", border = "grey10") {
    .Deprecated(new = "viz_cos2()", old = "plot_cos2()")
    check_package("ggplot2")

    ## Prepare data
    data <- prepare_cos2(object, margin = margin, axes = axes,
                         active = active, sup = sup, sort = sort,
                         decreasing = decreasing, limit = limit)

    ## ggplot2
    xx <- sprintf("along %s", paste0("F", axes, collapse = "-"))
    y_name <- bquote(paste(plain(cos)^2~.(xx)))

    ## ggplot2
    ggplot2::ggplot(data = data) +
      ggplot2::aes(
        x = .data$x,
        y = .data$y,
        label = .data$label
      ) +
      ggplot2::geom_col(fill = fill, colour = border) +
      ggplot2::scale_x_discrete(name = "") +
      ggplot2::scale_y_continuous(name = y_name)
  }
)

plot_points <- function(object, margin, axes, active = TRUE, sup = TRUE,
                        alpha = NULL, colour = NULL, fill = NULL, shape = NULL,
                        size = NULL, group = NULL) {
  check_package("ggplot2")

  ## Prepare data
  data <- lapply(
    X = margin,
    FUN = function(x, object, axes, active, sup, group) {
      prepare_coord(object, margin = x, axes = axes, active = active,
                    sup = sup, group = group)
    },
    object = object, axes = axes, active = active, sup = sup, group = group
  )
  data <- do.call(rbind, data)

  ## Aesthetics
  aes_points <- ggplot2::aes(
    x = .data$x,
    y = .data$y,
    label = .data$label
  )
  aes_alpha <- aes_colour <- aes_fill <- aes_shape <- aes_size <- NULL
  choices <- c("observation", "mass", "sum", "contribution",
               "cos2", "group", "data")
  if (!is.null(alpha)) {
    alpha <- match.arg(alpha, choices = choices[c(3, 4, 5)])
    aes_alpha <- ggplot2::aes(alpha = .data[[alpha]])
  }
  if (!is.null(colour)) {
    colour <- match.arg(colour, choices = choices)
    aes_colour <- ggplot2::aes(colour = .data[[colour]])
  }
  if (!is.null(fill)) {
    fill <- match.arg(fill, choices = choices)
    aes_fill <- ggplot2::aes(fill = .data[[fill]])
  }
  if (!is.null(shape)) {
    shape <- match.arg(shape, choices = choices[c(1, 6)])
    aes_shape <- ggplot2::aes(shape = .data[[shape]])
  }
  if (!is.null(size)) {
    size <- match.arg(size, choices = choices[c(2, 3, 4, 5, 6)])
    aes_size <- ggplot2::aes(size = .data[[size]])
  }

  aes_group <- ggplot2::aes(group = .data$group)

  ## ggplot2
  ggplot2::ggplot(data = data) +
    aes_points +
    aes_alpha +
    aes_colour +
    aes_fill +
    aes_shape +
    aes_size +
    aes_group +
    ggplot2::geom_vline(xintercept = 0, linewidth = 0.5, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.5, linetype = "dashed") +
    ggplot2::geom_point() +
    ggplot2::scale_x_continuous(name = print_variance(object, axes[[1]])) +
    ggplot2::scale_y_continuous(name = print_variance(object, axes[[2]])) +
    ggplot2::coord_fixed()
}
