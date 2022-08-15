# PLOT
#' @include AllClasses.R
NULL

# Coordinates ==================================================================
plot_points <- function(object, margin, axes, active = TRUE, sup = TRUE,
                        alpha = NULL, colour = NULL, fill = NULL, shape = NULL,
                        size = NULL, group = NULL) {
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
    ggplot2::geom_vline(xintercept = 0, size = 0.5, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 0, size = 0.5, linetype = "dashed") +
    ggplot2::geom_point() +
    ggplot2::scale_x_continuous(name = print_variance(object, axes[[1]])) +
    ggplot2::scale_y_continuous(name = print_variance(object, axes[[2]])) +
    ggplot2::coord_fixed()
}

## Biplots ---------------------------------------------------------------------
#' @export
#' @rdname biplot
#' @aliases biplot,CA-method
setMethod(
  f = "biplot",
  signature = signature(x = "CA"),
  definition = function(x, axes = c(1, 2),
                        type = c("rows", "columns", "contributions"),
                        active = TRUE, sup = TRUE,
                        label = c("rows", "columns")) {
    ## Validation
    type <- match.arg(type, several.ok = FALSE)
    label <- match.arg(label, several.ok = TRUE)

    ## Type of biplot
    if (type == "rows") {
      princ_row <- TRUE
      princ_col <- FALSE
    }
    if (type == "columns") {
      princ_row <- FALSE
      princ_col <- TRUE
    }
    if (type == "contributions") {
      princ_row <- FALSE
      princ_col <- TRUE
    }

    ## Get data
    coord_row <-  prepare_coord(x, margin = 1, axes = axes,
                                active = active, sup = sup,
                                principal = princ_row)
    coord_col <-  prepare_coord(x, margin = 2, axes = axes,
                                active = active, sup = sup,
                                principal = princ_col)

    if (type == "contributions") {
      coord_row$x <- coord_row$x * sqrt(coord_row$mass)
      coord_row$y <- coord_row$y * sqrt(coord_row$mass)
    }

    coord <- rbind(coord_row, coord_col)

    ## Labels
    if ("rows" %notin% label) {
      coord$label[coord$data == "row"] <- NA
    }
    if ("columns" %notin% label) {
      coord$label[coord$data == "column"] <- NA
    }

    ## Aesthetics
    aes_points <- ggplot2::aes(
      x = .data$x,
      y = .data$y,
      colour = .data$data,
      label = .data$label
    )
    if (length(unique(coord$observation)) > 1) {
      aes_shape <- ggplot2::aes(shape = .data$observation)
    } else {
      aes_shape <- ggplot2::aes(shape = .data$data)
    }
    if (type == "contributions") {
      aes_size <- ggplot2::aes(size = .data$mass)
    } else {
      aes_size <- NULL
    }

    ## ggplot2
    ggplot2::ggplot(data = coord) +
      aes_points +
      aes_shape +
      aes_size +
      ggplot2::geom_vline(xintercept = 0, size = 0.5, linetype = "dashed") +
      ggplot2::geom_hline(yintercept = 0, size = 0.5, linetype = "dashed") +
      ggplot2::geom_point() +
      ggplot2::scale_x_continuous(name = print_variance(x, axes[[1]])) +
      ggplot2::scale_y_continuous(name = print_variance(x, axes[[2]])) +
      ggplot2::coord_fixed()
  }
)

#' @export
#' @rdname biplot
#' @aliases biplot,PCA-method
setMethod(
  f = "biplot",
  signature = signature(x = "PCA"),
  definition = function(x, axes = c(1, 2), type = c("form", "covariance"),
                        active = TRUE, sup = TRUE,
                        label = c("individuals", "variables")) {
    ## Validation
    type <- match.arg(type, several.ok = FALSE)
    label <- match.arg(label, several.ok = TRUE)

    ## Type of biplot
    if (type == "form") {
      princ_row <- TRUE
      princ_col <- FALSE
    }
    if (type == "covariance") {
      princ_row <- FALSE
      princ_col <- TRUE
    }

    ## Get data
    coord_row <-  prepare_coord(x, margin = 1, axes = axes,
                                active = active, sup = sup,
                                principal = princ_row)
    coord_col <-  prepare_coord(x, margin = 2, axes = axes,
                                active = active, sup = sup,
                                principal = princ_col)

    coord <- rbind(coord_row, coord_col)
    coord_col$z <- 0 # Set the origin of arrows

    ## Labels
    if ("individuals" %notin% label) {
      coord$label[coord$data == "row"] <- NA
    }
    if ("variables" %notin% label) {
      coord$label[coord$data == "column"] <- NA
    }

    ## Aesthetics
    aes_points <- ggplot2::aes(
      x = .data$x,
      y = .data$y,
      colour = .data$data,
      label = .data$label
    )
    aes_segments <- ggplot2::aes(
      xend = .data$z,
      yend = .data$z
    )

    ## ggplot2
    ggplot2::ggplot(data = coord) +
      aes_points +
      ggplot2::geom_vline(xintercept = 0, size = 0.5, linetype = "dashed") +
      ggplot2::geom_hline(yintercept = 0, size = 0.5, linetype = "dashed") +
      ggplot2::geom_point(
        data = coord_row
      ) +
      ggplot2::geom_segment(
        data = coord_col,
        mapping = aes_segments,
        arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm"), ends = "first"),
        size = 0.5
      ) +
      ggplot2::scale_x_continuous(name = print_variance(x, axes[[1]])) +
      ggplot2::scale_y_continuous(name = print_variance(x, axes[[2]])) +
      ggplot2::coord_fixed()
  }
)

## Rows ------------------------------------------------------------------------
#' @export
#' @rdname plot_coordinates
#' @aliases plot_rows,MultivariateAnalysis-method
setMethod(
  f = "plot_rows",
  signature = signature(object = "MultivariateAnalysis"),
  definition = function(object, axes = c(1, 2), active = TRUE, sup = TRUE,
                        alpha = NULL, colour = NULL, fill = NULL, shape = NULL,
                        size = NULL, group = NULL) {
    ## ggplot2
    plot_points(
      object,
      margin = 1,
      axes = axes,
      active = active,
      sup = sup,
      alpha = alpha,
      colour = colour,
      fill = fill,
      shape = shape,
      size = size,
      group = group
    )
  }
)

## Columns ---------------------------------------------------------------------
#' @export
#' @rdname plot_coordinates
#' @aliases plot_columns,MultivariateAnalysis-method
setMethod(
  f = "plot_columns",
  signature = signature(object = "MultivariateAnalysis"),
  definition = function(object, axes = c(1, 2), active = TRUE, sup = TRUE,
                        alpha = NULL, colour = NULL, fill = NULL, shape = NULL,
                        size = NULL, group = NULL) {
    ## ggplot2
    plot_points(
      object,
      margin = 2,
      axes = axes,
      active = active,
      sup = sup,
      alpha = alpha,
      colour = colour,
      fill = fill,
      shape = shape,
      size = size,
      group = group
    )
  }
)

#' @export
#' @rdname plot_coordinates
#' @aliases plot_columns,BootstrapPCA-method
setMethod(
  f = "plot_columns",
  signature = signature(object = "BootstrapPCA"),
  definition = function(object, axes = c(1, 2), active = TRUE, sup = TRUE,
                        alpha = NULL, colour = NULL, fill = NULL, shape = NULL,
                        size = NULL, group = NULL) {
    ## ggplot2
    plot_points(
      object,
      margin = 2,
      axes = axes,
      active = active,
      sup = sup,
      alpha = alpha,
      colour = colour,
      fill = fill,
      shape = shape,
      size = size,
      group = group
    )
  }
)

## Individuals -----------------------------------------------------------------
#' @export
#' @rdname plot_coordinates
#' @aliases plot_individuals,PCA-method
setMethod(
  f = "plot_individuals",
  signature = signature(object = "PCA"),
  definition = function(object, axes = c(1, 2), active = TRUE, sup = TRUE,
                        alpha = NULL, colour = NULL, fill = NULL, shape = NULL,
                        size = NULL, group = NULL) {
    plot_rows(object, axes = axes, active = active, sup = sup,
              alpha = alpha, colour = colour, fill = fill, shape = shape,
              size = size, group = group)
  }
)

## Arrows ----------------------------------------------------------------------
#' @export
#' @rdname plot_coordinates
#' @aliases plot_variables,PCA-method
setMethod(
  f = "plot_variables",
  signature = signature(object = "PCA"),
  definition = function(object, axes = c(1, 2), active = TRUE, sup = TRUE,
                        alpha = NULL, colour = NULL, linetype = NULL,
                        size = NULL, group = NULL) {
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
        size = 0.5,
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
      ggplot2::geom_vline(xintercept = 0, size = 0.5, linetype = "dashed") +
      ggplot2::geom_hline(yintercept = 0, size = 0.5, linetype = "dashed") +
      gg_circle +
      ggplot2::geom_segment(
        arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm"), ends = "first"),
        size = 0.5
      ) +
      ggplot2::scale_x_continuous(name = print_variance(object, axes[[1]])) +
      ggplot2::scale_y_continuous(name = print_variance(object, axes[[2]])) +
      ggplot2::coord_fixed()
  }
)

# Contributions ================================================================
#' @export
#' @rdname plot_contributions
#' @aliases plot_contributions,MultivariateAnalysis-method
setMethod(
  f = "plot_contributions",
  signature = signature(object = "MultivariateAnalysis"),
  definition = function(object, margin = 2, axes = 1,
                        sort = TRUE, decreasing = TRUE, limit = 10,
                        fill = "grey30", border = "grey10") {
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
#' @export
#' @rdname plot_contributions
#' @aliases plot_cos2,MultivariateAnalysis-method
setMethod(
  f = "plot_cos2",
  signature = signature(object = "MultivariateAnalysis"),
  definition = function(object, margin = 2, axes = c(1, 2), active = TRUE,
                        sup = TRUE, sort = TRUE, decreasing = TRUE,
                        limit = 10, fill = "grey30", border = "grey10") {
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

# Eigenvalues ==================================================================
#' @export
#' @rdname plot_eigenvalues
#' @aliases plot_variance,MultivariateAnalysis-method
setMethod(
  f = "plot_variance",
  signature = signature(object = "MultivariateAnalysis"),
  definition = function(object, variance = TRUE, cumulative = TRUE,
                        fill = "grey30", border = "grey10", colour = "red") {
    ## Prepare data
    data <- get_eigenvalues(object)
    data$x <- seq_len(nrow(data))
    data$z <- data[[3L]]

    ## Eigenvalues
    gg_var <- NULL
    gg_scale <- ggplot2::waiver()
    if (variance) {
      data$y <- data[[2L]]
      data$label <- paste0(round(data$y, digits = 1), "%")
      y_name <- "Explained variance (%)"
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
        name = "Cumulative percentage of variance"
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

# Helpers ======================================================================
print_variance <- function(object, axis) {
  v <- get_variance(object, digits = 1) # Get percentage of variance
  sprintf("%s (%g%%)", names(v)[[axis]], v[[axis]])
}

# Must return a data.frame
prepare_coord <- function(object, margin, axes = c(1, 2), active = TRUE,
                          sup = TRUE, principal = TRUE, group = NULL) {
  ## Prepare data
  data <- augment(object, margin = margin, axes = axes, principal = principal)
  data$x <- data[[1]]
  data$y <- data[[2]]

  k <- get_order(object, margin = margin)
  if (!is.null(group)) {
    assert_length(group, nrow(data))
    group <- group[k]
  } else if (has_groups(object, margin = margin)) {
    group <- get_groups(object, margin = margin)
  } else {
    group <- rep(NA_character_, length(k))
  }
  data$group <- group

  type <- ifelse(margin == 1, "row", "column")
  data$data <- rep(type, length(k))

  ## Subset
  if (active & !sup) data <- data[!data$supplementary, ]
  if (!active & sup) data <- data[data$supplementary, ]
  data$observation <- ifelse(data$supplementary, "suppl.", "active")

  data
}
# Must return a data.frame
prepare_contrib <- function(object, margin, axes, sort = TRUE,
                            decreasing = TRUE, limit = 10) {
  ## Get data
  contrib <- get_contributions(object, margin = margin)
  if (length(axes) > 1) {
    values <- joint_contributions(object, margin = margin, axes = axes)
  } else {
    values <- contrib[[axes[[1]]]]
  }

  ## Prepare data
  data <- data.frame(
    x = rownames(contrib),
    y = values,
    label = round(values, digits = 2)
  )

  ## Sort data
  if (sort) {
    data <- data[order(data$y, decreasing = decreasing), ]
  }

  ## Subset
  if (!is.null(limit)) {
    limit <- min(nrow(data), limit)
    data <- data[seq_len(limit), , drop = FALSE]
  }

  ## Prevent reordering by ggplot2
  data$x <- factor(data$x, levels = unique(data$x))

  data
}

# Must return a data.frame
prepare_cos2 <- function(object, margin, axes, active = TRUE, sup = TRUE,
                         sort = TRUE, decreasing = TRUE, limit = 10) {
  ## Get data
  cos2 <- get_cos2(object, margin = margin)
  if (length(axes) > 1) {
    values <- joint_cos2(object, margin = margin, axes = axes)
  } else {
    values <- cos2[[axes[[1]]]]
  }

  ## Prepare data
  data <- data.frame(
    x = rownames(cos2),
    y = values,
    label = round(values, digits = 2)
  )

  ## Subset
  if (!active & sup) data <- data[cos2$.sup, ]
  if (active & !sup) data <- data[!cos2$.sup, ]

  ## Sort data
  if (sort) {
    data <- data[order(data$y, decreasing = decreasing), ]
  }

  ## Subset
  if (!is.null(limit)) {
    limit <- min(nrow(data), limit)
    data <- data[seq_len(limit), , drop = FALSE]
  }

  ## Prevent reordering by ggplot2
  data$x <- factor(data$x, levels = unique(data$x))

  data
}
