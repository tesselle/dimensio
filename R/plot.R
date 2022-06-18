# PLOT
#' @include AllClasses.R
NULL

# Coordinates ==================================================================
## CA --------------------------------------------------------------------------
#' @export
#' @method plot CA
plot.CA <- function(x, margin = c(1, 2), axes = c(1, 2),
                    active = TRUE, sup = TRUE,
                    highlight = NULL, group = NULL, ...) {
  ## ggplot2
  gg <- plot_points(
    x,
    margin = margin,
    axes = axes,
    active = active,
    sup = sup,
    highlight = highlight,
    group = group
  )
  return(gg)
}

#' @export
#' @rdname plot_coordinates
#' @aliases plot,CA,missing-method
setMethod("plot", c(x = "CA", y = "missing"), plot.CA)

## PCA -------------------------------------------------------------------------
#' @export
#' @method plot PCA
plot.PCA <- function(x, margin = 1, axes = c(1, 2),
                     active = TRUE, sup = TRUE,
                     highlight = NULL, group = NULL, ...) {
  gg <- switch (
    margin[[1L]],
    ## Plot individuals factor map
    `1` = plot_rows(x, axes = axes, active = active, sup = sup,
                    highlight = highlight, group = group),
    ## Plot variables factor map
    `2` = plot_columns(x, axes = axes, active = active, sup = sup,
                       highlight = highlight, group = group)
  )
  return(gg)
}

#' @export
#' @rdname plot_coordinates
#' @aliases plot,PCA,missing-method
setMethod("plot", c(x = "PCA", y = "missing"), plot.PCA)

## Rows ------------------------------------------------------------------------
#' @export
#' @rdname plot_coordinates
#' @aliases plot_rows,MultivariateAnalysis-method
setMethod(
  f = "plot_rows",
  signature = signature(object = "MultivariateAnalysis"),
  definition = function(object, axes = c(1, 2), active = TRUE, sup = TRUE,
                        highlight = NULL, group = NULL) {
    ## ggplot2
    plot_points(
      object,
      margin = 1,
      axes = axes,
      active = active,
      sup = sup,
      highlight = highlight,
      group = group
    )
  }
)

## Columns ---------------------------------------------------------------------
#' @export
#' @rdname plot_coordinates
#' @aliases plot_columns,CA-method
setMethod(
  f = "plot_columns",
  signature = signature(object = "CA"),
  definition = function(object, axes = c(1, 2), active = TRUE, sup = TRUE,
                        highlight = NULL, group = NULL) {
    ## ggplot2
    plot_points(
      object,
      margin = 2,
      axes = axes,
      active = active,
      sup = sup,
      highlight = highlight,
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
                        highlight = NULL, group = NULL) {
    ## ggplot2
    plot_points(
      object,
      margin = 2,
      axes = axes,
      active = active,
      sup = sup,
      highlight = highlight,
      group = group
    )
  }
)

#' @export
#' @rdname plot_coordinates
#' @aliases plot_columns,PCA-method
setMethod(
  f = "plot_columns",
  signature = signature(object = "PCA"),
  definition = function(object, axes = c(1, 2), active = TRUE, sup = TRUE,
                        highlight = NULL, group = NULL) {
    ## Prepare data
    data <- prepare_coord(object, margin = 2, axes = axes, active = active,
                          sup = sup, highlight = highlight, group = group)
    data$z <- 0 # Set the origin of arrows

    ## Highlight or groups, if any
    aes_group <- ggplot2::aes(color = .data$type)
    if (!is.null(group)) {
      if (is.numeric(group)) {
        aes_group <- ggplot2::aes(color = .data$group, size = .data$group)
      } else {
        aes_group <- ggplot2::aes(color = .data$group)
      }
    }
    if (!is.null(highlight)) {
      aes_group <- ggplot2::aes(color = .data[[highlight]])
    }

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

    ## ggplot2
    ggplot2::ggplot(data = data) +
      ggplot2::aes(
        x = .data$x,
        y = .data$y,
        xend = .data$z,
        yend = .data$z,
        label = .data$label,
        linetype = .data$type
      ) +
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
                        fill = "grey30", border = "grey10", color = "red") {
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
        ggplot2::geom_line(mapping = aes_var, colour = color),
        ggplot2::geom_point(mapping = aes_var, colour = color)
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
plot_points <- function(object, margin, axes, active = TRUE, sup = TRUE,
                        highlight = NULL, group = NULL) {
  ## Prepare data
  data <- prepare_coord(object, margin = margin, axes = axes, active = active,
                        sup = sup, highlight = highlight, group = group)

  ## Highlight or groups, if any
  aes_group <- ggplot2::aes(color = .data$group)
  if (!is.null(group)) {
    if (is.numeric(group)) {
      aes_group <- ggplot2::aes(color = .data$group, size = .data$group)
    } else {
      aes_group <- ggplot2::aes(color = .data$group)
    }
  }
  if (!is.null(highlight)) {
    aes_group <- ggplot2::aes(color = .data[[highlight]],
                              size = .data[[highlight]])
  }

  ## ggplot2
  ggplot2::ggplot(data = data) +
    ggplot2::aes(
      x = .data$x,
      y = .data$y,
      label = .data$label,
      group = .data$group,
      shape = .data$type
    ) +
    aes_group +
    ggplot2::geom_vline(xintercept = 0, size = 0.5, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 0, size = 0.5, linetype = "dashed") +
    ggplot2::geom_point() +
    ggplot2::scale_x_continuous(name = print_variance(object, axes[[1]])) +
    ggplot2::scale_y_continuous(name = print_variance(object, axes[[2]])) +
    ggplot2::coord_fixed()
}

print_variance <- function(object, axis) {
  v <- get_variance(object, digits = 1) # Get percentage of variance
  sprintf("%s (%g%%)", names(v)[[axis]], v[[axis]])
}

# Must returns a data.frame
prepare_coord <- function(object, margin, axes, active = TRUE, sup = TRUE,
                          highlight = NULL, group = NULL) {
  ## Validation
  choices <- c("row", "column")

  ## Get data
  row_data <- col_data <- data.frame()
  if (any(margin == 1)) {
    row_data <- get_coordinates(object, margin = 1)
  }
  if (any(margin == 2)) {
    col_data <- get_coordinates(object, margin = 2)
  }

  ## Prepare data
  data <- rbind(row_data, col_data)
  type <- rep(choices, times = c(nrow(row_data), nrow(col_data)))
  obs <- ifelse(data$.sup, "suppl.", "active")

  data$x <- data[[axes[[1]]]]
  data$y <- data[[axes[[2]]]]
  data$type <- obs
  data$label <- rownames(data)

  ## Variables factor map?
  is_var <- all(margin == 2)

  ## Group
  grp <- unlist(get_groups(object, margin = margin))
  if (length(grp) > 0) {
    data$group <- factor(grp, levels = unique(grp))
  } else if (!is.null(group)) {
    group_k <- unlist(get_order(object, margin = margin))
    data$group <- group[group_k]
  } else {
    data$group <- type
  }

  ## Highlight
  if (!is.null(highlight)) {
    high_k <- joint(object, what = highlight, margin = margin, axes = axes)
    length(high_k) <- nrow(data)
    data[[highlight]] <- high_k
  }

  ## Subset
  if (active & !sup) data <- data[!data$.sup, ]
  if (!active & sup) data <- data[data$.sup, ]

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
