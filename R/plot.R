# PLOT
#' @include AllClasses.R
NULL

# Correspondence Analysis ======================================================
## Coordinates -----------------------------------------------------------------
#' @export
#' @rdname plot_coordinates
#' @aliases plot,CA,missing-method
setMethod(
  f = "plot",
  signature = signature(x = "CA", y = "missing"),
  definition = function(x, margin = c(1, 2), axes = c(1, 2),
                        active = TRUE, sup = TRUE,
                        highlight = NULL, group = NULL) {
    ## ggplot2
    gg <- plot_points(
      x,
      margin = margin,
      axes = axes,
      active = active,
      sup = sup,
      highlight = highlight,
      group = group,
      several.ok = TRUE
    )
    return(gg)
  }
)

#' @export
#' @rdname plot_coordinates
#' @aliases plot_rows,CA-method
setMethod(
  f = "plot_rows",
  signature = signature(object = "CA"),
  definition = function(object, axes = c(1, 2), active = TRUE, sup = TRUE,
                        highlight = NULL, group = NULL) {
    ## ggplot2
    gg <- plot_points(
      object,
      margin = 1,
      axes = axes,
      active = active,
      sup = sup,
      highlight = highlight,
      group = group,
      several.ok = FALSE
    )
    return(gg)
  }
)

#' @export
#' @rdname plot_coordinates
#' @aliases plot_columns,PCA-method
setMethod(
  f = "plot_columns",
  signature = signature(object = "CA"),
  definition = function(object, axes = c(1, 2), active = TRUE, sup = TRUE,
                        highlight = NULL, group = NULL) {
    ## ggplot2
    gg <- plot_points(
      object,
      margin = 2,
      axes = axes,
      active = active,
      sup = sup,
      highlight = highlight,
      group = group,
      several.ok = FALSE
    )
    return(gg)
  }
)

# Principal Components Analysis ================================================
## Coordinates -----------------------------------------------------------------
#' @export
#' @rdname plot_coordinates
#' @aliases plot,PCA,missing-method
setMethod(
  f = "plot",
  signature = signature(x = "PCA", y = "missing"),
  definition = function(x, margin = 1, axes = c(1, 2),
                        active = TRUE, sup = TRUE,
                        highlight = NULL, group = NULL) {
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
)

#' @export
#' @rdname plot_coordinates
#' @aliases plot_rows,PCA-method
setMethod(
  f = "plot_rows",
  signature = signature(object = "PCA"),
  definition = function(object, axes = c(1, 2), active = TRUE, sup = TRUE,
                        highlight = NULL, group = NULL) {
    ## ggplot2
    gg <- plot_points(
      object,
      margin = 1,
      axes = axes,
      active = active,
      sup = sup,
      highlight = highlight,
      group = group,
      several.ok = FALSE
    )
    return(gg)
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
    ## ggplot2
    gg <- plot_arrows(
      object,
      margin = 2,
      axes = axes,
      active = active,
      sup = sup,
      highlight = highlight,
      group = group,
      several.ok = FALSE
    )
    return(gg)
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
    contrib <- get_contributions(object, margin = margin)
    data <- prepare_contrib(
      contrib,
      axes = axes,
      sort = sort,
      decreasing = decreasing,
      limit = limit
    )

    ## ggplot2
    gg <- ggplot2::ggplot(data = data) +
      ggplot2::aes(x = .data$x, y = .data$y, label = paste0(.data$label, "%")) +
      ggplot2::geom_col(fill = fill, colour = border) +
      ggplot2::scale_x_discrete(name = "") +
      ggplot2::scale_y_continuous(name = "Contributions (%)")

    return(gg)
  }
)

# Cos2 =========================================================================
#' @export
#' @rdname plot_contributions
#' @aliases plot_cos2,MultivariateAnalysis-method
setMethod(
  f = "plot_cos2",
  signature = signature(object = "MultivariateAnalysis"),
  definition = function(object, margin = 1, axes = 1,
                        sort = TRUE, decreasing = TRUE, limit = 10,
                        active = TRUE, sup = FALSE,
                        fill = "grey30", border = "grey10") {
    ## Prepare data
    cos2 <- get_cos2(object, margin = margin, sup = sup)
    if (sup) cos2 <- cos2[cos2$.sup, , drop = FALSE]
    data <- prepare_contrib(
      cos2,
      axes = axes,
      sort = sort,
      decreasing = decreasing,
      limit = limit
    )

    ## ggplot2
    gg <- ggplot2::ggplot(data = data) +
      ggplot2::aes(x = .data$x, y = .data$y) +
      ggplot2::geom_col(fill = fill, colour = border) +
      ggplot2::scale_x_discrete(name = "") +
      ggplot2::scale_y_continuous(name = expression("Cos"^2))

    return(gg)
  }
)

# Eigenvalues ==================================================================
#' @export
#' @rdname plot_eigenvalues
#' @aliases plot_eigenvalues,MultivariateAnalysis-method
setMethod(
  f = "plot_eigenvalues",
  signature = signature(object = "MultivariateAnalysis"),
  definition = function(object, fill = "grey30", border = "grey10") {
    plot_variance(object, variance = FALSE, fill = fill, border = border)
  }
)

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

    ## Eigenvalues
    gg_var <- NULL
    gg_scale <- ggplot2::waiver()
    if (variance) {
      data$y <- data[[2L]]
      data$label <- paste0(round(data$y, digits = 1), "%")
      y_name <- "Variance (%)"
      if (cumulative) {
        k <- max(data$y) / max(data$cumulative)
        aes_var <- ggplot2::aes(y = .data$cumulative * k)
        gg_var <- list(
          ggplot2::geom_line(mapping = aes_var, colour = color),
          ggplot2::geom_point(mapping = aes_var, colour = color)
        )
        gg_scale <- ggplot2::sec_axis(
          trans = ~ . / k,
          name = "Cumulative percentage of variance"
        )
      }
    } else {
      data$y <- data[[1L]]
      data$label <- round(data$y, digits = 1)
      y_name <- "Eigenvalues"
      gg_scale <- ggplot2::waiver()
    }
    data$x <- seq_len(nrow(data))

    ## ggplot2
    gg <- ggplot2::ggplot(data = data) +
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$label) +
      ggplot2::geom_col(fill = fill, colour = border) +
      gg_var +
      ggplot2::scale_x_continuous(name = "Dimensions") +
      ggplot2::scale_y_continuous(name = y_name, sec.axis = gg_scale)

    return(gg)
  }
)

# Helpers ======================================================================
plot_points <- function(object, margin, axes, active = TRUE, sup = TRUE,
                        highlight = NULL, group = NULL, several.ok = FALSE) {
  ## Prepare data
  data <- prepare_coord(object, margin = margin, axes = axes,
                        active = active, sup = sup, highlight = highlight,
                        group = group, several.ok = several.ok)

  ## Highlight or groups, if any
  aes_group <- ggplot2::aes(color = .data$value)
  if (!is.null(group)) {
    if (is.numeric(group)) {
      aes_group <- ggplot2::aes(color = .data$group, size = .data$group)
    } else {
      aes_group <- ggplot2::aes(color = .data$group, group = .data$group)
    }
  }
  if (!is.null(highlight)) {
    aes_group <- ggplot2::aes(color = .data[[highlight]])
  }

  ## ggplot2
  gg <- ggplot2::ggplot(data = data) +
    ggplot2::aes(
      x = .data$x,
      y = .data$y,
      label = .data$label,
      shape = .data$value
    ) +
    aes_group +
    ggplot2::geom_vline(xintercept = 0, size = 0.5, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 0, size = 0.5, linetype = "dashed") +
    ggplot2::geom_point() +
    ggplot2::scale_x_continuous(name = print_variance(object, axes[[1]])) +
    ggplot2::scale_y_continuous(name = print_variance(object, axes[[2]])) +
    ggplot2::coord_fixed()

  return(gg)
}

plot_arrows <- function(object, margin, axes, active = TRUE, sup = TRUE,
                        highlight = NULL, group = NULL, several.ok = FALSE) {
  ## Prepare data
  data <- prepare_coord(object, margin = margin, axes = axes,
                        active = active, sup = sup, highlight = highlight,
                        group = group, several.ok = several.ok)
  data$z <- 0 # Set the origin of arrows

  ## Highlight or groups, if any
  aes_group <- ggplot2::aes(color = .data$value, linetype = .data$value)
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
  scaled <- !all(object[["data"]][["sd"]] == 1)
  gg_circle <- NULL
  if (scaled) {
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
  gg <- ggplot2::ggplot(data = data) +
    ggplot2::aes(
      x = .data$x,
      y = .data$y,
      xend = .data$z,
      yend = .data$z,
      label = .data$label
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

  return(gg)
}

print_variance <- function(object, axis) {
  v <- get_variance(object, digits = 1) # Get percentage of variance
  sprintf("%s (%g%%)", names(v)[[axis]], v[[axis]])
}

# Must returns a data.frame
prepare_coord <- function(object, margin, axes, active = TRUE, sup = TRUE,
                          highlight = NULL, group = NULL, several.ok = FALSE) {
  ## Validation
  choices <- c("row", "column")

  ## Get data
  row_data <- col_data <- data.frame()
  if (any(margin == 1)) {
    row_coord <- get_coordinates(object, margin = 1, sup = TRUE)
    row_i <- seq_len(nrow(row_coord))
    if (active & !sup) row_i <- !row_coord$.sup
    if (!active & sup) row_i <- row_coord$.sup
    row_data <- row_coord[row_i, ]
  }
  if (any(margin == 2)) {
    col_coord <- get_coordinates(object, margin = 2, sup = TRUE)
    col_i <- seq_len(nrow(col_coord))
    if (active & !sup) col_i <- !col_coord$.sup
    if (!active & sup) col_i <- col_coord$.sup
    col_data <- col_coord[col_i, ]
  }

  ## Prepare data
  data <- rbind(row_data, col_data)
  type <- rep(choices, times = c(nrow(row_data), nrow(col_data)))
  obs <- ifelse(data$.sup, "suppl.", "active")
  active <- sprintf("%s (%s)", type, obs)

  data$x <- data[[axes[[1]]]]
  data$y <- data[[axes[[2]]]]
  data$value <- active
  data$label <- rownames(data)

  ## Variables factor map?
  is_var <- all(margin == 2)

  ## Group
  if (!is.null(group)) {
    group_k <- get_order(object, margin = ifelse(is_var, 2, 1))
    group_i <- if (is_var) col_i else row_i # Subset
    group <- group[group_k][group_i]
    data$group <- group
  }

  ## Highlight
  if (!is.null(highlight)) {
    highlight_i <- if (is_var) col_i else row_i # Subset
    highlight_k <- joint(object, what = highlight, margin = margin,
                         axes = axes, sup = TRUE)
    highlight_k <- highlight_k[highlight_i]
    data[[highlight]] <- highlight_k
  }

  return(data)
}

# Must return a data.frame
prepare_contrib <- function(object, axes, sort, decreasing = TRUE, limit = 10) {
  ## Prepare data
  values <- object[[axes[[1]]]]
  data <- data.frame(
    x = rownames(object),
    y = values,
    label = round(values, digits = 1)
  )

  ## Sort data
  if (sort) {
    data <- data[order(data$y, decreasing = decreasing), ]
  }

  ## Subset
  if (!is.null(limit)) {
    limit <- ifelse(limit > nrow(data), nrow(data), limit)
    data <- data[seq_len(limit), , drop = FALSE]
  }

  ## Prevent reordering by ggplot2
  data$x <- factor(data$x, levels = unique(data$x))

  return(data)
}
