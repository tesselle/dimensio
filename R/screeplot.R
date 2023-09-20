# SCREEPLOT
#' @include AllGenerics.R
NULL

# Screeplot ====================================================================
#' @export
#' @rdname screeplot
#' @aliases screeplot,MultivariateAnalysis-method
setMethod(
  f = "screeplot",
  signature = c(x = "MultivariateAnalysis"),
  definition = function(x, ..., eigenvalues = FALSE, cumulative = FALSE,
                        labels = TRUE, limit = 10,
                        col = "grey90", border = "grey10",
                        col.cumulative = "red", lty.cumulative = "solid",
                        lwd.cumulative = 2) {
    ## TODO
    horiz <- FALSE

    ## Save and restore graphical parameters
    old_par <- graphics::par(mar = c(5, 4, 4, 2 + 2 * cumulative) + 0.1,
                             no.readonly = TRUE)
    on.exit(graphics::par(old_par), add = TRUE)

    ## Prepare data
    data <- get_eigenvalues(x)
    data$x <- seq_len(nrow(data))
    data$z <- data[[3L]]

    ## Subset
    if (!is.null(limit)) {
      limit <- min(nrow(data), limit)
      data <- data[seq_len(limit), , drop = FALSE]
    }

    if (eigenvalues) {
      data$y <- data[[1L]]
      data$labels <- round(data$y, digits = 1)
      ylab <- "Eigenvalues"
    } else {
      data$y <- data[[2L]]
      data$labels <- paste0(round(data$y, digits = 1), "%")
      ylab <- if (methods::is(x, "CA")) "Inertia" else "Explained variance (%)"
    }

    k <- max(data$y) / max(data$z)
    data$k <- data$z * k

    ## Bar plot
    mid <- graphics::barplot(
      height = data$y,
      names.arg = data$x,
      horiz = horiz,
      xlab = if (horiz) ylab else NULL,
      ylab = if (horiz) NULL else ylab,
      ylim = c(0, max(data$k)) * 1.05,
      col = col,
      border = border,
      las = 1,
      ...
    )

    if (labels) {
      graphics::text(
        x = mid,
        y = data$y,
        labels = data$labels,
        pos = 3
      )
    }

    if (cumulative && !horiz) {
      tick_labels <- seq(from = 0, to = 100, by = 20)
      tick_at <- tick_labels * k
      graphics::lines(
        x = mid,
        y = data$k,
        type = "b",
        pch = 16,
        lty = lty.cumulative,
        lwd = lwd.cumulative,
        col = col.cumulative
      )
      graphics::axis(side = 4, at = tick_at, labels = tick_labels,
                     col = col.cumulative, col.ticks = col.cumulative,
                     col.axis = col.cumulative, las = 1)
      graphics::mtext(
        text = sprintf("Cumulative percentage of %s", tolower(ylab)),
        side = 4, line = 3, col = col.cumulative
      )
    }

    invisible(x)
  }
)
