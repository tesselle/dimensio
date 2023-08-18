# TIDY DATA
#' @include AllGenerics.R
NULL

#' @export
#' @rdname tidy
#' @aliases tidy,MultivariateAnalysis-method
setMethod(
  f = "tidy",
  signature = c(x = "MultivariateAnalysis"),
  definition = function(x, margin = 1, principal = TRUE, ...) {
    ## Validation
    assert_length(margin, 1)

    ## Get data
    coords <- get_coordinates(x, margin = margin, principal = principal)
    coords_long <- cbind(rownames(coords), coords[, ncol(coords)],
                         utils::stack(coords[, -ncol(coords)]))
    colnames(coords_long) <- c("label", "supplementary", "coordinate", "component")

    contrib <- get_contributions(x, margin = margin)
    contrib_long <- cbind(rownames(contrib), utils::stack(contrib))
    colnames(contrib_long) <- c("label", "contribution", "component")

    cos2 <- get_cos2(x, margin = margin)
    cos2_long <- cbind(rownames(cos2), utils::stack(cos2[, -ncol(cos2)]))
    colnames(cos2_long) <- c("label", "cos2", "component")

    ## Join data
    Reduce(
      f = function(df1, df2) {
        merge(df1, df2, by = c("label", "component"), all = TRUE, sort = TRUE)
      },
      x = list(coords_long, contrib_long, cos2_long)
    )
  }
)
