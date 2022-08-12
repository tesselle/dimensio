## Bootstrap on CA
## Data from Lebart et al. 2006, p. 170-172
data("colours")

## Compute correspondence analysis
X <- ca(colours)

## Bootstrap (30 replicates)
Y <- bootstrap(X, n = 30)

\dontrun{
## Get replicated coordinates
get_replications(Y, margin = 1)
get_replications(Y, margin = 2)
}

## Plot with ellipses
plot_rows(Y, colour = "group") +
  ggplot2::stat_ellipse()

## Plot with convex hulls
plot_columns(Y, colour = "group", fill = "group") +
  stat_hull(geom = "polygon", alpha = 0.5)

## Bootstrap on PCA
## Compute principal components analysis
data("iris")
X <- pca(iris)

## Bootstrap (30 replicates)
Y <- bootstrap(X, n = 30)

## Plot with ellipses
plot_columns(Y, colour = "group") +
  ggplot2::stat_ellipse()
