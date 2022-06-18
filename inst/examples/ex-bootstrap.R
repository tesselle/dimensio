library(ggrepel)

## Bootstrap on CA
## Data from Lebart et al. 2006, p. 170-172
data("colours")

## Compute correspondence analysis
X <- ca(colours)

## Plot results
plot(X) +
  ggrepel::geom_label_repel()

## Bootstrap (30 replicates)
Y <- bootstrap(X, n = 30)

\dontrun{
## Get replicated coordinates
get_replications(Y, margin = 1)
get_replications(Y, margin = 2)
}

## Plot with ellipses
plot_rows(Y) +
  ggplot2::stat_ellipse()

plot_columns(Y) +
  ggplot2::stat_ellipse()

## Bootstrap on PCA
## Compute principal components analysis
data(iris)
X <- pca(iris)

## Plot results
plot_columns(X) +
  ggrepel::geom_label_repel()

## Bootstrap (30 replicates)
Y <- bootstrap(X, n = 30)

## Plot with ellipses
plot_columns(Y) +
  ggplot2::stat_ellipse()
