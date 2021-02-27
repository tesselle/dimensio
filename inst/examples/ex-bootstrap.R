library(ggrepel)

## Partial bootstrap on CA
## Data from Lebart et al. 2006, p. 170-172
color <- data.frame(
  brun = c(68, 15, 5, 20),
  chatain = c(119, 54, 29, 84),
  roux = c(26, 14, 14, 17),
  blond = c(7, 10, 16, 94),
  row.names = c("marron", "noisette", "vert", "bleu")
)

## Compute correspondence analysis
X <- ca(color)

## Plot results
plot(X) +
  ggrepel::geom_label_repel()

## Bootstrap (30 replicates)
Y <- bootstrap(X, n = 30)

\donttest{
## Get replicated coordinates
get_replications(Y, margin = 1)
get_replications(Y, margin = 2)
}

## Plot with ellipses
plot_rows(Y) +
  ggplot2::stat_ellipse()

plot_columns(Y) +
  ggplot2::stat_ellipse()

## Partial bootstrap on PCA
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
