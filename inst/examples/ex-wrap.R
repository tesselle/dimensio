## Load data
data("iris")

## Compute principal components analysis
X <- pca(iris, scale = TRUE)

## Plot results
plot_rows(X, colour = "group", group = iris$Species) +
    khroma::scale_colour_highcontrast()

## Convex hull coordinates
hulls <- wrap_hull(X, group = iris$Species)
head(hulls)

## Plot with convex hulls
plot_rows(X, colour = "group", group = iris$Species) +
  stat_hull(geom = "path") +
  khroma::scale_colour_highcontrast()
