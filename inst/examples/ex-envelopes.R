## Load data
data("iris")

## Compute principal components analysis
X <- pca(iris, scale = TRUE, sup_quali = "Species")

## Plot with convex hulls
col <- c("#004488", "#DDAA33", "#BB5566")
viz_rows(X, extra_quali = iris$Species, color = col)
viz_hull(X, group = iris$Species, color = col)

## Plot with tolerance ellipses
col <- c("#004488", "#DDAA33", "#BB5566")
viz_rows(X, extra_quali = iris$Species, color = col)
viz_tolerance(X, group = iris$Species, color = col)
