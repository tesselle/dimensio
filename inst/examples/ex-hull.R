## Load data
data("iris")

## Compute principal components analysis
X <- pca(iris, scale = TRUE, sup_quali = "Species")

## Compute convex hull coordinates
hulls <- wrap_hull(X, margin = 1, group = "Species")

## Plot convex hulls
col <- c("#004488", "#DDAA33", "#BB5566")
viz_rows(X, extra_quali = iris$Species, color = col)
viz_hull(X, group = iris$Species, color = col)
