## Load data
data("iris")

## Compute principal components analysis
X <- pca(iris, scale = TRUE, sup_quali = "Species")

## Plot with tolerance ellipses
col <- c("#004488", "#DDAA33", "#BB5566")
viz_rows(X, extra_quali = iris$Species, color = col)
viz_ellipses(
  x = X,
  type = "tolerance",
  level = c(0.68, 0.95),
  group = iris$Species,
  color = col
)
