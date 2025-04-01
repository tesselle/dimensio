## Load data
data("iris")

## Compute principal components analysis
X <- pca(iris, scale = TRUE, sup_quali = "Species")

## Compute confidence ellipse coordinates
conf <- wrap_confidence(X, margin = 1, group = "Species", level = 0.95)

## Plot confidence ellipses
col <- c("#004488", "#DDAA33", "#BB5566")
viz_rows(X, extra_quali = iris$Species, color = col)
viz_confidence(X, group = iris$Species, color = col, level = 0.95)
