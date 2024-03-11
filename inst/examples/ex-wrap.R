## Load data
data("iris")

## Compute principal components analysis
X <- pca(iris, scale = TRUE, sup_quali = "Species")

## Confidence ellipse coordinates
conf <- wrap_confidence(X, margin = 1, group = "Species", level = c(0.68, 0.95))

## Tolerance ellipse coordinates
conf <- wrap_confidence(X, margin = 1, group = "Species", level = 0.95)

## Convex hull coordinates
hulls <- wrap_hull(X, margin = 1, group = "Species")
