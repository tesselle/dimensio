## Load data
data("iris")

## Compute principal components analysis
X <- pca(iris, scale = TRUE)

## Convex hull coordinates
hulls <- wrap_hull(X, margin = 1, group = iris$Species)

## Confidence ellipse coordinates
conf <- wrap_confidence(X, margin = 1, group = iris$Species,
                        level = c(0.68, 0.95))

## Tolerance ellipse coordinates
conf <- wrap_confidence(X, margin = 1, group = iris$Species, level = 0.95)

## Plot with convex hulls
viz_rows(X, colour = iris$Species, col = khroma::color("high contrast")(3))
viz_hull(X, group = iris$Species, border = khroma::color("high contrast")(3))
