## Load data
data("iris")

## Compute principal components analysis
X <- pca(iris, scale = TRUE)

## Plot individuals
viz_individuals(X, colour = iris$Species, shape = iris$Species)

viz_individuals(X, colour = iris$Sepal.Width, size = "cos2")

viz_individuals(X, colour = "contribution", size = "contribution", pch = 16)

## Plot variables
viz_variables(X)

viz_variables(X, colour = "contribution")
