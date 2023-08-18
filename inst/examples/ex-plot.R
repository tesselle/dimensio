## Load data
data("iris")

## Compute principal components analysis
X <- pca(iris, scale = TRUE)

## Plot individuals
viz_individuals(X, map_color = iris$Species, map_shape = iris$Species)

viz_individuals(X, map_color = iris$Sepal.Width, map_size = "cos2")

viz_individuals(X, map_color = "contribution", map_size = "contribution",
                scale_shape = 16)

## Plot variables
viz_variables(X)

viz_variables(X, map_color = "contribution")
