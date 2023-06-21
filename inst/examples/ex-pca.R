## Load data
data("iris")

## Compute principal components analysis
X <- pca(iris, scale = TRUE, sup_row = sample(150, 10), sup_col = 2)

## Get row coordinates
head(get_coordinates(X, margin = 1))

## Get column coordinates
head(get_coordinates(X, margin = 2))

## Get correlations between variables and dimensions
head(get_correlations(X))

## Get eigenvalues
get_eigenvalues(X)
