## Load data
data("iris")

## Compute principal components analysis
X <- pca(iris, scale = TRUE, sup_row = 8:10, sup_col = 1, sup_quali = "Species")

## Get results for the individuals
X[["rows"]]
