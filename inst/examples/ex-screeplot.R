## Load data
data("iris")

## Compute principal components analysis
X <- pca(iris, scale = TRUE)

## Screeplot
screeplot(X)
screeplot(X, cumulative = TRUE)
