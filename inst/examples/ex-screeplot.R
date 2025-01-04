## Load data
data("iris")

## Compute principal components analysis
X <- pca(iris, scale = TRUE, sup_quali = "Species")

## Screeplot
screeplot(X)
screeplot(X, cumulative = TRUE)
