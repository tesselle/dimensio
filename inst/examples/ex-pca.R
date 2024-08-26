## Load data
data("iris")

## Compute principal components analysis
X <- pca(iris)

## Get eigenvalues
get_eigenvalues(X)

## Get individual cos2
head(get_cos2(X, margin = 1))

## Get variable contributions
get_contributions(X, margin = 2)

## Get correlations between variables and dimensions
get_correlations(X)
