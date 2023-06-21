## Load data
data("iris")

## Compute principal components analysis
X <- pca(iris, scale = TRUE)

## Get row contributions
head(get_contributions(X, margin = 1))

## Plot contributions
viz_contributions(X)

## Plot cos2
viz_cos2(X)
