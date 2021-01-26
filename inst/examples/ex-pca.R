## Load data
library(codex)
data("compiegne")

## Compute principal components analysis
X <- pca(compiegne, scale = TRUE, sup_var = 7:10)

## Get row coordinates
get_coordinates(X, margin = 1)

## Get column coordinates
get_coordinates(X, margin = 2)

## Get row contributions
get_contributions(X, margin = 1)

## Get correlations between variables and dimensions
get_correlations(X)

## Get eigenvalues
get_eigenvalues(X)
