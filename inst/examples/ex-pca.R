## Create a matrix
A <- matrix(data = sample(1:10, 100, TRUE), nrow = 10, ncol = 10)

## Compute principal components analysis
X <- pca(A, scale = TRUE, sup_ind = 8:10, sup_var = 7:10)

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
