## Create count data matrix
A <- matrix(data = sample(1:10, 100, TRUE), nrow = 10, ncol = 10)
B <- matrix(data = sample(12:20, 42, TRUE), nrow = 7, ncol = 6)

## Compute correspondence analysis
X <- ca(A, sup_rows = 8:10, sup_columns = 7:10)

## Get row coordinates
get_coordinates(X, margin = 1, standard = FALSE)

## Get column coordinates
get_coordinates(X, margin = 2, standard = FALSE)

## Get row distances to centroid
get_distances(X, margin = 1)

## Get row inertias
get_inertia(X, margin = 1)

## Get row contributions
get_contributions(X, margin = 1)

## Get eigenvalues
get_eigenvalues(X)
