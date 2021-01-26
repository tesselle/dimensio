## Load data
library(codex)
data("compiegne")

## Compute correspondence analysis
X <- ca(compiegne, sup_col = 7:10)

## Get row coordinates
get_coordinates(X, margin = 1)

## Get column coordinates
get_coordinates(X, margin = 2)

## Get row distances to centroid
get_distances(X, margin = 1)

## Get row inertias
get_inertia(X, margin = 1)

## Get row contributions
get_contributions(X, margin = 1)

## Get eigenvalues
get_eigenvalues(X)
