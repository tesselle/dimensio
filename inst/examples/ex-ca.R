## Load data
data("zuni", package = "codex")

## The chi square of independence between the two variables
stats::chisq.test(zuni)

## Compute correspondence analysis
X <- ca(zuni)

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
