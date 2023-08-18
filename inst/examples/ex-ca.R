## Data from Lebart et al. 2006, p. 170-172
data("colours")

## The chi square of independence between the two variables
stats::chisq.test(colours)

## Compute correspondence analysis
X <- ca(colours)

## Plot rows
viz_rows(X, labels = TRUE)

## Plot columns
viz_columns(X, labels = TRUE)

## Get row coordinates
head(get_coordinates(X, margin = 1))

## Get column coordinates
head(get_coordinates(X, margin = 2))

## Get row distances to centroid
head(get_distances(X, margin = 1))

## Get row inertias
head(get_inertia(X, margin = 1))

## Get row contributions
head(get_contributions(X, margin = 1))

## Get eigenvalues
get_eigenvalues(X)
