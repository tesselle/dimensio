## Bootstrap on CA
## Data from Lebart et al. 2006, p. 170-172
data("colours")

## Compute correspondence analysis
X <- ca(colours)

## Bootstrap (30 replicates)
Y <- bootstrap(X, n = 30)

\dontrun{
## Get replicated coordinates
get_replications(Y, margin = 1)
get_replications(Y, margin = 2)
}

## Plot with ellipses
viz_rows(Y)
viz_tolerance(Y, margin = 1, level = c(0.68, 0.95))

viz_columns(Y)
viz_tolerance(Y, margin = 2, level = c(0.68, 0.95))

## Plot with convex hulls
viz_columns(Y)
viz_hull(Y, margin = 2)

## Bootstrap on PCA
## Compute principal components analysis
data("iris")
X <- pca(iris, sup_quali = "Species")

## Bootstrap (30 replicates)
Y <- bootstrap(X, n = 30)

## Plot with ellipses
viz_variables(Y)
viz_tolerance(Y, margin = 2, level = c(0.68, 0.95))
