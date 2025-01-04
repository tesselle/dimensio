\dontrun{
## Load data
data("iris")

## Compute principal components analysis
X <- pca(iris, sup_quali = "Species")

## Export results
export(X, file = "results.zip")
}
