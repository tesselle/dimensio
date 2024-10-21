\dontrun{
## Load data
data("iris")

## Compute principal components analysis
X <- pca(iris)

## Export results
export(X, file = "results.zip")
}
