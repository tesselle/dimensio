## Load data
library(codex)
data("mississippi")

## Compute principal components analysis
X <- pca(mississippi, scale = TRUE, sup_ind = 8:10, sup_var = 7:10)

## Get results for the individuals
X[["individuals"]]

## Compute correspondence analysis
Y <- ca(mississippi, sup_row = 8:10, sup_col = 7:10)

## Get results for the rows
Y[["rows"]]
