## Create a matrix
A <- matrix(data = sample(1:10, 100, TRUE), nrow = 10, ncol = 10)

## Compute principal components analysis
X <- pca(A, scale = TRUE, sup_ind = 8:10, sup_var = 7:10)

## Get results for the individuals
X[["individuals"]]

## Compute correspondence analysis
Y <- ca(A, sup_row = 8:10, sup_col = 7:10)

## Get results for the rows
Y[["rows"]]
