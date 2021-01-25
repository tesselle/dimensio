## Create a matrix
A <- matrix(data = sample(1:10, 100, TRUE), nrow = 20)

## Compute correspondence analysis
X <- ca(A, sup_row = 11:15)

## Plot observations
plot_coordinates(X)
