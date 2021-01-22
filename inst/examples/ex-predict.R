## Create count data matrix
A <- matrix(data = sample(1:10, 100, TRUE), nrow = 10, ncol = 10)
B <- matrix(data = sample(12:20, 42, TRUE), nrow = 7, ncol = 6)

## Compute correspondence analysis
X <- ca(A, sup_rows = 8:10, sup_columns = 7:10)

## Predict new row coordinates
predict(X, B, margin = 1)

## Predict new column coordinates
predict(X, B, margin = 2)
