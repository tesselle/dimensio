## Jackknife
x <- rnorm(20)
jack <- jackknife(x, do = mean) # Sample mean
summary(jack)
