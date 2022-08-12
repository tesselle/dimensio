## Replicate examples from Greenacre 2007, p. 59-68
data("countries")

## Compute principal components analysis
## All rows and all columns obtain the same weight
row_w <- rep(1 / nrow(countries), nrow(countries)) # 1/13
col_w <- rep(1 / ncol(countries), ncol(countries)) # 1/6
Y <- pca(countries, scale = FALSE, weight_row = row_w, weight_col = col_w)

## Row-metric-preserving biplot (form biplot)
biplot(Y, type = "form") +
  ggrepel::geom_label_repel()

## Column-metric-preserving biplot (covariance biplot)
biplot(Y, type = "covariance") +
  ggrepel::geom_label_repel()

## Replicate examples from Greenacre 2007, p. 79-88
data("benthos")

## Compute correspondence analysis
X <- ca(benthos)

## Row principal CA biplot
biplot(X, type = "row") +
  ggrepel::geom_label_repel()

## Column principal CA biplot
biplot(X, type = "column") +
  ggrepel::geom_label_repel()

## Contribution CA biplot
biplot(X, type = "contrib") +
  ggrepel::geom_label_repel()
