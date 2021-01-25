
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dimensio

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

Simple Principal Components Analysis (PCA) and Correspondence Analysis
(CA). This package provides S4 classes and methods to compute, extract,
summarize and visualize results of multivariate data analysis.

There are many very good packages for multivariate data analysis (such
as [**FactoMineR**](http://factominer.free.fr/),
[**ade4**](https://pbil.univ-lyon1.fr/ade4/) or
[**ca**](https://cran.r-project.org/package=ca), all extended by
[**FactoExtra**](https://rpkgs.datanovia.com/factoextra)). **dimensio**
is designed to have as few dependencies as possible: computation and
exploration of the results depends only on the R base packages.
Visualization methods require
[**ggplot2**](https://ggplot2.tidyverse.org/).

## Installation

You can install the released version of **dimensio** from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("dimensio")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("nfrerebeau/dimensio")
```

## Usage

``` r
library(dimensio)

X <- pca(iris, scale = TRUE, sup_ind = 50:75)
#> 1 qualitative variable was removed: Species.
```

### Summarize

### Extract

### Visualize

**dimensio** uses [**ggplot2**](https://github.com/tidyverse/ggplot2)
for plotting informations. Visualization methods produce graphics with
as few elements as possible: this makes it easy to customize diagrams
(e.g. using extra layers, themes and scales).

``` r
library(ggplot2)
library(ggrepel)
library(khroma)
```

``` r
## Plot active individuals by group
plot_individuals(X, group = iris$Species, active = TRUE, sup = FALSE) +
  ggplot2::stat_ellipse() + # Add ellipses
  ggplot2::theme_bw() + # Change theme
  khroma::scale_color_contrast() # Custom color scale

## Plot all individuals by cos2
plot_individuals(X, highlight = "cos2", active = TRUE, sup = TRUE) +
  ggplot2::theme_bw() + # Change theme
  khroma::scale_color_iridescent() # Custom color scale
```

![](man/figures/README-plot-ind-1.png)![](man/figures/README-plot-ind-2.png)

``` r
## Plot variables factor map
plot_variables(X) +
  ggrepel::geom_label_repel() + # Add repelling labels
  ggplot2::theme_bw() + # Change theme
  ggplot2::theme(legend.position = "bottom") # Edit theme

## Highlight contributions
plot_variables(X, highlight = "contrib") +
  ggrepel::geom_label_repel() + # Add repelling labels
  ggplot2::theme_bw() + # Change theme
  ggplot2::theme(legend.position = "bottom") + # Edit theme
  khroma::scale_color_YlOrBr(range = c(0.5, 1)) # Custom color scale
```

![](man/figures/README-plot-var-1.png)![](man/figures/README-plot-var-2.png)

``` r
## Plot eigenvalues
plot_eigenvalues(X) +
  ggplot2::theme_bw() # Change theme

## Plot percentages of variance
plot_variance(X, variance = TRUE, cumulative = TRUE) +
  ggplot2::geom_text(nudge_y = 3) + # Add labels
  ggplot2::theme_bw() # Change theme

## Plot variables contributions
plot_contributions(X, margin = 2, axes = 1) +
  ggplot2::theme_bw() + # Change theme
  ggplot2::theme( # Edit theme
    # Rotate x axis labels
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1)
  )
```

![](man/figures/README-plot-eig-1.png)![](man/figures/README-plot-eig-2.png)![](man/figures/README-plot-eig-3.png)

## Contributing

Please note that the **dimensio** project is released with a
[Contributor Code of
Conduct](https://github.com/nfrerebeau/dimensio/blob/master/.github/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.
