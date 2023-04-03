
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dimensio <img width=120px src="man/figures/logo.png" align="right" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/tesselle/dimensio/workflows/R-CMD-check/badge.svg)](https://github.com/tesselle/dimensio/actions)
[![codecov](https://codecov.io/gh/tesselle/dimensio/branch/main/graph/badge.svg?token=0mcb7gbZu3)](https://app.codecov.io/gh/tesselle/dimensio)
[![CodeFactor](https://www.codefactor.io/repository/github/tesselle/dimensio/badge/main)](https://www.codefactor.io/repository/github/tesselle/dimensio/overview/main)
[![Dependencies](https://tinyverse.netlify.com/badge/dimensio)](https://cran.r-project.org/package=dimensio)

<a href="https://tesselle.r-universe.dev" class="pkgdown-devel"><img
src="https://tesselle.r-universe.dev/badges/dimensio"
alt="r-universe" /></a>
<a href="https://cran.r-project.org/package=dimensio"
class="pkgdown-release"><img
src="http://www.r-pkg.org/badges/version/dimensio"
alt="CRAN Version" /></a> <a
href="https://cran.r-project.org/web/checks/check_results_dimensio.html"
class="pkgdown-release"><img
src="https://cranchecks.info/badges/worst/dimensio"
alt="CRAN checks" /></a>
<a href="https://cran.r-project.org/package=dimensio"
class="pkgdown-release"><img
src="http://cranlogs.r-pkg.org/badges/dimensio"
alt="CRAN Downloads" /></a>

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4478530.svg)](https://doi.org/10.5281/zenodo.4478530)
<!-- badges: end -->

## Overview

Simple Principal Components Analysis (PCA) and Correspondence Analysis
(CA) based on the Singular Value Decomposition (SVD). This package
provides S4 classes and methods to compute, extract, summarize and
visualize results of multivariate data analysis. It also includes
methods for partial bootstrap validation.

There are many very good packages for multivariate data analysis (such
as [**FactoMineR**](http://factominer.free.fr/),
[**ade4**](https://pbil.univ-lyon1.fr/ade4/),
[**vegan**](https://rpubs.com/brouwern/veganpca) or
[**ca**](https://cran.r-project.org/package=ca), all extended by
[**FactoExtra**](https://rpkgs.datanovia.com/factoextra/)). **dimensio**
is designed to be as simple as possible, providing all the necessary
tools to explore the results of the analysis.

## Installation

You can install the released version of **dimensio** from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("dimensio")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("tesselle/dimensio")
```

## Usage

``` r
## Load packages
library(dimensio)

library(ggplot2)
library(ggrepel)
library(khroma)
```

### Compute

``` r
## Load data
data(iris)

## Compute PCA
## (non numeric variables are automatically removed)
X <- pca(iris, center = TRUE, scale = TRUE)
#> 1 qualitative variable was removed: Species.
```

### Extract

**dimensio** provides several methods to extract the results:

- `get_data()` returns the original data.
- `get_contributions()` returns the contributions to the definition of
  the principal dimensions.
- `get_coordinates()` returns the principal or standard coordinates.
- `get_correlations()` returns the correlations between variables and
  dimensions.
- `get_cos2()` returns the cos<sup>2</sup> values (i.e. the quality of
  the representation of the points on the factor map).
- `get_eigenvalues()` returns the eigenvalues, the percentages of
  variance and the cumulative percentages of variance.

### Visualize

The package allows to quickly visualize the results:

- `plot_rows()`/`plot_individuals()` displays row/individual principal
  coordinates.
- `plot_columns()`/`plot_variables()` displays columns/variable
  principal coordinates. `plot_variables()` depicts the variables by
  rays emanating from the origin (both their lengths and directions are
  important to the interpretation).
- `plot_contributions()` displays (joint) contributions.
- `plot_cos2()` displays (joint) cos<sup>2</sup>.
- `plot_variance()` produces a scree plot.

**dimensio** uses [**ggplot2**](https://github.com/tidyverse/ggplot2)
for plotting informations. Visualization methods produce graphics with
as few elements as possible: this makes it easy to customize diagrams
(e.g. using extra layers, themes and scales). The `plot_*()` functions
allow to highlight additional information by varying different graphical
elements (color, transparency, shape and size of symbols…).

``` r
## Form biplot
biplot(X, type = "form", label = "variables") +
  ggrepel::geom_label_repel() + # Add repelling labels
  ggplot2::theme_bw() + # Change theme
  ggplot2::theme(legend.position = "none") +
  khroma::scale_colour_highcontrast() # Custom color scale
```

<img src="man/figures/README-biplot-1.png" style="display: block; margin: auto;" />

``` r
## Highlight species
plot_individuals(X, colour = "group", group = iris$Species) +
  ggplot2::stat_ellipse() + # Add ellipses
  ggplot2::theme_bw() + # Change theme
  khroma::scale_colour_highcontrast() # Custom color scale
#> Warning: The following aesthetics were dropped during statistical transformation: label
#> ℹ This can happen when ggplot fails to infer the correct grouping structure in
#>   the data.
#> ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
#>   variable into a factor?

## Highlight petal length
plot_individuals(X, colour = "group", size = "group", group = iris$Petal.Length) +
  ggplot2::theme_bw() + # Change theme
  ggplot2::scale_size_continuous(range = c(1, 3)) + # Custom size scale
  khroma::scale_color_iridescent() # Custom color scale
```

<img src="man/figures/README-plot-ind-1.png" width="50%" /><img src="man/figures/README-plot-ind-2.png" width="50%" />

``` r
## Plot variables factor map
plot_variables(X) +
  ggrepel::geom_label_repel() + # Add repelling labels
  ggplot2::theme_bw() # Change theme

## Highlight contributions
plot_variables(X, colour = "contrib") +
  ggrepel::geom_label_repel() + # Add repelling labels
  ggplot2::theme_bw() + # Change theme
  khroma::scale_color_YlOrBr(range = c(0.5, 1)) # Custom color scale
```

<img src="man/figures/README-plot-var-1.png" width="50%" /><img src="man/figures/README-plot-var-2.png" width="50%" />

``` r
## Scree plot
plot_variance(X, variance = TRUE, cumulative = TRUE) +
  ggplot2::geom_text(nudge_y = 3) + # Add labels
  ggplot2::theme_bw() # Change theme

## Plot variables contributions to the definition of the first component
plot_contributions(X, margin = 2, axes = 1) +
  ggplot2::geom_text(nudge_y = 2) + # Add labels
  ggplot2::theme_bw() + # Change theme
  ggplot2::theme( # Edit theme
    # Rotate x axis labels
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1)
  )
```

<img src="man/figures/README-plot-eig-1.png" width="50%" /><img src="man/figures/README-plot-eig-2.png" width="50%" />

## Contributing

Please note that the **dimensio** project is released with a
[Contributor Code of Conduct](https://www.tesselle.org/conduct.html). By
contributing to this project, you agree to abide by its terms.
