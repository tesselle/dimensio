# dimensio 0.3.0.9000

# dimensio 0.3.0
## New classes and methods
* Add `biplot()` to produce PCA and CA biplots.
* Add `plot_individuals()` and `plot_variables()` methods for `PCA` class.
* Add `tidy()` and `augment()` to get coordinates in tidy format with extra information.
* Add `wrap_hull()` to compute convex hull around coordinates.
* Add a **ggplot2** layer: `stat_hull()` (thanks to [**ggplot2** documentation](https://ggplot2.tidyverse.org/articles/extending-ggplot2.html))

## Enhancements
* `plot_rows` and `plot_columns()` gained new arguments allowing to highlight additional information by varying different graphical elements.
* `get_coordinates()` gained a new argument allowing to select principal or standard coordinates.

## Breaking changes
* Remove `plot()` methods for `PCA` and `CA` classes.
* Remove `bootstrap()` and `jackknife()` methods for `numeric` and `integer` vectors.

# dimensio 0.2.2
## New classes and methods
* Add `bootstrap()` and `jackknife()` methods for `numeric` and `integer` vectors.

## Bugfixes & changes
* Fix sign of singular vectors for consistency with **FactoMineR**.

## Internals
* Add `BootstrapVector` and `JackknifeVector` classes to store bootstrap and jackknife resampling values (inherit from base `numeric`).

# dimensio 0.2.1
## Bugfixes & changes
* Fix row names in `build_results()`. When initializing a `MultivariateResults` object with supplementary observations, the row names of the `standard` and `contributions` matrices were incorrect (computation moves all supplementary points at the end of the results).

# dimensio 0.2.0
## New classes and methods
* Add `bootstrap()` for partial bootstrap analysis.
* Add `BootstrapCA` and `BootstrapPCA`: S4 classes to store partial bootstrap analysis.

## Bugfixes & changes
* Rename `plot_individuals()` and `plot_variables()` to `plot_rows()` and `plot_columns()`.
* Remove `plot_eigenvalues()`.

# dimensio 0.1.0
* Prelease.
