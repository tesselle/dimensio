# dimensio 0.10.0.9000
## Bugfixes & changes
* Fix ellipses default border color.

# dimensio 0.10.0
## New classes and methods
* Add `export()` to create a Zip archive of all results in CSV format.

## Internals
* Translate into French.
* Compute the position and draw only the labels in the plotting region.
* Add type checking of arguments.

# dimensio 0.9.0
## New classes and methods
* Add `pcoa()` to compute principal coordinates analysis.

## Enhancements
* Allow to display extra qualitative and quantitative variables in `viz_rows()`/`viz_individuals()` and `viz_columns()`/`viz_variables()`.

## Bugfixes & changes
* Deprecate `hightlight` argument of `viz_rows()`/`viz_individuals()` and `viz_columns()`/`viz_variables()`.

# dimensio 0.8.1
## Internals
* Use palette functions from **khroma**.

# dimensio 0.8.0
## Enhancements
* Improve aesthetic mapping in scatterplots.
* Faster computation of non-overlapping labels.

## Internals
* Store supplementary variables in a list in `MultivariateAnalysis` object.

# dimensio 0.7.0
## Bugfixes & changes
* Fix highlighting in `viz_individuals()` and `viz_row()`.
* The default number of labeled points can now be changed in `viz_individuals()`, `viz_row()`, `viz_variables()` and `viz_columns()`.

## Enhancements
* Allow to highlight supplementary qualitative variables in `viz_individuals()`, `viz_row()`, `viz_variables()` and `viz_columns()`.

# dimensio 0.6.0
## New classes and methods
* Add `predict()` method for MCA.

## Bugfixes & changes
* By default, `viz_individuals()`, `viz_row()`, `viz_variables()` and `viz_columns()` only display labels of the 10 observations contributing the most to the factorial map.

## Enhancements
* `viz_contributions()` displays the expected average contribution.
* `pca()` gained a new argument to specify supplementary qualitative variables.

## Breaking changes
* `plot_*()` methods are now defunct (deprecated in v0.4.0).

# dimensio 0.5.0
## New classes and methods
* Add `cdt()` to compute the complete disjunctive table of a factor table.
* Add `burt()` to compute the Burt table of a factor table.
* Add `mca()` to compute multiple correspondence analysis.

## Enhancements
* `biplot()` allows to produce a symetric CA biplot.
* Set graphical parameters as arguments in `biplot()`.
* Add automatic legend in `biplot()`.

## Internals
* Import **arkhe**.

# dimensio 0.4.1
## Bugfixes & changes
* Put `...` after required arguments.

## Enhancements
* `viz_individuals()`, `viz_row()`, `viz_variables()`, `viz_columns()` and `biplot()` gained new arguments allowing set the x and y limits of the plot.

# dimensio 0.4.0
## New classes and methods
* Add `screeplot()` to produce a scree plot.
* Add `viz_*()` methods to replace `plot_*()` methods.

## Bugfixes & changes
* Deprecate `plot_*()` methods. The internal use of **ggplot2** is poorly interoperable or composable. This will reduces hard dependencies.

# dimensio 0.3.1
## Bugfixes & changes
* Fix `rownames()` and `colnames()` for S3 generic/method consistency.

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
