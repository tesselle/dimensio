#' @details
#' \tabular{ll}{
#'  **Version** \tab 0.14.0 \cr
#'  **License** \tab GPL-3 \cr
#'  **CRAN DOI** \tab \doi{10.32614/CRAN.package.dimensio} \cr
#'  **Zenodo DOI** \tab \doi{10.5281/zenodo.4478530} \cr
#' }
#'
#' Archéosciences Bordeaux (UMR 6034)\cr
#' Maison de l'Archéologie\cr
#' Université Bordeaux Montaigne\cr
#' F-33607 Pessac cedex\cr
#' France
#'
#' @section Package options:
#'  \pkg{dimensio} uses the following [options()] to configure behavior:
#'  * `dimensio.verbose`: a [`logical`] scalar. Should \R report extra
#'    information on progress? Defaults to [interactive()].
#'
#' @name dimensio-package
#' @aliases dimensio
#' @docType package
#' @keywords internal
"_PACKAGE"

#' @import arkhe
#' @importFrom grDevices adjustcolor as.raster chull colorRamp hcl.colors rgb
#' @importFrom methods Arith as as<- callGeneric callNextMethod
#' .hasSlot initialize is new setClass setClassUnion
#' setGeneric setMethod slot slot<- slotNames validObject .valueClassTest
#' @importFrom stats aggregate approx cmdscale cov na.omit qchisq qf rmultinom
#' runif
#' @importFrom utils capture.output modifyList stack tail
NULL

# Notes:
# matrix * vector is faster (!) than:
# matrix %*% t(vector)
# t(t(matrix) * vector)
# https://stackoverflow.com/questions/18349053/fastest-way-for-multiplying-a-matrix-to-a-vector
