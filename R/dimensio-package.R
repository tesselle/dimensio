#' @details
#' \tabular{ll}{
#'  **Package:** \tab dimensio \cr
#'  **Type:** \tab Package \cr
#'  **Version:** \tab 0.8.0 \cr
#'  **License:** \tab GPL-3 \cr
#'  **Zenodo:** \tab \doi{10.5281/zenodo.4478530} \cr
#' }
#'
#' @author
#' **Full list of authors and contributors** (alphabetic order)
#'
#' \tabular{ll}{
#'  Jean-Baptiste Fourvel \tab *CNRS, France* \cr
#'  Nicolas Frerebeau \tab *Université Bordeaux Montaigne, France* \cr
#'  Brice Lebrun \tab *Université Bordeaux Montaigne, France* \cr
#' }
#'
#' **Package maintainer**
#'
#' Nicolas Frerebeau\cr
#' \email{nicolas.frerebeau@@u-bordeaux-montaigne.fr}
#'
#' Archéosciences Bordeaux (UMR 6034)\cr
#' Maison de l'Archéologie\cr
#' Université Bordeaux Montaigne\cr
#' F-33607 Pessac cedex\cr
#' France
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
#' @importFrom stats aggregate approx cov na.omit qchisq qf rmultinom runif
#' @importFrom utils capture.output modifyList stack tail
NULL

# Notes:
# matrix * vector is faster (!) than:
# matrix %*% t(vector)
# t(t(matrix) * vector)
# https://stackoverflow.com/questions/18349053/fastest-way-for-multiplying-a-matrix-to-a-vector
