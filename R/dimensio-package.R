#' @details
#' \tabular{ll}{
#'  \strong{Package:} \tab dimensio \cr
#'  \strong{Type:} \tab Package \cr
#'  \strong{Version:} \tab 0.2.0 \cr
#'  \strong{License:} \tab GPL-3 \cr
#'  \strong{DOI:} \tab \href{https://doi.org/10.5281/zenodo.4478530}{10.5281/zenodo.4478530}
#' }
#' @author
#' \strong{Full list of authors and contributors} (alphabetic order)
#'
#' \tabular{ll}{
#'  Jean-Baptiste Fourvel \tab \emph{CNRS, France} \cr
#'  Nicolas Frerebeau \tab \emph{Université Bordeaux Montaigne, France} \cr
#'  Brice Lebrun \tab \emph{Université Bordeaux Montaigne, France} \cr
#' }
#'
#' \strong{Package maintainer}
#'
#' Nicolas Frerebeau\cr
#' \email{nicolas.frerebeau@@u-bordeaux-montaigne.fr}
#'
#' IRAMAT-CRP2A (UMR 5060)\cr
#' Maison de l'Archéologie\cr
#' Université Bordeaux Montaigne\cr
#' F-33607 Pessac cedex\cr
#' France
#' @name dimensio-package
#' @aliases dimensio
#' @docType package
#' @keywords internal
"_PACKAGE"

#' @importFrom ggplot2 aes arrow coord_fixed geom_col geom_hline geom_line
#' geom_path geom_point geom_segment geom_vline ggplot scale_x_continuous
#' scale_x_discrete scale_y_continuous sec_axis waiver
#' @importFrom methods Arith as as<- callGeneric callNextMethod
#' .hasSlot initialize is new setClass setClassUnion
#' setGeneric setMethod slot slot<- slotNames validObject .valueClassTest
#' @importFrom rlang .data
NULL

# Notes:
# matrix * vector is faster (!) than:
# matrix %*% t(vector)
# t(t(matrix) * vector)
# https://stackoverflow.com/questions/18349053/fastest-way-for-multiplying-a-matrix-to-a-vector
