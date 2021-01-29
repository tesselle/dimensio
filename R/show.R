# SHOW METHODS
#' @include AllClasses.R
NULL

setMethod(
  f = "show",
  signature = "MultivariateAnalysis",
  definition = function(object) {
    header <- format_header(object)
    cat(header, sep = "\n")
    invisible(object)
  }
)
setMethod(
  f = "show",
  signature = "MultivariateSummary",
  definition = function(object) {
    ## Get options
    n_dig <- getOption("dimensio.digits")
    n_max <- getOption("dimensio.max.print")

    analysis <- switch (
      class(object),
      SummaryCA = c("rows", "columns"),
      SummaryPCA = c("individuals", "variables")
    )

    ## Get data
    eig <- round(object@eigenvalues, digits = n_dig)
    res <- round(object@results, digits = n_dig)
    mar <- analysis[[object@margin]]

    ## Prepare data
    is_sup <- object@supplement
    eigen <- c("\nEigenvalues:", utils::capture.output(eig))

    ## Supplementary points
    sum_sup <- extra_sup <- NULL
    if (any(is_sup)) {
      res_sup <- res[is_sup, ]
      n_sup <- nrow(res_sup)
      if (n_sup > n_max) {
        res_sup <- res_sup[seq_len(n_max), ]
        extra_sup <- sprintf("(%s more)", n_sup)
      }
      is_na <- apply(X = res_sup, MARGIN = 2, FUN = anyNA)
      res_sup <- res_sup[, !is_na]
      sum_sup <- c(sprintf("\nSupplementary %s:", mar),
                   utils::capture.output(res_sup))
    }

    ## Active points
    sum_act <- extra_act <- NULL
    if (any(!is_sup)) {
      res_act <- res[!is_sup, ]
      n_act <- nrow(res_act)
      if (n_act > n_max) {
        res_act <- res_act[seq_len(n_max), ]
        extra_act <- sprintf("(%s more)", n_act)
      }
      sum_act <- c(sprintf("\nActive %s:", mar), utils::capture.output(res_act))
    }

    ## Print
    header <- format_header(object)
    cat(header, eigen, sum_act, extra_act, sum_sup, extra_sup, sep = "\n")
    invisible(object)
  }
)

format_header <- function(object, width = getOption("width")) {
  analysis <- switch (
    class(object),
    CA = "Correspondence Analysis (CA)",
    BootstrapCA = "Correspondence Analysis (CA)",
    SummaryCA = "Correspondence Analysis (CA)",
    PCA = "Principal Components Analysis (PCA)",
    BootstrapPCA = "Principal Components Analysis (PCA)",
    SummaryPCA = "Principal Components Analysis (PCA)"
  )

  n_dashes <- width - nchar(analysis) - 4
  dashes <- paste0(rep("-", n_dashes), collapse = "")
  sprintf("--- %s %s", analysis, dashes)
}
