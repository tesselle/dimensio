# SHOW METHODS
#' @include AllClasses.R
NULL

setMethod(
  f = "show",
  signature = "MultivariateAnalysis",
  definition = function(object) {
    analysis <- switch (
      class(object),
      CA = "Correspondence Analysis (CA)",
      PCA = "Principal Components Analysis (CA)"
    )
    cat(analysis, sep = "\n")
    invisible(object)
  }
)
setMethod(
  f = "show",
  signature = "MultivariateSummary",
  definition = function(object) {
    txt_ca <- c("Correspondence Analysis", "rows", "columns")
    txt_pca <- c("Principal Components Analysis", "individuals", "variables")
    analysis <- switch (
      class(object),
      SummaryCA = txt_ca,
      SummaryPCA = txt_pca
    )

    ## Get data
    eig <- round(object@eigenvalues, digits = getOption("dimensio.digits"))
    res <- round(object@results, digits = getOption("dimensio.digits"))
    mar <- analysis[[object@margin + 1]]

    ## Split data
    is_sup <- object@supplement
    res_act <- res[!is_sup, ] # Active points
    res_sup <- res[is_sup, ] # Supplementary points

    ## Prepare data
    n_act <- nrow(res_act)
    n_sup <- nrow(res_sup)
    n_max <- getOption("dimensio.max.print")
    sum_act <- sum_sup <- NULL
    extra_act <- extra_sup <- NULL
    if (n_act > 0) {
      if (n_act > n_max) {
        res_act <- res_act[seq_len(n_max), ]
        extra_act <- sprintf("(%s more)", n_act)
      }
      sum_act <- c(sprintf("\nActive %s:", mar), utils::capture.output(res_act))
    }
    if (n_sup > 0) {
      if (n_sup > n_max) {
        res_sup <- res_sup[seq_len(n_max), ]
        extra_sup <- sprintf("(%s more)", n_sup)
      }
      is_na <- apply(X = res_sup, MARGIN = 2, FUN = anyNA)
      res_sup <- res_sup[, !is_na]
      sum_sup <- c(sprintf("\nSupplementary %s:", mar),
                   utils::capture.output(res_sup))
    }
    eigen <- c("\nEigenvalues:", utils::capture.output(eig))

    ## Print
    n_dashes <- getOption("width") - nchar(analysis[[1]]) - 4
    dashes <- paste0(rep("-", n_dashes), collapse = "")
    header <- sprintf("--- %s %s", analysis[[1]], dashes)
    cat(header, eigen, sum_act, extra_act, sum_sup, extra_sup, sep = "\n")
    invisible(NULL)
  }
)
