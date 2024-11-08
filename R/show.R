# SHOW
#' @include AllGenerics.R
NULL

setMethod(
  f = "show",
  signature = "CA",
  definition = function(object) {
    row_sup <- object@rows@supplement
    col_sup <- object@columns@supplement

    sup_txt <- tr_(" (+ %d supplementary)")
    row_txt <- if (any(row_sup)) sprintf(sup_txt, sum(row_sup)) else ""
    col_txt <- if (any(col_sup)) sprintf(sup_txt, sum(col_sup)) else ""

    cat(
      format_header(tr_("Correspondence Analysis (CA)")),
      sprintf(tr_("* Row variable: %d categories%s."), sum(!row_sup), row_txt),
      sprintf(tr_("* Column variable: %d categories%s."), sum(!col_sup), col_txt),
      sep = "\n"
    )
    invisible(object)
  }
)
setMethod(
  f = "show",
  signature = "MCA",
  definition = function(object) {
    row_sup <- object@rows@supplement
    col_sup <- object@columns@supplement

    sup_txt <- tr_(" (+ %d supplementary)")
    row_txt <- if (any(row_sup)) sprintf(sup_txt, sum(row_sup)) else ""
    col_txt <- if (any(col_sup)) sprintf(sup_txt, sum(col_sup)) else ""

    cat(
      format_header(tr_("Multiple Correspondence Analysis (MCA)")),
      sprintf(tr_("* Row variable: %d categories%s."), sum(!row_sup), row_txt),
      sprintf(tr_("* Column variable: %d categories%s."), sum(!col_sup), col_txt),
      sep = "\n"
    )
    invisible(object)
  }
)
setMethod(
  f = "show",
  signature = "PCA",
  definition = function(object) {
    row_sup <- object@rows@supplement
    col_sup <- object@columns@supplement

    sup_txt <- tr_(" (+ %d supplementary)")
    row_txt <- if (any(row_sup)) sprintf(sup_txt, sum(row_sup)) else ""
    col_txt <- if (any(col_sup)) sprintf(sup_txt, sum(col_sup)) else ""

    if (is_centered(object)) {
      var_center <- tr_("* Variables were shifted to be zero centered.")
    } else {
      var_center <- tr_("* Variables were NOT shifted to be zero centered.")
    }
    if (is_scaled(object)) {
      var_scale <- tr_("* Variables were scaled to unit variance.")
    } else {
      var_scale <- tr_("* Variables were NOT scaled to unit variance.")
    }

    cat(
      format_header(tr_("Principal Components Analysis (PCA)")),
      sprintf(tr_("* %d individuals%s."), sum(!row_sup), row_txt),
      sprintf(tr_("* %d variables%s."), sum(!col_sup), col_txt),
      var_center,
      var_scale,
      sep = "\n"
    )
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

    if (methods::is(object, "SummaryCA")) {
      analysis <- c("rows", "columns")
      title <- tr_("Correspondence Analysis (CA)")
    }
    if (methods::is(object, "SummaryPCA")) {
      analysis <- c("individuals", "variables")
      title <- tr_("Principal Components Analysis (PCA)")
    }

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
        extra_sup <- sprintf("(%s more)", n_sup - n_max)
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
        extra_act <- sprintf("(%s more)", n_act - n_max)
      }
      sum_act <- c(sprintf("\nActive %s:", mar), utils::capture.output(res_act))
    }

    ## Print
    header <- format_header(title)
    cat(header, eigen, sum_act, extra_act, sum_sup, extra_sup, sep = "\n")
    invisible(object)
  }
)

format_header <- function(title, width = getOption("width")) {
  n_dashes <- width - nchar(title) - 4
  dashes <- paste0(rep("-", n_dashes), collapse = "")
  sprintf("--- %s %s", title, dashes)
}
