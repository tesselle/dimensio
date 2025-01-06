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
      paste0(tr_("Correspondence Analysis (CA)"), ":"),
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
      paste0(tr_("Multiple Correspondence Analysis (MCA)"), ":"),
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
      paste0(tr_("Principal Components Analysis (PCA)"), ":"),
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
  signature = "PCOA",
  definition = function(object) {
    cat(
      paste0(tr_("Principal Coordinate Analysis (PCoA)"), ":"),
      sprintf(tr_("* Method: %s."), object@method),
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
      active <- c(tr_("Active rows"), tr_("Active columns"))
      suppl <- c(tr_("Supplementary rows"), tr_("Supplementary columns"))
      title <- tr_("Correspondence Analysis (CA)")
    }
    if (methods::is(object, "SummaryPCA")) {
      active <- c(tr_("Active individuals"), tr_("Active variables"))
      suppl <- c(tr_("Supplementary individuals"), tr_("Supplementary variables"))
      title <- tr_("Principal Components Analysis (PCA)")
    }

    ## Get data
    eig <- round(object@eigenvalues, digits = n_dig)
    res <- round(object@results, digits = n_dig)

    ## Prepare data
    is_sup <- object@supplement
    eigen <- c(paste0("\n## ", tr_("Eigenvalues")), "",
               utils::capture.output(format_table(eig)))

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
      sum_sup <- c(paste0("\n## ", suppl[[object@margin]]), "",
                   utils::capture.output(format_table(res_sup)))
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
      sum_act <- c(paste0("\n## ", active[[object@margin]]), "",
                   utils::capture.output(format_table(res_act)))
    }

    ## Print
    header <- paste0("# ", title)
    cat(header, eigen, sum_act, extra_act, sum_sup, extra_sup, sep = "\n")
    invisible(object)
  }
)


format_table <- function(x) {
  val <- rbind(colnames(x), format_head(colnames(x), left = FALSE), x)
  val <- apply(X = val, MARGIN = 2, FUN = format_col, left = FALSE)
  row_names <- c("", format_head(rownames(x))[which.max(nchar(rownames(x)))], rownames(x))
  val <- cbind(format_col(row_names), val)
  val <- apply(X = val, MARGIN = 1, FUN = format_row)
  cat(val, sep = "\n")
}

vec_rep <- function(x, times) {
  force(x)
  vapply(
    X = times,
    FUN = function(i) paste0(rep(x, i), collapse = ""),
    FUN.VALUE = character(1)
  )
}
format_head <- function(x, left = TRUE) {
  n <- nchar(x) - 1
  d <- vec_rep("-", n)
  if (left) paste0(":", d) else paste0(d, ":")
}
format_col <- function(x, left = TRUE) {
  n <- max(nchar(x))
  d <- vapply(
    X = n - nchar(x),
    FUN = function(i) ifelse(i == 0, "", paste0(rep(" ", i), collapse = "")),
    FUN.VALUE = character(1)
  )
  if (left) paste0(x, d) else paste0(d, x)
}
format_row <- function(x) {
  paste0("| ", paste0(x, collapse = " | "), " |")
}
