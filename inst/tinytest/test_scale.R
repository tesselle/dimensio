xcont <- seq(1, 2, by = 0.2)
xint <- c(1L, 2L, 3L, 4L, 5L)
xcha <- c("A", "B", "C", "D", "E")

# Rescale continuous vector ====================================================
expect_identical(dimensio:::scale_range(5:10), c(0, 0.2, 0.4, 0.6, 0.8, 1))

# Color scale ==================================================================
expect_identical(dimensio:::scale_color(x = NULL, col = NULL), graphics::par("col"))
expect_identical(dimensio:::scale_color(x = NULL, col = "red"), "red")

## Continuous scale ------------------------------------------------------------
expect_length(dimensio:::scale_color(xcont), 6L)
expect_identical(
  dimensio:::scale_color(xcont, col = "#000000"),
  c("#000000", "#000000", "#000000", "#000000", "#000000", "#000000")
)

## Default palette
expect_identical(
  dimensio:::scale_color(xcont),
  c("#FFFFC8", "#FAE092", "#F6B024", "#EF6F00", "#C92700", "#7D0025")
)
## Custom palette
expect_identical(
  dimensio:::scale_color(xcont, col = grDevices::hcl.colors(12, "BluGrn")),
  c("#14505C", "#247172", "#3D9287", "#5EB395", "#90CFA4", "#C7E5BE")
)
## Alpha transparency
expect_identical(
  dimensio:::scale_color(xcont, col = grDevices::hcl.colors(12, "BluGrn"), alpha = TRUE),
  c("#14505CFF", "#247172FF", "#3D9287FF", "#5EB395FF", "#90CFA4FF", "#C7E5BEFF")
)

## Discrete scale --------------------------------------------------------------
expect_length(dimensio:::scale_color(xint), 5L)
expect_identical(dimensio:::scale_color(xint), dimensio:::scale_color(xcha))

## Default palette
expect_identical(
  dimensio:::scale_color(xint),
  c("#4B0055", "#00588B", "#009B95", "#53CC67", "#FDE333")
)
## Custom palette
expect_equivalent(
  dimensio:::scale_color(xint, col = grDevices::palette.colors(9, "Okabe-Ito")),
  c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442")
)

## Recycle
expect_message(dimensio:::scale_color(xint, col = c("black", "red")))
expect_identical(
  suppressMessages(dimensio:::scale_color(xint, col = c("black", "red"))),
  c("black", "red", "black", "red", "black")
)
expect_identical(
  dimensio:::scale_color(xint, col = "black"),
  c("black", "black", "black", "black", "black")
)

# Shape scale ==================================================================
expect_identical(
  dimensio:::scale_symbole(x = NULL, symb = NULL, what = "pch"),
  graphics::par("pch")
)
expect_identical(
  dimensio:::scale_symbole(x = NULL, symb = NULL, what = "lty"),
  graphics::par("lty")
)
expect_identical(dimensio:::scale_symbole(x = NULL, symb = 16), 16)

expect_identical(dimensio:::scale_symbole(xint), c(1L, 1L, 1L, 1L, 1L))
expect_identical(dimensio:::scale_symbole(xint), dimensio:::scale_symbole(xcha))

expect_identical(dimensio:::scale_symbole(xint, symb = 15:19), 15:19)

expect_message(dimensio:::scale_symbole(xint, symb = c(15, 16)))
expect_identical(
  suppressMessages(dimensio:::scale_symbole(xint, symb = c(15, 16))),
  c(15, 16, 15, 16, 15)
)
expect_identical(
  dimensio:::scale_symbole(xint, symb = 16),
  c(16, 16, 16, 16, 16)
)

expect_warning(
  dimensio:::scale_symbole(xcont, symb = 15:19),
  "Continuous value supplied to discrete scale."
)

# Size scale ===================================================================
expect_identical(
  dimensio:::scale_size(x = NULL, size = NULL, what = "cex"),
  graphics::par("cex")
)
expect_identical(
  dimensio:::scale_size(x = NULL, size = NULL, what = "lwd"),
  graphics::par("lwd")
)
expect_identical(dimensio:::scale_size(x = NULL, size = 2), 2)

expect_equal(dimensio:::scale_size(xcont, size = c(1, 6)), c(1, 2, 3, 4, 5, 6))

expect_warning(
  dimensio:::scale_size(xint, size = c(1, 6)),
  "Discrete value supplied to continuous scale."
)
