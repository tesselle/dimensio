xcont <- seq(1, 2, by = 0.2)
xint <- c(1L, 2L, 3L, 4L, 5L)
xcha <- c("A", "B", "C", "D", "E")

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
