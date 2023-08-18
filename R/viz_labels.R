# PLOT LABELS
#' @include AllGenerics.R
NULL

viz_labels <- function(x, y, labels = seq_along(x),
                       xlim = c(-Inf, Inf), ylim = c(-Inf, Inf),
                       box = FALSE, segment = FALSE,
                       cex = graphics::par("cex"), col = graphics::par("fg"),
                       bg = graphics::par("bg"), ...) {
  ## Compute label positions
  coord <- get_labels(x = x, y = y, labels = labels, cex = cex)
  xt <- coord$x
  yt <- coord$y
  wt <- coord$width
  ht <- coord$height

  ## Plot lines
  if (isTRUE(segment)) {
    for (i in seq_along(x)) {
      if (x[i] != xt[i] || y[i] != yt[i]) {
        graphics::lines(
          x = c(x[i], xt[i] - 0.5 * wt[i]),
          y = c(y[i], yt[i]),
          col = "grey"
        )
      }
    }
  }

  ## Plot boxes
  if (isTRUE(box)) {
    mar_x <- graphics::strwidth ("m", cex = cex, ...) * 0.3
    mar_y <- graphics::strheight("x", cex = cex, ...) * 0.3

    .mapply(
      FUN = function(x, y, w, h, col, border) {
        roundrect(
          xleft = x - w - mar_x,
          ybottom = y - h - mar_y,
          xright = x + w + mar_x,
          ytop = y + h + mar_y,
          col = col,
          border = border
        )
      },
      dots = list(x = xt, y = yt, w = wt * 0.5, h = ht * 0.5,
                  col = bg, border = col),
      MoreArgs = NULL
    )
  } else {
    shadowtext(x = xt, y = yt, labels = labels, col = col, bg = bg, cex = cex)
  }

  ## Plot labels
  graphics::text(x = xt, y = yt, labels = labels, col = col, cex = cex)
}

# Adapted from vegan::ordipointlabel() by Jari Oksanen
get_labels <- function(x, y, labels, cex = 1) {
  xy <- cbind.data.frame(x, y)

  em <- graphics::strwidth("m", cex = min(cex))
  ex <- graphics::strheight("x", cex = min(cex))
  ltr <- em * ex

  width <- graphics::strwidth(labels, cex = cex) + em
  height <- graphics::strheight(labels, cex = cex) + ex
  box <- cbind.data.frame(width, height)

  makeoff <- function(pos, lab) {
    cbind(
      c(0, 1, 0, -1, 0.9, 0.9, -0.9, -0.9)[pos] * lab[, 1] / 2,
      c(1, 0, -1, 0, 0.8, -0.8, -0.8, 0.8)[pos] * lab[, 2] / 2
    )
  }
  overlap <- function(xy1, off1, xy2, off2) {
    pmax(0, pmin(xy1[, 1] + off1[, 1]/2, xy2[, 1] + off2[, 1]/2) -
           pmax(xy1[, 1] - off1[, 1]/2, xy2[, 1] - off2[, 1]/2)) *
      pmax(0, pmin(xy1[, 2] + off1[, 2]/2, xy2[, 2] + off2[, 2]/2) -
             pmax(xy1[, 2] - off1[, 2]/2, xy2[, 2] - off2[, 2]/2))
  }

  n <- nrow(xy)
  j <- as.vector(stats::as.dist(row(matrix(0, n, n))))
  k <- as.vector(stats::as.dist(col(matrix(0, n, n))))

  maylap <- overlap(xy[j, ], 2 * box[j, ], xy[k, ], 2 * box[k, ]) > 0
  j <- j[maylap]
  k <- k[maylap]
  jk <- sort(unique(c(j, k)))

  nit <- min(48 * length(jk), 10000)
  pos <- rep(1, n)

  ## Simulated annealing
  fn <- function(pos) {
    off <- makeoff(pos, box)
    val <- sum(overlap(xy[j, ] + off[j, ], box[j, ], xy[k, ] + off[k, ], box[k, ]))
    val <- val / ltr + sum(pos > 1) * 0.1 + sum(pos > 4) * 0.1
  }
  gr <- function(pos) {
    take <- sample(jk, 1)
    pos[take] <- sample((1:8)[-pos[take]], 1)
    pos
  }
  sol <- stats::optim(par = pos, fn = fn, gr = gr, method = "SANN",
                      control = list(maxit = nit))

  coord <- xy + makeoff(sol$par, box)
  coord$width <- width
  coord$height <- height
  coord
}

# Helpers ======================================================================
roundrect <- function(xleft, ybottom, xright, ytop,
                      rounding = 0.25, n = 200, ...) {

  XD <- YD <- min(c(xright - xleft, ytop - ybottom))
  xi <- rounding * XD
  yi <- rounding * YD

  ## Elliptic corners function
  elx <- function(from, to) xi * cos(seq(from, to, length.out = n / 4))
  ely <- function(from, to) yi * sin(seq(from, to, length.out = n / 4))

  ## x and y coordinates
  xc <- c(xright - xi + elx(0, pi / 2),
          xleft + xi + elx(pi / 2, pi),
          xleft + xi + elx(pi, 3 * pi / 2),
          xright - xi + elx(3 * pi / 2, 2 * pi))
  yc <- c(ytop - yi + ely(0, pi / 2),
          ytop - yi + ely(pi / 2, pi),
          ybottom + yi + ely(pi, 3 * pi / 2),
          ybottom + yi + ely(3 * pi / 2, 2 * pi))

  graphics::polygon(x = xc, y = yc, ...)
}

shadowtext <- function(x, y, labels,
                       cex = graphics::par("cex"),
                       col = graphics::par("fg"), bg = graphics::par("bg"),
                       theta= seq(0, 2 * pi, length.out = 50), r = 0.1, ... ) {

  xo <- r * graphics::strwidth("A", cex = cex, ...)
  yo <- r * graphics::strheight("A", cex = cex, ...)

  for (i in theta) {
    graphics::text(x + cos(i) * xo, y + sin(i) * yo, labels, col = bg, ...)
  }

  graphics::text(x, y, labels, col = col, ...)
}
