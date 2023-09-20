# PLOT LABELS
#' @include AllGenerics.R
NULL

viz_labels <- function(x, y, ..., labels = seq_along(x),
                       box = FALSE, segment = FALSE,
                       cex = graphics::par("cex"),
                       col = graphics::par("fg"),
                       bg = graphics::par("bg"),
                       font = graphics::par("font")) {
  ## Compute label positions
  coord <- compute_labels(x = x, y = y, labels = labels, cex = cex)
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
    boxtext(x = xt, y = yt, width = wt, height = ht, labels = labels,
            col = col, bg = bg, cex = cex, font = font)
  } else {
    shadowtext(x = xt, y = yt, labels = labels, col = col, bg = bg,
               cex = cex, font = font)
  }
}

# Helpers ======================================================================
# Adapted from vegan::ordipointlabel() by Jari Oksanen
compute_labels <- function(x, y, labels, cex = graphics::par("cex"),
                           font = graphics::par("font")) {
  xy <- cbind.data.frame(x, y)

  em <- graphics::strwidth("m", cex = min(cex))
  ex <- graphics::strheight("x", cex = min(cex))
  ltr <- em * ex

  width <- graphics::strwidth(labels, cex = cex, font = font) + em
  height <- graphics::strheight(labels, cex = cex, font = font) + ex
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

boxtext <- function(x, y, width, height, labels, ..., r = 0.1,
                    cex = graphics::par("cex"),
                    col = graphics::par("fg"),
                    bg = graphics::par("bg"),
                    font = graphics::par("font"),
                    xpd = TRUE) {

  xo <- r * graphics::strwidth("M", cex = cex, font = font, ...)
  yo <- r * graphics::strheight("X", cex = cex, font = font, ...)

  .mapply(
    FUN = function(x, y, w, h, col, border, xpd) {
      roundrect(
        xleft = x - w - xo,
        ybottom = y - h - yo,
        xright = x + w + xo,
        ytop = y + h + yo,
        col = col,
        border = border,
        xpd = xpd
      )
    },
    dots = list(x = x, y = y, w = width * 0.5, h = height * 0.5,
                col = bg, border = col),
    MoreArgs = list(xpd = xpd)
  )
  graphics::text(x = x, y = y, labels = labels, col = col,
                 cex = cex, font = font, xpd = xpd)
}

shadowtext <- function(x, y, labels, ...,
                       theta = seq(0, 2 * pi, length.out = 50),
                       r = 0.1,
                       cex = graphics::par("cex"),
                       col = graphics::par("fg"),
                       bg = graphics::par("bg"),
                       font = graphics::par("font"),
                       xpd = TRUE) {

  xo <- r * graphics::strwidth("M", cex = cex, font = font, ...)
  yo <- r * graphics::strheight("X", cex = cex, font = font, ...)

  for (i in theta) {
    graphics::text(x + cos(i) * xo, y + sin(i) * yo, labels,
                   col = bg, cex = cex, font = font, xpd = xpd)
  }

  graphics::text(x, y, labels, col = col, cex = cex, font = font, xpd = xpd)
}

roundrect <- function(xleft, ybottom, xright, ytop, ...,
                      rounding = 0.25, n = 200) {

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
