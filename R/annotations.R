# REPELLING LABELS

# Text =========================================================================
#' Non-Overlapping Text Labels
#'
#' Optimize the location of text labels to minimize overplotting text.
#' @param x,y A [`numeric`] vector giving the x and y coordinates of a set of
#'  points. If `y` is `NULL`, an attempt is made to interpret `x` in a suitable
#'  way (see [grDevices::xy.coords()]).
#' @param labels A [`character`] vector or [`expression`] specifying the text
#'  to be written.
#' @param type A [`character`] string specifying the shape of the field.
#'  It must be one of "`text`", "`shadow`" or "`box`". Any unambiguous substring
#'  can be given.
#' @param ... Further arguments to be passed to [graphics::text()],
#'  particularly, character expansion, `cex` and color, `col`.
#' @return
#'  `label()` is called it for its side-effects: it results in a graphic
#'  being displayed.
#' @seealso [graphics::text()]
#' @note
#'  This function is modeled after [car::pointLabel()] (originally from the
#'  \pkg{maptools} package).
#' @author N. Frerebeau
#' @family annotations
#' @keywords internal
#' @export
label <- function(x, y = NULL, labels = seq_along(x$x),
                  type = c("text", "shadow", "box"), ...) {
  ## Validation
  type <- match.arg(type, several.ok = FALSE)
  x <- grDevices::xy.coords(x = x, y = y)

  labels <- grDevices::as.graphicsAnnot(labels)
  if (length(labels) < length(x$x)) labels <- rep(labels, length(x$x))

  ## Compute label positions
  labs <- compute_labels(x = x$x, y = x$y, labels = labels)

  ## Draw labels
  switch(
    type,
    text = graphics::text(labs, labels = labels, ...),
    shadow = text_shadow(labs, labels = labels, ...),
    box = text_box(labs, labels = labels, ...)
  )

  invisible(labs)
}

# Adapted from car::pointLabel()
compute_labels <- function(x, y, labels, ..., iter = 50,
                           cex = graphics::par("cex"),
                           font = NULL, vfont = NULL) {
  ## Coordinates
  bound <- graphics::par("usr")
  ratio <- graphics::par("pin")[1] / graphics::par("pin")[2] # x/y ratio

  to_unity <- function(x, y) {
    list(x = (x - bound[1]) / (bound[2] - bound[1]) * ratio,
         y = (y - bound[3]) / (bound[4] - bound[3]) / ratio)
  }
  to_usr <- function(x, y) {
    list(x = bound[1] + x / ratio * (bound[2] - bound[1]),
         y = bound[3] + y * ratio * (bound[4] - bound[3]))
  }

  xy <- to_unity(x = x, y = y)
  x <- xy$x
  y <- xy$y
  n <- length(x)

  ## 8 positions: corners and side mid-points of the rectangle
  ## Position 7 (top right) is the most preferred
  width <- graphics::strwidth(labels, units = "figure", cex = cex,
                              font = font, vfont = vfont)
  height <- graphics::strheight(labels, units = "figure", cex = cex,
                                font = font, vfont = vfont)
  width <- (width + 0.02) * ratio
  height <- (height + 0.02) / ratio

  makeoff <- function(pos) {
    c(-1, -1, -1, 0, 0, 1, 1, 1)[pos] * (width / 2) +
      1i * c(-1, 0, 1, -1, 1, -1, 0, 1)[pos] * (height / 2)
  }

  ## Find intersection area of two rectangles
  overlap <- function(xy1, off1, xy2, off2) {
    w <- pmin(Re(xy1 + off1 / 2), Re(xy2 + off2 / 2)) -
      pmax(Re(xy1 - off1 / 2), Re(xy2 - off2 / 2))
    h <- pmin(Im(xy1 + off1 / 2), Im(xy2 + off2 / 2)) -
      pmax(Im(xy1 - off1 / 2), Im(xy2 - off2 / 2))
    w[w <= 0] <- 0
    h[h <= 0] <- 0
    w * h
  }

  objective <- function(gene) {
    offset <- makeoff(gene)

    if (!is.null(rectidx1)) {
      area <- sum(overlap(xy[rectidx1] + offset[rectidx1], rectv[rectidx1],
                          xy[rectidx2] + offset[rectidx2], rectv[rectidx2]))
    } else {
      area <- 0
    }

    ## Penalize labels which go outside the image area
    ## Count points outside of the image
    a <- Re(xy + offset - rectv / 2) < 0 | Re(xy + offset + rectv / 2) > ratio
    b <- Im(xy + offset - rectv / 2) < 0 | Im(xy + offset + rectv / 2) > 1 / ratio
    outside <- sum(a | b)
    res <- 1000 * area + outside
    res
  }

  # Make a list of label rectangles in their reference positions,
  # centered over the map feature; the real labels are displaced
  # from these positions so as not to overlap
  # Note that some labels can be bigger than others
  xy <- x + 1i * y
  rectv <- width + 1i * height

  rectidx1 <- rectidx2 <- array(0, (length(x)^2 - length(x)) / 2)
  k <- 0
  for (i in seq_along(x))
    for (j in seq_len(i - 1)) {
      k <- k + 1
      rectidx1[k] <- i
      rectidx2[k] <- j
    }
  maylap <- overlap(xy[rectidx1], 2 * rectv[rectidx1],
                    xy[rectidx2], 2 * rectv[rectidx2]) > 0
  rectidx1 <- rectidx1[maylap]
  rectidx2 <- rectidx2[maylap]

  ## Simulated annealing
  ## Initial state
  gene <- rep(8, n)
  score <- objective(gene)
  ## Initial "best" solution
  bestgene <- gene
  bestscore <- score
  iter <- seq_len(iter)
  temp <- 2.5
  for (i in iter) {
    k <- 1 # Energy evaluation count
    for (j in iter) {
      newgene <- gene
      newgene[sample(n, 1)] <- sample(8, 1)
      newscore <- objective(newgene)
      if (newscore <= score || stats::runif(1) < exp((score - newscore) / temp)) {
        ## keep the new set if it has the same or better score or
        ## if it's worse randomly based on the annealing criteria
        k <- k + 1
        score <- newscore
        gene <- newgene
      }
      if (score <= bestscore) {
        bestscore <- score
        bestgene <- gene
      }
      if (bestscore == 0 || k == 10) break
    }
    if (bestscore == 0) break
    temp <- 0.9 * temp
  }

  nx <- Re(xy + makeoff(bestgene))
  ny <- Im(xy + makeoff(bestgene))

  xy <- to_usr(x = nx, y = ny)
  xy$labels <- labels
  xy
}

#' Shadow Text
#'
#' @param x,y A [`numeric`] vector. If `y` is `NULL`, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xy.coords()]).
#' @param labels A [`character`] vector specifying the text to be written.
#' @param width Thickness of the shadow, as a fraction of the plotting size.
#' @param theta Angles for plotting the background.
#' @param cex A [`numeric`] character expansion factor.
#' @param col The color to be used for the text.
#' @param bg The color to be used for the shadow.
#' @param font,vfont The font to be used (see [graphics::text()]).
#' @param ... Further parameters to be passed to [graphics::text()].
#' @return
#'  `text_shadow()` is called it for its side-effects: it results in a graphic
#'  being displayed.
#' @author N. Frerebeau
#' @family geometries
#' @keywords internal
#' @noRd
text_shadow <- function(x, y = NULL, labels = seq_along(x$x),
                        width = 1/10, theta = seq(0, 2 * pi, length.out = 50),
                        cex = graphics::par("cex"), col = graphics::par("fg"),
                        bg = graphics::par("bg"), font = NULL, vfont = NULL, ...) {

  x <- grDevices::xy.coords(x = x, y = y)

  xo <- width * graphics::strwidth("M", units = "user", cex = cex, font = font, vfont = vfont)
  yo <- width * graphics::strheight("X", units = "user", cex = cex, font = font, vfont = vfont)

  for (i in theta) {
    graphics::text(x = x$x + cos(i) * xo, y = x$y + sin(i) * yo, labels = labels,
                   col = bg, cex = cex, font = font, vfont = vfont, ...)
  }

  graphics::text(x = x$x, y = x$y, labels = labels, col = col, cex = cex,
                 font = font, vfont = vfont, ...)

  invisible(NULL)
}

#' Text with Halo Underneath
#'
#' @param x,y A [`numeric`] vector. If `y` is `NULL`, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xy.coords()]).
#' @param labels A [`character`] vector specifying the text to be written.
#' @param padding A length-one [`numeric`] vector giving the amount of padding
#'  around label.
#' @param rounding A length-one [`numeric`] vector giving the rounding of the
#'  angles (see [rounded()]).
#' @param vertices A length-on [`integer`] vector specifying the number of
#'  vertices to draw (see [rounded()]).
#' @param cex A numeric character expansion factor.
#' @param col The color to be used for the text.
#' @param bg The color to be used for the background.
#' @param font,vfont The font to be used (see [graphics::text()]).
#' @param ... Further parameters to be passed to [graphics::text()] (see details).
#' @details
#'  Specifying `pos` and `offset` will currently change the position of the
#'  text, but not of the field.
#' @return
#'  `text_box()` is called it for its side-effects: it results in a graphic
#'  being displayed.
#' @author N. Frerebeau
#' @family geometries
#' @keywords internal
#' @noRd
text_box <- function(x, y = NULL, labels = seq_along(x$x), padding = 1/3,
                     rounding = 0.2, vertices = 100,
                     cex = graphics::par("cex"), col = graphics::par("fg"),
                     bg = graphics::par("bg"), font = NULL, vfont = NULL, ...) {

  x <- grDevices::xy.coords(x = x, y = y)
  srt <- list(...)$srt %||% graphics::par("srt")

  em <- graphics::strwidth("M", units = "user", cex = cex, font = font, vfont = vfont)
  ex <- graphics::strheight("X", units = "user", cex = cex, font = font, vfont = vfont)

  xo <- padding * em
  yo <- padding * ex

  width <- graphics::strwidth(labels, units = "user", cex = cex, font = font, vfont = vfont)
  height <- graphics::strheight(labels, units = "user", cex = cex, font = font, vfont = vfont)

  .mapply(
    FUN = function(x, y, w, h, r, n, col, border, rotate) {
      rounded(
        x0 = x - w - xo,
        y0 = y - h - yo,
        x1 = x + w + xo,
        y1 = y + h + yo,
        r = r,
        n = n,
        col = col,
        border = border,
        rotate = rotate,
        aspect = TRUE
      )
    },
    dots = list(x = x$x, y = x$y, w = width * 0.5, h = height * 0.5,
                col = bg, border = col, rotate = srt),
    MoreArgs = list(r = rounding, n = vertices)
  )
  graphics::text(x = x$x, y = x$y, labels = labels, col = col, cex = cex,
                 font = font, vfont = vfont, ...)

  invisible(NULL)
}

# Shapes =======================================================================
#' Circle
#'
#' Draws a circle.
#' @param x,y A length-one [`numeric`] vector giving the coordinates of the
#'  center of the circle.
#' @param radius A length-one [`numeric`] vector giving the radius of the
#'  circle.
#' @param n A length-on [`integer`] vector specifying the number of vertices to
#'  draw the circle.
#' @param ... Further parameters to be passed to [graphics::polygon()].
#' @return
#'  `circle()` is called it for its side-effects: it results in a graphic
#'  being displayed.
#' @author N. Frerebeau
#' @family shapes
#' @keywords internal
#' @noRd
circle <- function(x, y, radius, ..., n = 100) {
  angle.inc <- 2 * pi / n
  angles <- seq(0, 2 * pi - angle.inc, by = angle.inc)

  xv <- cos(angles) * radius + x
  yv <- sin(angles) * radius + y
  graphics::polygon(xv, yv, ...)
}

#' Rounded Rectangle
#'
#' Draws a rectangular box with rounded left and right edges.
#' @param x0,y0 A length-one [`numeric`] vector giving the coordinates of the
#'  bottom left angle.
#' @param x1,y1 A length-one [`numeric`] vector giving the coordinates of the
#'  top right angle.
#' @param r A length-one [`numeric`] vector giving the rounding of the edges.
#' @param n A length-on [`integer`] vector specifying the number of vertices to
#'  draw.
#' @param rotate A [`numeric`] vector giving the angle of rotation, in degrees.
#' @param aspect A [`logical`] scalar: should the aspect ratio be kept during
#'  rotation?
#' @param ... Further parameters to be passed to [graphics::polygon()].
#' @return
#'  `rounded()` is called it for its side-effects: it results in a graphic
#'  being displayed.
#' @author N. Frerebeau
#' @family shapes
#' @keywords internal
#' @noRd
rounded <- function(x0, y0, x1, y1, ..., r = 0.2, n = 100,
                    rotate = NULL, aspect = FALSE) {

  XD <- YD <- min(c(x1 - x0, y1 - y0))
  xi <- r * XD
  yi <- r * YD

  ## Elliptic corners function
  elx <- function(from, to) xi * cos(seq(from, to, length.out = n / 4))
  ely <- function(from, to) yi * sin(seq(from, to, length.out = n / 4))

  ## Coordinates
  x <- c(x1 - xi + elx(0, pi / 2),
         x0 + xi + elx(pi / 2, pi),
         x0 + xi + elx(pi, 3 * pi / 2),
         x1 - xi + elx(3 * pi / 2, 2 * pi))
  y <- c(y1 - yi + ely(0, pi / 2),
         y1 - yi + ely(pi / 2, pi),
         y0 + yi + ely(pi, 3 * pi / 2),
         y0 + yi + ely(3 * pi / 2, 2 * pi))

  ## Rotate
  xy <- list(x = x, y = y)
  if (!is.null(rotate)) xy <- rotate(xy$x, xy$y, angle = rotate, aspect = aspect)

  graphics::polygon(x = xy$x, y = xy$y, ...)
}

# Palette ======================================================================
## Color -----------------------------------------------------------------------
#' Color Mapping
#'
#' Maps values to a colors.
#' @param colors A vector of colors that values will be mapped to.
#' @param domain A [`numeric`] range or a vector of categorical data specifying
#'  the possible values that can be mapped.
#' @param midpoint A length-one [`numeric`] vector specifying the mid-point of
#'  input range.
#' @param ordered A [`logical`] scalar: should the levels be treated as already
#'  in the correct order?
#' @param missing The color to return for `NA` values.
#' @param ... Further parameters to be passed to internal methods.
#' @details
#'  A wrapper around `palette_color_continuous()` and
#'  `palette_color_discrete()`.
#' @return
#'  A palette [`function`] that when called with a single argument returns
#'  a [`character`] vector of colors.
#' @family palettes
#' @keywords internal
#' @export
palette_color <- function(colors = NULL, domain = NULL, midpoint = NULL,
                          ordered = FALSE, missing = "#DDDDDD", ...) {
  function(x) {
    if (is.double(x)) {
      fun <- palette_color_continuous(colors = colors, domain = domain,
                                      midpoint = midpoint, missing = missing)
    } else {
      fun <- palette_color_discrete(colors = colors, domain = domain,
                                    ordered = ordered, missing = missing)
    }
    fun(x)
  }
}

#' Color Mapping (continuous)
#'
#' Maps continuous values to an interpolated colors gradient.
#' @param colors A vector of colors that values will be mapped to. If `NULL`
#'  (the default), uses *YlOrRd* (see [grDevices::hcl.colors()]).
#' @param domain A [`numeric`] range specifying the possible values that can be
#'  mapped.
#' @param midpoint A length-one [`numeric`] vector specifying the mid-point of
#'  input range.
#' @param missing The color to return for `NA` values.
#' @return
#'  A palette [`function`] that when called with a single argument
#'  (a [`numeric`] vector of continuous values) returns a [`character`] vector
#'  of colors.
#' @family palettes
#' @keywords internal
#' @export
palette_color_continuous <- function(colors = NULL, domain = NULL,
                                     midpoint = NULL, missing = "#DDDDDD") {

  force(colors)
  force(domain)
  force(midpoint)
  force(missing)

  function(x, ...) {
    need_continuous(x)

    rng <- if (!is.null(domain)) range(domain, finite = TRUE) else range(x, finite = TRUE)
    if (!is.null(midpoint) && is.numeric(midpoint)) {
      x <- scale_midpoint(x, to = c(0, 1), from = rng, midpoint = midpoint)
    } else {
      x <- scale_range(x, to = c(0, 1), from = rng)
    }

    out <- x < 0 | x > 1
    if (any(out, na.rm = TRUE)) {
      x[out] <- NA
      warning("Some values were outside the color scale.", call. = FALSE)
    }

    OK <- !is.na(x)
    if (is.null(colors)) {
      colors <- grDevices::hcl.colors(12, "YlOrRd", rev = TRUE)
    }
    colors <- grDevices::colorRamp(colors)(x[OK], ...)

    col <- rep(missing, length(x))
    col[OK] <- grDevices::rgb(colors, maxColorValue = 255)
    col
  }
}

#' Color Mapping (discrete)
#'
#' Maps categorical values to colors.
#' @param colors A vector of colors that values will be mapped to. If `NULL`
#'  (the default), uses *viridis* (see [grDevices::hcl.colors()]).
#' @param domain A vector of categorical data specifying the possible values
#'  that can be mapped.
#' @param ordered A [`logical`] scalar: should the levels be treated as already
#'  in the correct order?
#' @param missing The color to return for `NA` values.
#' @return
#'  A palette [`function`] that when called with a single argument
#'  (a vector of categorical values) returns a [`character`] vector of colors.
#' @family palettes
#' @keywords internal
#' @export
palette_color_discrete <- function(colors = NULL, domain = NULL,
                                   ordered = FALSE, missing = "#DDDDDD") {

  force(colors)
  force(domain)
  force(ordered)
  force(missing)

  ## If named colors, override user settings
  if (!is.null(names(colors))) {
    domain <- names(colors)
    ordered <- TRUE
  }

  function(x, ...) {
    need_discrete(x)

    domain <- make_levels(x, levels = domain, ordered = ordered)
    n <- length(domain)
    x <- match(as.character(x), domain)

    OK <- !is.na(x)
    if (is.null(colors)) {
      colors <- grDevices::hcl.colors(n, "viridis")
    }

    if (length(colors) < n) {
      msg <- "Only %d colors were specified (%d are required)."
      warning(sprintf(msg, length(colors), n), call. = FALSE)
    }
    col <- colors[x]
    col[!OK] <- missing
    col
  }
}

## Symbol ----------------------------------------------------------------------
#' Symbol Mapping
#'
#' @param symbols,types A vector of symbols or line types.
#' @param domain A vector of categorical data specifying the possible values
#'  that can be mapped.
#' @param ordered A [`logical`] scalar: should the levels be treated as already
#'  in the correct order?
#' @param ... Currently not used.
#' @return
#'  A palette [`function`] that when called with a single argument
#'  (a [`character`] vector of categorical values) returns a vector of symbols.
#' @family palettes
#' @keywords internal
#' @export
palette_shape <- function(symbols = NULL, domain = NULL, ordered = FALSE, ...) {

  force(symbols)
  force(domain)
  force(ordered)

  ## If named symbol, override user settings
  if (!is.null(names(symbols))) {
    domain <- names(symbols)
    ordered <- TRUE
  }

  function(x) {
    need_discrete(x)

    domain <- make_levels(x, levels = domain, ordered = ordered)
    x <- match(as.character(x), domain)

    if (is.null(symbols)) {
      n <- length(domain)
      if (n > 6) {
        warning("Consider specifying symbols manually: ",
                "more than 6 becomes difficult to discriminate.", call. = FALSE)
      }
      symbols <- c(16, 17, 15, 3, 7, 8)[seq_len(n)]
    }
    symbols[x]
  }
}

#' @export
#' @rdname palette_shape
palette_line <- function(types = NULL, domain = NULL, ordered = FALSE, ...) {
  if (is.null(types)) {
    types <- c("solid", "22", "42", "44", "13", "1343", "73", "2262",
               "12223242", "F282", "F4448444", "224282F2", "F1")
  }
  palette_shape(symbols = types, domain = domain, ordered = ordered, ...)
}

## Size ------------------------------------------------------------------------
#' Symbol Size Mapping
#'
#' @param range A length-two [`numeric`] vector giving range of possible sizes
#' (greater than 0).
#' @param ... Currently not used.
#' @return
#'  A palette [`function`] that when called with a single argument
#'  (a [`numeric`] vector of continuous values) returns a [`numeric`] vector
#'  giving the amount by which plotting text and symbols should be magnified
#'  relative to the default.
#' @family palettes
#' @keywords internal
#' @export
palette_size_range <- function(range = c(1, 6), ...) {

  force(range)

  function(x) {
    need_continuous(x)
    scale_range(sqrt(x), to = range(range, na.rm = TRUE), from = c(0, 1))
  }
}

# Scales =======================================================================
#' Rescale Continuous Vector (minimum, maximum)
#'
#' Rescales continuous vector to have specified minimum and maximum.
#' @param x A [`numeric`] vector.
#' @param to A length-two [`numeric`] vector specifying the output range.
#' @param from A length-two [`numeric`] vector specifying the input range.
#' @return A [`numeric`] vector.
#' @family scales
#' @keywords internal
#' @export
scale_range <- function(x, to = c(0, 1), from = range(x, finite = TRUE)) {
  if (is_zero(to) || is_zero(from)) return(ifelse(is.na(x), NA, mean(to)))
  (x - from[1L]) / diff(from) * diff(to) + to[1L]
}

#' Rescale Continuous Vector (minimum, midpoint, maximum)
#'
#' Rescales continuous vector to have specified minimum, midpoint and maximum.
#' @param x A [`numeric`] vector.
#' @param to A length-two [`numeric`] vector specifying the output range.
#' @param from A length-two [`numeric`] vector specifying the input range.
#' @param midpoint A length-one [`numeric`] vector specifying the mid-point of
#'  input range.
#' @return A [`numeric`] vector.
#' @family scales
#' @keywords internal
#' @export
scale_midpoint <- function(x, to = c(0, 1), from = range(x, finite = TRUE), midpoint = 0) {
  if (is_zero(to) || is_zero(from)) return(ifelse(is.na(x), NA, mean(to)))
  extent <- 2 * max(abs(from - midpoint))
  (x - midpoint) / extent * diff(to) + mean(to)
}

is_zero <- function(x, tolerance = sqrt(.Machine$double.eps)) {
  diff(range(x)) <= tolerance
}

# Helpers ======================================================================
need_continuous <- function(x) {
  if (!is.numeric(x)) {
    stop("Discrete value supplied to continuous scale.", call. = FALSE)
  }
  invisible(x)
}

need_discrete <- function(x) {
  if (is.double(x)) {
    warning("Continuous value supplied to discrete scale.", call. = FALSE)
  }
  invisible(x)
}

make_levels <- function(x, levels = NULL, ordered = FALSE) {
  if (!is.null(levels)) return(make_levels(x = levels, ordered = ordered))

  if (is.null(x)) levels <- NULL
  else if (is.factor(x)) levels <- levels(x)
  else if (ordered) levels <- unique(x)
  else levels <- sort(unique(x))
  levels
}

#' Rotation in Euclidean Space
#'
#' Rotates points in the `xy` plane counterclockwise.
#' @param x,y A [`numeric`] vector. If `y` is `NULL`, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xy.coords()]).
#' @param angle A [`numeric`] vector giving the angle of rotation, in degrees.
#' @param center A length-two [`numeric`] vector giving the coordinates of the
#'  rotation point. If `NULL`, defaults to centroid.
#' @param aspect A [`logical`] scalar: should aspect ratio be kept?
#' @return
#'  Returns a [`list`] with two components `x` and `y`.
#' @example inst/examples/ex-rotate.R
#' @keywords internal
#' @noRd
rotate <- function(x, y = NULL, angle = 0, center = NULL, aspect = FALSE) {

  xy <- grDevices::xy.coords(x = x, y = y)
  if (is.null(center)) center <- c(mean(xy$x), mean(xy$y))

  theta <- angle / 180 * pi
  cos_theta  <- cos(theta)
  sin_theta  <- sin(theta)

  dx <- xy$x - center[[1L]]
  dy <- xy$y - center[[2L]]

  ex <- center[[1L]] + cos_theta * dx - sin_theta * dy
  ey <- center[[2L]] + sin_theta * dx + cos_theta * dy

  if (aspect) {
    usr <- graphics::par("usr")
    pin  <- graphics::par("pin")
    sy   <- usr[[4L]] - usr[[3L]]
    sx   <- usr[[2L]] - usr[[1L]]
    ey   <- center[[2L]] + (ey - center[[2L]]) * sy / sx * pin[[1L]] / pin[[2L]]
  }

  list(x = ex, y = ey)
}
