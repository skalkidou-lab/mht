# Generates man/figures/logo.png. Run from the package root:
#   Rscript dev/logo.R
#
# R packages, none of which is an mht dependency (this script is in dev/ and is
# .Rbuildignore'd, so it never ships):
#   pak::pak(c("hexSticker", "ggplot2", "data.table"))
#
# hexSticker pulls in ggimage -> magick, which links against the system
# ImageMagick C++ library. Install that first or `library(hexSticker)` fails at
# dyn.load:
#   sudo apt install -y libmagick++-dev      # Debian/Ubuntu
#   brew install imagemagick@6               # macOS
# The exact missing-object message names whichever libMagick++ SONAME your
# `magick` binary was built against; it is not a fixed string.
#
# The motif is two dispensed products: a two-tone capsule and a scored tablet.
# The package classifies dispensed product names into MHT groups, so the pill
# itself is the subject. Two large shapes rather than several small ones,
# because this renders at 64 px in a favicon.
#
# The shapes are POLYGONS IN DATA SPACE, not thick segments and points. A
# geom_segment linewidth and a geom_point size are both absolute (mm), so they
# do not scale with the panel: inside hexSticker's small subplot a capsule drawn
# that way overflows the coordinate limits and gets clipped into a flat-topped
# blob. Polygons scale with coord_fixed and stay the shape you specified.

library(hexSticker)
library(ggplot2)
library(data.table)

# sticker() prints the ggplot, which opens the default device. Under Rscript
# that device is pdf(), so the run drops an Rplots.pdf in the package root, and
# R CMD build ships it -- "Non-standard file/directory found at top level". Open
# a null device first so there is nothing to leave behind.
grDevices::pdf(NULL)

CFG <- list(
  bg = "#251B31",
  border = "#C58FC8",
  pill_lo = "#EBD3EC", # capsule's pale half, and the tablet body
  pill_hi = "#B173B8", # capsule's coloured half
  score = "#9C6BA4", # the tablet's score line
  text = "#F3E9F4",
  out = "man/figures/logo.png"
)

rot <- function(x, y, deg) {
  a <- deg * pi / 180
  list(x = x * cos(a) - y * sin(a), y = x * sin(a) + y * cos(a))
}

circle <- function(cx, cy, r, n = 256) {
  t <- seq(0, 2 * pi, length.out = n)
  data.table(x = cx + r * cos(t), y = cy + r * sin(t))
}

# Half a stadium: a flat edge on the seam side, a semicircular cap on the other.
# Two of these butted together make a capsule with a straight seam, which is how
# a real two-piece capsule reads.
half_capsule <- function(
  cx,
  cy,
  len,
  r,
  deg,
  side = c("right", "left"),
  n = 128
) {
  side <- match.arg(side)
  s <- if (side == "right") 1 else -1
  t <- seq(-pi / 2, pi / 2, length.out = n)
  px <- c(0, s * (len / 2 + r * cos(t)))
  py <- c(-r, r * sin(t))
  px <- c(px, 0)
  py <- c(py, r)
  p <- rot(px, py, deg)
  data.table(x = cx + p$x, y = cy + p$y)
}

# A whole stadium as ONE polygon. Butting two half_capsule()s together leaves a
# hairline seam where the two flat edges meet, which on the tablet's score line
# reads as a broken groove.
capsule <- function(cx, cy, len, r, deg, n = 128) {
  t1 <- seq(-pi / 2, pi / 2, length.out = n)
  t2 <- seq(pi / 2, 3 * pi / 2, length.out = n)
  px <- c(len / 2 + r * cos(t1), -len / 2 + r * cos(t2))
  py <- c(r * sin(t1), r * sin(t2))
  p <- rot(px, py, deg)
  data.table(x = cx + p$x, y = cy + p$y)
}

CAP <- list(cx = -0.74, cy = 0.06, len = 1.42, r = 0.30, deg = -21)
TAB <- list(cx = 0.98, cy = -0.26, r = 0.43)

d_hi <- half_capsule(CAP$cx, CAP$cy, CAP$len, CAP$r, CAP$deg, "right")
d_lo <- half_capsule(CAP$cx, CAP$cy, CAP$len, CAP$r, CAP$deg, "left")
d_tab <- circle(TAB$cx, TAB$cy, TAB$r)
d_score <- capsule(TAB$cx, TAB$cy, 0.34, 0.045, 0)

q <- ggplot()
q <- q + geom_polygon(data = d_hi, aes(x, y), fill = CFG$pill_hi)
q <- q + geom_polygon(data = d_lo, aes(x, y), fill = CFG$pill_lo)
q <- q + geom_polygon(data = d_tab, aes(x, y), fill = CFG$pill_lo)
q <- q + geom_polygon(data = d_score, aes(x, y), fill = CFG$score)
q <- q + coord_fixed(xlim = c(-1.85, 1.85), ylim = c(-0.95, 0.95))
q <- q + theme_void()
q <- q + theme(legend.position = "none")

sticker(
  q,
  package = "mht",
  p_size = 26,
  p_y = 1.46,
  p_color = CFG$text,
  p_family = "sans",
  s_x = 1.0,
  s_y = 0.88,
  s_width = 1.52,
  s_height = 0.86,
  h_fill = CFG$bg,
  h_color = CFG$border,
  h_size = 1.5,
  dpi = 600,
  filename = CFG$out
)
