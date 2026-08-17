# ===========================================================================
# PhysioAppKit -- domain-neutral single-case engine.
#
# Extracted from PhysioRehab / PhysioSport / PhysioPsych at the third instance
# (rule of three). Knows nothing about rehab, sport or psychophysiology: it
# operates on numeric vectors and generic "group A vs group B" contrasts. Each
# application layer supplies the domain slots (ontology, threshold, goal frame,
# reasoning copy, report) and calls these functions.
# ===========================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

#' Row-bind two data.frames with differing columns (fill missing with NA)
#' @param a,b data.frames. @return their union-column rbind. @export
rbind_fill <- function(a, b) {
  if (nrow(a) == 0) return(b)
  cols <- union(names(a), names(b))
  for (cc in setdiff(cols, names(a))) a[[cc]] <- NA
  for (cc in setdiff(cols, names(b))) b[[cc]] <- NA
  rbind(a[cols], b[cols])
}

#' First and last non-NA of a vector
#' @param v numeric. @return length-2 vector c(first, last). @export
first_last <- function(v) {
  idx <- which(!is.na(v)); if (!length(idx)) return(c(NA, NA))
  c(v[idx[1]], v[idx[length(idx)]])
}

.open_png <- function(file, width, height, res) {
  if (requireNamespace("ragg", quietly = TRUE)) {
    ragg::agg_png(file, width = width, height = height, res = res); return(invisible())
  }
  if (isTRUE(capabilities("cairo")))
    grDevices::png(file, width = width, height = height, res = res, type = "cairo")
  else grDevices::png(file, width = width, height = height, res = res)
  invisible()
}

#' Nonoverlap of All Pairs (NAP)
#'
#' Distribution-free single-case / two-group effect size: the proportion of
#' A x B pairs that improve (0.5 = complete overlap / no effect).
#'
#' @param a group A (baseline / condition 1) values.
#' @param b group B (follow-up / condition 2) values.
#' @param direction "increase" (higher is better) or "decrease".
#' @return NAP in `[0, 1]`.
#' @export
nap <- function(a, b, direction = c("increase", "decrease")) {
  direction <- match.arg(direction)
  a <- a[!is.na(a)]; b <- b[!is.na(b)]
  if (!length(a) || !length(b)) return(NA_real_)
  m <- outer(b, a, function(bj, ai) {
    d <- bj - ai; if (direction == "decrease") d <- -d
    ifelse(d > 0, 1, ifelse(d == 0, 0.5, 0))
  })
  mean(m)
}

#' Interpret a NAP value (Parker & Vannest 2009 bands)
#' @param x a NAP value. @return a Japanese effect-size label. @export
interpret_nap <- function(x) {
  if (is.na(x)) return("判定不能")
  if (x >= 0.93) "大きい効果" else if (x >= 0.66) "中程度の効果"
  else if (x >= 0.50) "小さい/不明な効果" else "反対方向"
}

#' Single-case A-vs-B analysis: NAP + threshold verdict + decision band
#'
#' The shared math behind rehab's `sced_analyze` (threshold = MCID), sport's
#' `sport_analyze` (threshold = SWC) and psych's condition contrast
#' (threshold = SESOI).
#'
#' @param a,b group A / group B values.
#' @param threshold the change that matters (MCID / SWC / SESOI).
#' @param direction improvement direction.
#' @param band_halfwidth half-width of the decision band; defaults to `2 * sd(a)`.
#' @return a list with nap, tau, interpretation, baseline_mean/sd, latest, delta,
#'   signed, beyond_threshold, adverse (a full threshold the wrong way), band_lo/hi,
#'   band_beyond, n_b, a, b.
#' @export
nonoverlap_analyze <- function(a, b, threshold, direction = c("increase", "decrease"),
                               band_halfwidth = NULL) {
  direction <- match.arg(direction)
  a <- a[!is.na(a)]; b <- b[!is.na(b)]
  napv <- nap(a, b, direction)
  base_mean <- mean(a); base_sd <- stats::sd(a)
  hw <- band_halfwidth %||% (2 * base_sd)
  latest <- b[length(b)]
  delta <- latest - base_mean
  signed <- if (direction == "decrease") -delta else delta
  thr_edge <- if (direction == "increase") base_mean + threshold else base_mean - threshold
  beyond <- if (direction == "increase") sum(b > thr_edge) else sum(b < thr_edge)
  list(nap = napv, tau = 2 * napv - 1, interpretation = interpret_nap(napv),
       baseline_mean = base_mean, baseline_sd = base_sd,
       latest = latest, delta = delta, signed = signed,
       beyond_threshold = signed >= threshold, adverse = signed <= -threshold,
       threshold = threshold, band_lo = base_mean - hw, band_hi = base_mean + hw,
       band_beyond = beyond, n_b = length(b), a = a, b = b)
}

#' Combine per-item verdicts into an honest overall (supported/partial/refuted)
#'
#' @param items list of items, each with a `$supported` field that is TRUE,
#'   FALSE or NA.
#' @return one of the Japanese overall labels.
#' @export
combine_verdicts <- function(items) {
  flags <- vapply(items, function(i) isTRUE(i$supported), logical(1))
  known <- !vapply(items, function(i) is.na(i$supported), logical(1))
  if (all(flags[known])) "支持 (supported)"
  else if (any(flags[known])) "部分的に支持 (partial)"
  else "不支持 (refuted)"
}

#' Generic single-case phase plot (A vs B, decision band, threshold line)
#'
#' @param x,y point positions and values.
#' @param a_idx,b_idx indices of group A / group B points.
#' @param center,lo,hi baseline mean and decision-band edges.
#' @param thr threshold line value.
#' @param thr_lab label drawn at the threshold line.
#' @param ylab,main axis and title text.
#' @param a_lab,b_lab phase labels for group A and group B.
#' @param b_col,b_bg group-B line and point colours.
#' @param file optional PNG path.
#' @return (invisibly) the file path or NULL.
#' @export
phase_plot <- function(x, y, a_idx, b_idx, center, lo, hi, thr, thr_lab,
                       ylab, main, a_lab = "基準(A)", b_lab = "介入(B)",
                       b_col = "#d95f0e", b_bg = "#fec44f", file = NULL) {
  if (!is.null(file)) .open_png(file, 1050, 640, 112)
  op <- graphics::par(mar = c(4.6, 4.8, 3.2, 1.2)); on.exit(graphics::par(op), add = TRUE)
  yr <- range(c(y, lo, hi, thr), na.rm = TRUE)
  graphics::plot(x, y, type = "n", xlab = "観測回", ylab = ylab,
                 ylim = yr + c(-.02, .02) * diff(yr), main = main)
  graphics::rect(min(x) - 1, lo, max(x) + 1, hi,
                 col = grDevices::rgb(.2, .5, .7, .08), border = NA)
  graphics::abline(h = center, lty = 2, col = "grey45")
  graphics::abline(h = thr, lty = 3, lwd = 2, col = "#2c7fb8")
  graphics::abline(v = (max(a_idx) + min(b_idx)) / 2, col = "grey30", lwd = 1.5)
  graphics::lines(x[a_idx], y[a_idx], col = "grey40", lwd = 2)
  graphics::points(x[a_idx], y[a_idx], pch = 21, bg = "grey70", cex = 1.3)
  graphics::lines(x[b_idx], y[b_idx], col = b_col, lwd = 2)
  graphics::points(x[b_idx], y[b_idx], pch = 21, bg = b_bg, cex = 1.3)
  graphics::text(mean(a_idx), yr[2], a_lab, col = "grey35", font = 2)
  graphics::text(mean(b_idx), yr[2], b_lab, col = b_col, font = 2)
  graphics::text(max(x), thr, thr_lab, col = "#2c7fb8", pos = 3, cex = .85)
  if (!is.null(file)) { grDevices::dev.off(); return(invisible(file)) }
  invisible(NULL)
}
