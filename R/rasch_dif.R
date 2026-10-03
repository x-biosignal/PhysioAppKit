# Differential item functioning (DIF) for the polytomous Rasch model.
#
# An ADL/IADL (or any) item shows DIF when two groups matched on the latent
# trait still respond differently to it -- e.g. a self-care item that is
# systematically harder for one sex after controlling for overall ability. DIF
# threatens the invariance that makes a Rasch measure comparable across groups,
# so screening for it is a standard step in every scale-validation study.
#
# Method: separate (per-group) calibration with each group's item scale centred
# to 0, then compare item locations. The centring removes the group ability
# difference, so the location contrast isolates relative item difficulty
# (Wright & Stone). Significance is the two-sample logit t-test on the contrast;
# magnitude is flagged with the ETS A/B/C bands adapted to the logit metric.

#' Differential item functioning between two groups (polytomous Rasch)
#'
#' Screens each item for DIF by calibrating the [pcm_measure()] model separately
#' in each group and comparing item locations. A large, significant location
#' contrast means the item is relatively harder in one group than the other at
#' equal ability -- a violation of measurement invariance.
#'
#' @param x persons x items matrix of ordered-category responses (see
#'   [pcm_measure()]).
#' @param group a length-`nrow(x)` grouping vector with exactly two levels.
#' @param model `"PCM"` (default) or `"RSM"`.
#' @param ... passed to [pcm_measure()].
#' @return a data.frame, one row per item: `item`, `location_1`, `location_2`
#'   (per group, logits), `contrast` (group1 - group2), `se`, `t`, and `dif`
#'   (ETS-style band: `"A"` negligible, `"B"` moderate, `"C"` large). Attribute
#'   `"groups"` records the level labels.
#' @references Wright BD, Stone MH (1979) Best Test Design; ETS DIF categories
#'   (Zwick 2012).
#' @seealso [pcm_measure()]
#' @export
#' @examples
#' set.seed(1)
#' n <- 300
#' g <- rep(c("A", "B"), each = n / 2)
#' ability <- rnorm(n)
#' # 5 invariant items + 1 item that is 1.2 logits harder for group B
#' x <- sapply(c(0, -0.5, 0.3, -0.8, 0.6, 0), function(d)
#'   pmax(0, pmin(2, round(ability - d + rnorm(n)))))
#' x[g == "B", 6] <- pmax(0, x[g == "B", 6] - 1L)      # induce DIF on item 6
#' pcm_dif(x, g)
pcm_dif <- function(x, group, model = c("PCM", "RSM"), ...) {
  model <- match.arg(model)
  x <- as.matrix(x)
  if (is.null(colnames(x))) colnames(x) <- paste0("item", seq_len(ncol(x)))
  g <- as.factor(group)
  if (length(g) != nrow(x)) stop("`group` must have one entry per person.",
                                 call. = FALSE)
  levs <- levels(droplevels(g))
  if (length(levs) != 2L) stop("`group` must have exactly two levels.",
                               call. = FALSE)

  fit1 <- pcm_measure(x[g == levs[1], , drop = FALSE], model = model, ...)
  fit2 <- pcm_measure(x[g == levs[2], , drop = FALSE], model = model, ...)
  items <- fit1$items$item
  j <- match(items, fit2$items$item)
  loc1 <- fit1$items$location; se1 <- fit1$items$se
  loc2 <- fit2$items$location[j]; se2 <- fit2$items$se[j]

  contrast <- loc1 - loc2
  se <- sqrt(se1^2 + se2^2)
  t <- contrast / se
  absc <- abs(contrast)
  dif <- ifelse(is.na(contrast) | is.na(t), NA_character_,
         ifelse(absc < 0.43 | abs(t) < 1.96, "A",
         ifelse(absc < 0.64, "B", "C")))

  out <- data.frame(item = items, location_1 = loc1, location_2 = loc2,
                    contrast = contrast, se = se, t = t, dif = dif,
                    stringsAsFactors = FALSE)
  attr(out, "groups") <- levs
  out
}
