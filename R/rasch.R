# ===========================================================================
# Rasch measurement -- ordinal -> interval (dichotomous Rasch, JMLE).
#
# Rehab, sport and psych all lean on ordinal ratings (ICF qualifiers, checklist
# pass/fail, Likert). A raw sum of ordinal items is NOT an interval scale: equal
# raw gains mean different real gains at different score levels. The Rasch model
# calibrates item difficulties and returns each person's ability on a linear
# (logit) scale, so change is measured honestly. This is a compact, dependency-
# free JMLE for the dichotomous case; polytomous (rating-scale) is a future add.
# ===========================================================================

.rasch_newton_person <- function(r, delta, obs, tol = 1e-6) {
  th <- 0
  for (k in 1:50) {
    pr <- stats::plogis(th - delta[obs]); E <- sum(pr); V <- sum(pr * (1 - pr))
    if (V < 1e-9) break
    step <- (r - E) / V; th <- th + step
    if (abs(step) < tol) break
  }
  max(min(th, 6), -6)
}

.rasch_newton_item <- function(s, theta, obs, tol = 1e-6) {
  d <- 0
  for (k in 1:50) {
    pr <- stats::plogis(theta[obs] - d); E <- sum(pr); V <- sum(pr * (1 - pr))
    if (V < 1e-9) break
    step <- (E - s) / V; d <- d + step
    if (abs(step) < tol) break
  }
  max(min(d, 6), -6)
}

#' Fit a dichotomous Rasch model by JMLE (ordinal -> interval)
#'
#' @param x persons x items matrix of 0/1 responses (NA allowed). Extreme
#'   persons/items (all-0 or all-1) are dropped from calibration.
#' @param max_iter,tol convergence controls.
#' @return list: `theta` (person measures, logits), `delta` (item difficulties,
#'   centred), `raw_person`, `converged`, `iterations`.
#' @export
rasch_measure <- function(x, max_iter = 200, tol = 1e-5) {
  x <- as.matrix(x)
  ps <- rowSums(x, na.rm = TRUE); is_ <- colSums(x, na.rm = TRUE)
  pmax_ <- rowSums(!is.na(x)); imax_ <- colSums(!is.na(x))
  keep_p <- ps > 0 & ps < pmax_; keep_i <- is_ > 0 & is_ < imax_
  xc <- x[keep_p, keep_i, drop = FALSE]
  P <- nrow(xc); I <- ncol(xc)
  theta <- rep(0, P); delta <- rep(0, I)
  rscore <- rowSums(xc, na.rm = TRUE); iscore <- colSums(xc, na.rm = TRUE)
  conv <- FALSE
  for (iter in seq_len(max_iter)) {
    t0 <- theta; d0 <- delta
    for (p in seq_len(P))
      theta[p] <- .rasch_newton_person(rscore[p], delta, !is.na(xc[p, ]))
    for (i in seq_len(I))
      delta[i] <- .rasch_newton_item(iscore[i], theta, !is.na(xc[, i]))
    delta <- delta - mean(delta)                       # identification
    if (max(abs(theta - t0)) < tol && max(abs(delta - d0)) < tol) {
      conv <- TRUE; break
    }
  }
  th_full <- rep(NA_real_, nrow(x)); th_full[keep_p] <- theta
  d_full <- rep(NA_real_, ncol(x)); d_full[keep_i] <- delta
  list(theta = th_full, delta = d_full, raw_person = ps,
       converged = conv, iterations = iter)
}

#' Raw-score -> Rasch-measure conversion table for a calibrated item set
#'
#' Shows the non-linear ogive: equal raw-score steps are unequal logit steps.
#' @param delta calibrated item difficulties (centred logits).
#' @return data.frame(raw, measure) for raw = 1 .. (n_items - 1).
#' @export
raw_score_measure <- function(delta) {
  delta <- delta[!is.na(delta)]; I <- length(delta)
  raws <- seq_len(I - 1)
  meas <- vapply(raws, function(r)
    .rasch_newton_person(r, delta, rep(TRUE, I)), numeric(1))
  data.frame(raw = raws, measure = round(meas, 3))
}
