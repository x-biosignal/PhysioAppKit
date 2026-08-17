# ===========================================================================
# Polytomous Rasch measurement -- ordinal -> interval for rating scales.
#
# ADL/IADL indices, ICF qualifiers, Likert items and most rehab / sport / psych
# instruments are polytomous ordinal. The dichotomous Rasch model
# (rasch_measure) handles pass/fail; the Partial Credit Model (Masters 1982)
# and the Rating Scale Model (Andrich 1978) extend it to ordered categories
# 0..m. Each item i carries Rasch-Andrich step thresholds delta_ik
# (k = 1..m_i); a person's ability theta and the item thresholds sit on one
# interval (logit) scale, so equal raw-score gains map to their true (unequal)
# real gains -- the whole point of Rasch scoring an ADL scale.
#
# Estimation is joint maximum likelihood (JMLE), the same engine style as the
# dichotomous case in rasch.R: alternate Newton updates of person abilities
# (the person raw score is the sufficient statistic) and of the step thresholds
# (the count of persons at or above a category is the sufficient statistic),
# centring the item scale each pass for identification. With two categories per
# item both models reduce exactly to the dichotomous Rasch model -- a drift
# test in test-rasch-poly.R pins this to rasch_measure().
#
# Dependency-free base R, consistent with rasch_measure().
# ===========================================================================

# --- category probabilities and moments (Partial Credit Model) -------------

# P(X = 0..m | theta, step thresholds d) for one person on one item.
# log-numerator of category x is sum_{k=1}^x (theta - d_k); category 0 is the
# reference (0). Computed on the log scale and max-shifted for stability.
.pcm_probs <- function(theta, d) {
  log_num <- c(0, cumsum(theta - d))
  log_num <- log_num - max(log_num)
  num <- exp(log_num)
  num / sum(num)
}

# expected score and variance (Fisher information) of one person-item.
.pcm_moments <- function(theta, d) {
  p <- .pcm_probs(theta, d)
  cats <- seq_along(p) - 1               # 0..m
  ex <- sum(cats * p)
  list(p = p, ex = ex, var = sum((cats - ex)^2 * p))
}

# Newton solve of a person's ability: raw score r is the sufficient statistic,
# so drive sum_i E[X_pi] to r. D is the list of step-threshold vectors for the
# items this person actually answered.
.pcm_newton_person <- function(r, D, tol = 1e-6, maxit = 50) {
  th <- 0
  for (k in seq_len(maxit)) {
    E <- 0; V <- 0
    for (i in seq_along(D)) {
      m <- .pcm_moments(th, D[[i]]); E <- E + m$ex; V <- V + m$var
    }
    if (V < 1e-9) break
    step <- (r - E) / V
    th <- th + step
    if (abs(step) < tol) break
  }
  max(min(th, 8), -8)
}

# Newton update of one item's step thresholds (Partial Credit): the sufficient
# statistic for threshold k is S_k = #{persons scoring >= k}. Drive
# sum_p P(X_pi >= k) to S_k, coordinate-wise over k (Gauss-Seidel within item).
.pcm_update_item <- function(d, theta_obs, resp_obs, tol = 1e-6, maxit = 50) {
  m <- length(d)
  Sk <- vapply(seq_len(m), function(k) sum(resp_obs >= k), numeric(1))
  for (it in seq_len(maxit)) {
    d0 <- d
    for (k in seq_len(m)) {
      A <- vapply(theta_obs, function(th)
        sum(.pcm_probs(th, d)[(k + 1):(m + 1)]), numeric(1))   # P(X >= k)
      den <- sum(A * (1 - A))
      if (den < 1e-9) next
      d[k] <- max(min(d[k] + (sum(A) - Sk[k]) / den, 10), -10)
    }
    if (max(abs(d - d0)) < tol) break
  }
  d
}

# --- input preparation -----------------------------------------------------

# Recode each item's observed ordinal levels to consecutive integers 0..m.
# Non-consecutive codes and empty intermediate categories (which have no data
# and hence an inestimable threshold) are collapsed away -- the standard Rasch
# response to a "null" category. The original levels are kept for back-mapping.
.prep_poly <- function(x) {
  x <- as.matrix(x)
  storage.mode(x) <- "double"
  I <- ncol(x)
  codes <- vector("list", I)
  xr <- x
  for (i in seq_len(I)) {
    lv <- sort(unique(x[!is.na(x[, i]), i]))
    codes[[i]] <- lv
    xr[, i] <- match(x[, i], lv) - 1
  }
  if (is.null(colnames(xr)))
    colnames(xr) <- paste0("item", seq_len(I))
  names(codes) <- colnames(xr)
  list(x = xr, codes = codes, m = vapply(codes, function(l) length(l) - 1L,
                                         integer(1)))
}

# --- fit, reliability ------------------------------------------------------

# infit / outfit mean-squares from the standardized-residual matrix and the
# model variance (information) matrix, taken along `margin` (1 = persons by
# row, 2 = items by column).
.poly_fit <- function(resid2, info, margin) {
  outfit <- apply(resid2 / info, margin, mean, na.rm = TRUE)         # z^2 mean
  infit_num <- apply(resid2, margin, sum, na.rm = TRUE)              # sum (x-E)^2
  infit_den <- apply(info,   margin, sum, na.rm = TRUE)              # sum var
  list(outfit = outfit, infit = infit_num / infit_den)
}

# Separation and reliability from measures and their standard errors
# (Wright & Masters): true variance = observed variance - mean squared error.
.sep_reliability <- function(measure, se) {
  ok <- is.finite(measure) & is.finite(se)
  measure <- measure[ok]; se <- se[ok]
  if (length(measure) < 2) return(list(separation = NA_real_, reliability = NA_real_))
  obs_var <- stats::var(measure)
  mse <- mean(se^2)
  true_var <- max(obs_var - mse, 0)
  sep <- if (mse > 0) sqrt(true_var / mse) else NA_real_
  list(separation = sep, reliability = if (obs_var > 0) true_var / obs_var else NA_real_)
}

# ---------------------------------------------------------------------------

#' Fit a polytomous Rasch model (Partial Credit or Rating Scale) by JMLE
#'
#' Turns a persons x items matrix of ordered-category responses into interval
#' (logit) person measures and item calibrations, the standard psychometric
#' treatment of an ordinal ADL/IADL scale, ICF-qualifier set or Likert
#' questionnaire. The Partial Credit Model (`"PCM"`, Masters 1982) lets each
#' item have its own step structure -- the model for scales whose items differ
#' in category count or spacing (e.g. the Barthel Index). The Rating Scale
#' Model (`"RSM"`, Andrich 1978) shares one set of category thresholds across
#' items and is appropriate when every item uses the same rating format (e.g.
#' all FIM items 1-7, or 0-4 ICF qualifiers).
#'
#' Item levels are recoded per item to consecutive categories 0..m; empty
#' intermediate categories are collapsed (their threshold is inestimable).
#' Extreme persons (all lowest / all highest category) and single-level items
#' are dropped from calibration and returned as `NA` measures.
#'
#' @param x persons x items matrix (or data.frame) of ordered-category
#'   responses coded as integers; `NA` allowed. Codes need not start at 0 or be
#'   consecutive -- they are treated ordinally and recoded internally.
#' @param model `"PCM"` (default) or `"RSM"`. RSM requires every item to share
#'   the same number of categories after recoding.
#' @param max_iter,tol outer JMLE convergence controls.
#' @return an object of class `poly_rasch`: a list with `theta` (person
#'   measures, logits, `NA` for extremes), `theta_se`, `raw_person`, `items`
#'   (data.frame of `location`, `se`, `infit`, `outfit`, `n_cat`, `disordered`),
#'   `thresholds` (long data.frame of Andrich step thresholds), `persons`
#'   (data.frame of person `infit`/`outfit`), `reliability` (person/item
#'   separation and reliability), `score_table` (raw total -> measure ogive),
#'   `model`, `converged`, `iterations`, and `codes` (per-item level recoding).
#' @seealso [rasch_measure()] for the dichotomous case, [poly_raw_measure()].
#' @export
#' @examples
#' set.seed(1)
#' # 80 persons, 6 four-category items; higher ability shifts categories up
#' ability <- rnorm(80)
#' x <- sapply(1:6, function(j)
#'   pmax(0, pmin(3, round(ability + rnorm(80) + 1))))
#' fit <- pcm_measure(x)
#' fit$items
pcm_measure <- function(x, model = c("PCM", "RSM"), max_iter = 200,
                        tol = 1e-4) {
  model <- match.arg(model)
  prep <- .prep_poly(x)
  xr <- prep$x; m <- prep$m
  P0 <- nrow(xr); I0 <- ncol(xr)
  item_names <- colnames(xr)

  keep_i <- m > 0
  if (!any(keep_i)) stop("no item has more than one observed category")
  if (model == "RSM" && length(unique(m[keep_i])) != 1L) {
    stop("RSM requires every item to have the same number of categories; ",
         "use model = \"PCM\" for a mixed-category scale such as the Barthel Index")
  }

  xc <- xr[, keep_i, drop = FALSE]
  mc <- m[keep_i]
  raw <- rowSums(xc, na.rm = TRUE)
  rmax <- rowSums(sweep(!is.na(xc), 2, mc, `*`))       # per-person max possible
  keep_p <- raw > 0 & raw < rmax
  xk <- xc[keep_p, , drop = FALSE]
  P <- nrow(xk); I <- ncol(xk)
  if (P < 1) stop("no non-extreme persons to calibrate")

  theta <- rep(0, P)
  D <- lapply(mc, function(mi) rep(0, mi))             # step thresholds per item
  r_p <- rowSums(xk, na.rm = TRUE)
  conv <- FALSE
  iter <- 0L
  for (iter in seq_len(max_iter)) {
    t0 <- theta; D0 <- unlist(D)
    # persons
    for (p in seq_len(P)) {
      obs <- which(!is.na(xk[p, ]))
      theta[p] <- .pcm_newton_person(r_p[p], D[obs])
    }
    # items
    if (model == "PCM") {
      for (i in seq_len(I)) {
        obs <- which(!is.na(xk[, i]))
        D[[i]] <- .pcm_update_item(D[[i]], theta[obs], xk[obs, i])
      }
    } else {
      D <- .rsm_update(D, theta, xk, mc[1])
    }
    D <- lapply(D, function(d) d - mean(unlist(D)))    # identification: centre
    if (max(abs(theta - t0)) < tol && max(abs(unlist(D) - D0)) < tol) {
      conv <- TRUE; break
    }
  }

  # --- residuals, fit, information over the kept block --------------------
  resid2 <- info <- matrix(NA_real_, P, I)
  theta_info <- numeric(P)
  for (p in seq_len(P)) for (i in seq_len(I)) {
    if (is.na(xk[p, i])) next
    mo <- .pcm_moments(theta[p], D[[i]])
    resid2[p, i] <- (xk[p, i] - mo$ex)^2
    info[p, i] <- mo$var
  }
  theta_se <- 1 / sqrt(rowSums(info, na.rm = TRUE))
  item_se_k <- 1 / sqrt(colSums(info, na.rm = TRUE))
  pf <- .poly_fit(resid2, info, 1)
  itf <- .poly_fit(resid2, info, 2)

  # --- assemble item-level table (map kept items back to all items) ------
  loc_k <- vapply(D, mean, numeric(1))                 # item location = mean step
  disord_k <- vapply(D, function(d) any(diff(d) < 0), logical(1))
  items <- data.frame(
    item = item_names,
    location = NA_real_, se = NA_real_, infit = NA_real_, outfit = NA_real_,
    n_cat = m + 1L, disordered = NA, stringsAsFactors = FALSE
  )
  items$location[keep_i] <- loc_k
  items$se[keep_i] <- item_se_k
  items$infit[keep_i] <- itf$infit
  items$outfit[keep_i] <- itf$outfit
  items$disordered[keep_i] <- disord_k

  thresholds <- do.call(rbind, Map(function(nm, d) {
    if (!length(d)) return(NULL)
    data.frame(item = nm, category = seq_along(d), threshold = d,
               stringsAsFactors = FALSE)
  }, item_names[keep_i], D))
  rownames(thresholds) <- NULL

  # --- full-length person outputs ----------------------------------------
  theta_full <- rep(NA_real_, P0); theta_full[keep_p] <- theta
  se_full <- rep(NA_real_, P0);    se_full[keep_p] <- theta_se
  infit_full <- rep(NA_real_, P0); infit_full[keep_p] <- pf$infit
  outfit_full <- rep(NA_real_, P0); outfit_full[keep_p] <- pf$outfit
  raw_full <- rep(NA_real_, P0);   raw_full[keep_p] <- r_p

  pr <- .sep_reliability(theta, theta_se)
  ir <- .sep_reliability(loc_k, item_se_k)

  structure(list(
    model = model,
    theta = theta_full, theta_se = se_full, raw_person = raw_full,
    items = items, thresholds = thresholds,
    persons = data.frame(infit = infit_full, outfit = outfit_full),
    reliability = list(person_separation = pr$separation,
                       person_reliability = pr$reliability,
                       item_separation = ir$separation,
                       item_reliability = ir$reliability),
    score_table = poly_raw_measure(D),
    converged = conv, iterations = iter,
    n_persons = P, n_items = I, codes = prep$codes
  ), class = "poly_rasch")
}

# Rating Scale Model item update: shared thresholds tau_k + item locations
# beta_i, with delta_ik = beta_i + tau_k. Identification is the standard RSM
# constraint sum_i beta_i = 0 and sum_k tau_k = 0 -- the latter is re-imposed
# after each threshold sweep, which is what removes the beta/tau confounding
# (for m = 1 it forces tau_1 = 0 and the model collapses to the dichotomous
# one). beta_i sufficient stat = item total; tau_k = global count scoring >= k.
.rsm_update <- function(D, theta, xk, m) {
  I <- ncol(xk)
  # decompose current delta_ik into item location beta_i + shared thresholds tau
  beta <- vapply(D, mean, numeric(1))
  tau <- colMeans(do.call(rbind, lapply(D, function(d) d - mean(d))))
  cap <- function(v) max(min(v, 10), -10)
  # item locations: drive each item's expected total to its observed total
  for (i in seq_len(I)) {
    obs <- which(!is.na(xk[, i]))
    Ti <- sum(xk[obs, i])
    for (it in 1:50) {
      mo <- lapply(theta[obs], function(th) .pcm_moments(th, beta[i] + tau))
      E <- sum(vapply(mo, `[[`, numeric(1), "ex"))
      V <- sum(vapply(mo, `[[`, numeric(1), "var"))
      if (V < 1e-9) break
      step <- (E - Ti) / V; beta[i] <- cap(beta[i] + step)   # difficulty sign
      if (abs(step) < 1e-6) break
    }
  }
  beta <- beta - mean(beta)
  # shared thresholds: drive global P(X >= k) to its global count, then re-centre
  # tau to sum-zero (the RSM identification) so its level cannot run away.
  for (k in seq_len(m)) {
    Sk <- sum(xk >= k, na.rm = TRUE)
    for (it in 1:50) {
      A <- numeric(0)
      for (i in seq_len(I)) {
        obs <- which(!is.na(xk[, i]))
        A <- c(A, vapply(theta[obs], function(th)
          sum(.pcm_probs(th, beta[i] + tau)[(k + 1):(m + 1)]), numeric(1)))
      }
      den <- sum(A * (1 - A))
      if (den < 1e-9) break
      step <- (sum(A) - Sk) / den; tau[k] <- cap(tau[k] + step)
      if (abs(step) < 1e-6) break
    }
  }
  tau <- tau - mean(tau)
  lapply(beta, function(b) b + tau)
}

#' Raw total-score to Rasch-measure table for a calibrated polytomous item set
#'
#' The polytomous analogue of [raw_score_measure()]: for a fully-answered set
#' of items with the given step thresholds, the interval measure each possible
#' raw total maps to. Exposes the non-linear ogive -- equal raw-score steps are
#' unequal logit steps, largest near the floor and ceiling.
#'
#' @param thresholds a list of numeric step-threshold vectors, one per item
#'   (e.g. the `D` used internally, or rebuilt from a `poly_rasch$thresholds`
#'   table). Extreme totals (0 and the maximum) return `NA`.
#' @return data.frame(`raw`, `measure`, `se`) for raw = 1 .. (max - 1).
#' @seealso [pcm_measure()], [raw_score_measure()]
#' @export
poly_raw_measure <- function(thresholds) {
  D <- thresholds
  rmax <- sum(vapply(D, length, integer(1)))
  raws <- seq_len(rmax - 1)
  meas <- vapply(raws, function(r) .pcm_newton_person(r, D), numeric(1))
  se <- vapply(meas, function(th)
    1 / sqrt(sum(vapply(D, function(d) .pcm_moments(th, d)$var, numeric(1)))),
    numeric(1))
  data.frame(raw = raws, measure = round(meas, 3), se = round(se, 3))
}

#' @export
print.poly_rasch <- function(x, ...) {
  cat(sprintf("<poly_rasch> model=%s | %d persons x %d items | %s (%d iter)\n",
              x$model, x$n_persons, x$n_items,
              if (x$converged) "converged" else "NOT converged", x$iterations))
  cat(sprintf("  person reliability=%.2f (separation=%.2f)\n",
              x$reliability$person_reliability, x$reliability$person_separation))
  nd <- sum(x$items$disordered, na.rm = TRUE)
  cat(sprintf("  items: %d calibrated, %d with disordered thresholds\n",
              sum(!is.na(x$items$location)), nd))
  invisible(x)
}

# ---------------------------------------------------------------------------
# Concurrent (stacked) calibration for repeated measures.
#
# To measure change honestly, both occasions must be on a common metric -- the
# item calibration must be identical, or the person-measure difference confounds
# real change with a shift in the yardstick. Concurrent calibration stacks the
# occasions into one matrix, calibrates the items ONCE, and reads each person-
# occasion off that shared scale. (It assumes the items do not drift across
# occasions; test that assumption with pcm_dif() using occasion as the group.)
# ---------------------------------------------------------------------------

#' Concurrent (stacked) polytomous Rasch calibration for repeated measures
#'
#' Calibrates the items ONCE across all occasions so that every person-occasion
#' measure sits on one common interval scale -- the anchoring that makes a
#' pre-to-post change interpretable. Stacks the occasion response matrices,
#' fits [pcm_measure()] on the pooled data, and splits the person measures back
#' out by occasion (in the original row order).
#'
#' @param occasions a named list of persons x items matrices, one per occasion;
#'   all must share the same items (columns). A person present at several
#'   occasions occupies the same row index in each.
#' @param model `"PCM"` (default) or `"RSM"`.
#' @param ... passed to [pcm_measure()].
#' @return an object of class `rasch_stack`: `items`/`thresholds`/`reliability`
#'   from the pooled calibration, and `occasions` -- a named list where each
#'   element is `list(theta, theta_se, raw)` for that occasion's persons on the
#'   common metric (ready to pass to a responder analysis).
#' @seealso [pcm_measure()], [pcm_dif()]
#' @export
#' @examples
#' set.seed(1)
#' items <- function(a) sapply(seq(-1, 1, length.out = 6), function(d)
#'   pmax(0, pmin(2, round(a - d + rnorm(length(a))))))
#' a <- rnorm(80)
#' st <- pcm_stack(list(pre = items(a), post = items(a + 0.8)))  # +0.8 logit gain
#' mean(st$occasions$post$theta, na.rm = TRUE) -
#'   mean(st$occasions$pre$theta, na.rm = TRUE)
pcm_stack <- function(occasions, model = c("PCM", "RSM"), ...) {
  model <- match.arg(model)
  if (!is.list(occasions) || length(occasions) < 2L) {
    stop("`occasions` must be a list of at least two response matrices.",
         call. = FALSE)
  }
  mats <- lapply(occasions, as.matrix)
  if (length(unique(vapply(mats, ncol, integer(1)))) != 1L) {
    stop("all occasions must have the same items (columns).", call. = FALSE)
  }
  ns <- vapply(mats, nrow, integer(1))
  stacked <- do.call(rbind, mats)
  fit <- pcm_measure(stacked, model = model, ...)

  end <- cumsum(ns); start <- c(1L, end[-length(end)] + 1L)
  labs <- names(occasions) %||% paste0("occ", seq_along(occasions))
  occ <- Map(function(s, e) list(
    theta = fit$theta[s:e], theta_se = fit$theta_se[s:e],
    raw = fit$raw_person[s:e]), start, end)
  names(occ) <- labs

  structure(list(items = fit$items, thresholds = fit$thresholds,
                 reliability = fit$reliability, occasions = occ,
                 model = model, converged = fit$converged,
                 n_occasions = length(occ)), class = "rasch_stack")
}

#' @export
print.rasch_stack <- function(x, ...) {
  cat(sprintf("<rasch_stack> model=%s | %d occasions (%s) | %s\n",
              x$model, x$n_occasions, paste(names(x$occasions), collapse = ", "),
              if (x$converged) "converged" else "NOT converged"))
  cat(sprintf("  shared calibration: %d items, person reliability=%.2f\n",
              sum(!is.na(x$items$location)), x$reliability$person_reliability))
  for (nm in names(x$occasions)) {
    th <- x$occasions[[nm]]$theta
    cat(sprintf("  %-8s mean measure = %.2f logits (n=%d)\n",
                nm, mean(th, na.rm = TRUE), sum(!is.na(th))))
  }
  invisible(x)
}
