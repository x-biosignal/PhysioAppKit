# Verification of the polytomous Rasch engine (pcm_measure / rsm_measure).
#
# The correctness spine is two-fold, mirroring the ADR-0003 drift discipline
# used for NAP:
#   (1) with two categories the PCM and the RSM must reduce to the dichotomous
#       rasch_measure() -- a near-exact internal drift check;
#   (2) known thresholds and abilities must be recovered from simulated data.
# An external structural cross-check against eRm (CML) guards the definition.

# deterministic PCM sampler from probabilities (no RNG package deps)
.sample_cat <- function(theta, d) {
  p <- PhysioAppKit:::.pcm_probs(theta, d)
  cumsum_p <- cumsum(p)
  function(u) sum(u > cumsum_p)      # returns 0..m
}

sim_pcm <- function(theta, thresholds, seed = 1) {
  set.seed(seed)
  I <- length(thresholds)
  x <- matrix(NA_integer_, length(theta), I)
  for (i in seq_len(I)) {
    for (p in seq_along(theta)) {
      pr <- PhysioAppKit:::.pcm_probs(theta[p], thresholds[[i]])
      x[p, i] <- sum(runif(1) > cumsum(pr))
    }
  }
  colnames(x) <- paste0("i", seq_len(I))
  x
}

test_that("PCM reduces to dichotomous rasch_measure (drift)", {
  set.seed(11)
  th <- rnorm(300, 0, 1.3)
  di <- c(-1.5, -0.8, -0.2, 0.4, 1.1, 1.8)          # 6 dichotomous items
  x <- sapply(di, function(d) as.integer(runif(300) < plogis(th - d)))
  colnames(x) <- paste0("i", seq_along(di))

  ref <- rasch_measure(x)
  pcm <- pcm_measure(x, model = "PCM")

  ok_i <- is.finite(ref$delta) & is.finite(pcm$items$location)
  # item difficulties agree near-exactly
  expect_gt(cor(ref$delta[ok_i], pcm$items$location[ok_i]), 0.999)
  expect_lt(max(abs(ref$delta[ok_i] - pcm$items$location[ok_i])), 0.05)
  # person measures agree near-exactly
  ok_p <- is.finite(ref$theta) & is.finite(pcm$theta)
  expect_gt(cor(ref$theta[ok_p], pcm$theta[ok_p]), 0.999)
  expect_lt(max(abs(ref$theta[ok_p] - pcm$theta[ok_p])), 0.1)
})

test_that("RSM reduces to dichotomous rasch_measure (drift)", {
  set.seed(12)
  th <- rnorm(300, 0, 1.3)
  di <- c(-1.5, -0.8, -0.2, 0.4, 1.1, 1.8)
  x <- sapply(di, function(d) as.integer(runif(300) < plogis(th - d)))
  colnames(x) <- paste0("i", seq_along(di))

  ref <- rasch_measure(x)
  rsm <- pcm_measure(x, model = "RSM")
  ok_i <- is.finite(ref$delta) & is.finite(rsm$items$location)
  expect_gt(cor(ref$delta[ok_i], rsm$items$location[ok_i]), 0.999)
  expect_lt(max(abs(ref$delta[ok_i] - rsm$items$location[ok_i])), 0.05)
})

test_that("PCM recovers simulated thresholds and abilities", {
  th_true <- rnorm(500, 0, 1.2)
  # 12 three-category items (enough measurement information for a clean
  # person-recovery check; 6 items leaves person measures noisy by design)
  thr_true <- list(c(-1.2, 0.4), c(-0.6, 0.9), c(-0.2, 1.4), c(-1.6, -0.1),
                   c(0.2, 1.8), c(-0.9, 0.6), c(-1.0, 0.2), c(-0.3, 1.1),
                   c(0.5, 1.6), c(-1.4, 0.3), c(-0.1, 0.8), c(0.0, 1.3))
  x <- sim_pcm(th_true, thr_true, seed = 7)
  fit <- pcm_measure(x, model = "PCM")

  loc_true <- vapply(thr_true, mean, numeric(1))
  loc_true <- loc_true - mean(loc_true)             # same centring as the fit
  expect_gt(cor(loc_true, fit$items$location), 0.97)
  expect_lt(mean(abs(loc_true - fit$items$location)), 0.2)

  ok_p <- is.finite(fit$theta)
  # person recovery is bounded above by the test's information: the attainable
  # correlation is ~ sqrt(person reliability). Check we reach that ceiling.
  rho <- cor(th_true[ok_p], fit$theta[ok_p])
  expect_gt(rho, 0.9)
  expect_gt(fit$reliability$person_reliability, 0.8)
  expect_gt(rho, 0.95 * sqrt(fit$reliability$person_reliability))
  # thresholds recovered in order for a well-behaved item
  expect_false(any(fit$items$disordered[fit$items$n_cat == 3], na.rm = TRUE))
  # infit/outfit centred near 1 for model-fitting data
  expect_lt(abs(mean(fit$items$outfit, na.rm = TRUE) - 1), 0.25)
  # reliability sane
  expect_true(fit$reliability$person_reliability > 0.3 &&
                fit$reliability$person_reliability <= 1)
})

test_that("disordered thresholds are flagged", {
  # an item where the middle category is barely used -> disordered Andrich
  # thresholds; contrast with a cleanly ordered item.
  set.seed(3)
  th <- rnorm(400, 0, 1.2)
  ordered_item <- list(c(-1, 1))
  x_ord <- sim_pcm(th, ordered_item, seed = 5)
  # rarely-used middle: wide-then-narrow structure forces threshold reversal
  x_rev <- matrix(0L, 400, 1)
  x_rev[th > 0.3, 1] <- 2L
  x_rev[th > -0.3 & th <= 0.3 & runif(400) < 0.1, 1] <- 1L  # sparse middle
  colnames(x_ord) <- "ord"; colnames(x_rev) <- "rev"
  fit <- pcm_measure(cbind(x_ord, x_rev), model = "PCM")
  expect_false(isTRUE(fit$items$disordered[fit$items$item == "ord"]))
  expect_true(isTRUE(fit$items$disordered[fit$items$item == "rev"]))
})

test_that("raw-score to measure table is monotone non-linear", {
  thr <- list(c(-1, 1), c(-0.5, 0.8), c(0, 1.2))
  tab <- poly_raw_measure(thr)
  expect_equal(nrow(tab), sum(vapply(thr, length, integer(1))) - 1)  # 6-1 = 5
  expect_true(all(diff(tab$measure) > 0))                            # monotone
  # step sizes grow toward the extremes (ogive): first step > a middle step
  steps <- diff(tab$measure)
  expect_gt(steps[1], steps[length(steps) %/% 2 + 1] * 0.99)
})

test_that("empty intermediate categories are collapsed, extremes dropped", {
  # item coded 0/2/4 (gap at 1,3) must recode to 0/1/2 = 3 categories
  x <- cbind(a = c(0, 2, 4, 0, 2, 4, 0, 4),
             b = c(0, 1, 1, 0, 1, 0, 1, 1))
  fit <- pcm_measure(x, model = "PCM")
  expect_equal(fit$items$n_cat[fit$items$item == "a"], 3)
  expect_equal(fit$codes$a, c(0, 2, 4))
})

test_that("PCM item ordering matches eRm (CML) structurally", {
  skip_if_not_installed("eRm")
  th_true <- rnorm(400, 0, 1.1)
  thr_true <- list(c(-1.2, 0.4), c(-0.6, 0.9), c(-0.2, 1.4),
                   c(-1.6, -0.1), c(0.2, 1.8), c(-0.9, 0.6))
  x <- sim_pcm(th_true, thr_true, seed = 21)
  fit <- pcm_measure(x, model = "PCM")

  erm_loc <- tryCatch({
    res <- eRm::PCM(as.data.frame(x))
    tt <- eRm::thresholds(res)$threshtable[[1]]
    tt[, "Location"]
  }, error = function(e) NULL)
  skip_if(is.null(erm_loc), "eRm threshold extraction failed")

  # JMLE (ours) vs CML (eRm) differ in scale/bias but agree in structure:
  expect_gt(cor(as.numeric(erm_loc), fit$items$location, method = "spearman"),
            0.9)
  expect_gt(cor(as.numeric(erm_loc), fit$items$location), 0.9)
})
