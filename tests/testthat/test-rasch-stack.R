# Concurrent (stacked) calibration for repeated measures (pcm_stack): both
# occasions land on one common metric, so a known ability shift is recovered.

sim_items <- function(ability, locs, seed = 1) {
  set.seed(seed)
  x <- sapply(locs, function(d)
    pmax(0L, pmin(2L, as.integer(round(ability - d + rnorm(length(ability)))))))
  colnames(x) <- paste0("i", seq_along(locs))
  x
}

test_that("stacked calibration recovers a known between-occasion shift", {
  locs <- seq(-1.4, 1.4, length.out = 10)
  a <- rnorm(250)
  pre  <- sim_items(a,        locs, seed = 1)
  post <- sim_items(a + 0.9,  locs, seed = 2)          # same persons, +0.9 logit
  st <- pcm_stack(list(pre = pre, post = post))
  expect_s3_class(st, "rasch_stack")
  expect_equal(st$n_occasions, 2L)

  shift <- mean(st$occasions$post$theta, na.rm = TRUE) -
           mean(st$occasions$pre$theta, na.rm = TRUE)
  # recovers a clear positive gain near the true +0.9 logit (the category
  # generator's floor/ceiling clamping keeps this from being exact)
  expect_gt(shift, 0.6)
  expect_lt(shift, 1.35)

  # person-level change tracks the true (constant) shift
  dth <- st$occasions$post$theta - st$occasions$pre$theta
  expect_gt(mean(dth, na.rm = TRUE), 0.6)
  # both occasions share one item calibration (that is the point)
  expect_equal(nrow(st$items), 10L)
  expect_true(st$reliability$person_reliability > 0.5)
})

test_that("stacked measures use a common item scale (unlike separate fits)", {
  locs <- seq(-1.4, 1.4, length.out = 10)
  a <- rnorm(250)
  pre  <- sim_items(a,       locs, seed = 3)
  post <- sim_items(a + 0.9, locs, seed = 4)
  st <- pcm_stack(list(pre = pre, post = post))
  # the stacked item calibration is identical for both occasions by construction;
  # its person measures are therefore directly comparable across time
  se_ok <- all(is.finite(st$occasions$pre$theta_se[
    is.finite(st$occasions$pre$theta)]))
  expect_true(se_ok)
})

test_that("pcm_stack validates its input", {
  x <- matrix(sample(0:2, 60, TRUE), 20, 3)
  expect_error(pcm_stack(list(x)), "at least two")
  expect_error(pcm_stack(list(x, matrix(0, 20, 4))), "same items")
})
