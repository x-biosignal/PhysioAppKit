test_that("nap is 1 for full nonoverlap, 0.5 for complete overlap", {
  expect_equal(nap(c(1, 2, 3), c(4, 5, 6), "increase"), 1)
  expect_equal(nap(c(1, 2, 3), c(1, 2, 3), "increase"), 0.5)
  expect_equal(nap(c(4, 5, 6), c(1, 2, 3), "decrease"), 1)
  expect_true(is.na(nap(numeric(0), c(1, 2), "increase")))
})

test_that("interpret_nap uses the Parker-Vannest bands", {
  expect_equal(interpret_nap(0.95), "大きい効果")
  expect_equal(interpret_nap(0.75), "中程度の効果")
  expect_equal(interpret_nap(0.55), "小さい/不明な効果")
  expect_equal(interpret_nap(NA_real_), "判定不能")
})

test_that("nonoverlap_analyze reports threshold + band verdicts (both directions)", {
  inc <- nonoverlap_analyze(c(0.41, 0.39, 0.43), c(0.46, 0.55, 0.66),
                            threshold = 0.16, direction = "increase")
  expect_true(inc$beyond_threshold)
  expect_false(inc$adverse)
  expect_equal(inc$nap, 1)
  expect_gt(inc$band_hi, inc$baseline_mean)

  dec <- nonoverlap_analyze(c(18, 18, 19), c(11, 12, 10),
                            threshold = 3, direction = "decrease")
  expect_true(dec$beyond_threshold)   # load fell past the threshold
})

test_that("combine_verdicts is honest: all/any/none", {
  T_ <- list(supported = TRUE); F_ <- list(supported = FALSE)
  NAo <- list(supported = NA)
  expect_match(combine_verdicts(list(T_, T_)), "supported")
  expect_match(combine_verdicts(list(T_, F_)), "partial")
  expect_match(combine_verdicts(list(F_, F_)), "refuted")
  expect_match(combine_verdicts(list(T_, NAo)), "supported")   # unknowns ignored
})

test_that("rbind_fill unions columns; first_last picks non-NA ends", {
  a <- data.frame(x = 1)
  b <- data.frame(y = 2)
  out <- rbind_fill(a, b)
  expect_setequal(names(out), c("x", "y"))
  expect_equal(nrow(out), 2)
  expect_equal(first_last(c(NA, 3, NA, 7, NA)), c(3, 7))
})

test_that("rasch_measure recovers item difficulties; raw->measure is monotone", {
  set.seed(1)
  I <- 12; N <- 150
  delta_true <- sort(seq(-2.2, 2.2, length.out = I))
  theta <- stats::rnorm(N, 0, 1.4)
  x <- outer(theta, delta_true,
             function(t, d) stats::rbinom(length(t), 1, stats::plogis(t - d)))
  fit <- rasch_measure(x)
  expect_true(fit$converged)
  expect_gt(cor(delta_true, fit$delta, use = "complete"), 0.95)
  tab <- raw_score_measure(fit$delta)
  expect_equal(nrow(tab), I - 1)
  expect_true(all(diff(tab$measure) > 0))   # strictly increasing (interval scale)
})
