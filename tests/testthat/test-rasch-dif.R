# Differential item functioning (pcm_dif): a planted DIF item must be flagged,
# invariant items must not; and it must run on real grouped data (VerbAgg).

test_that("pcm_dif flags a planted DIF item and not the invariant ones", {
  set.seed(7)
  n <- 500L
  g <- rep(c("A", "B"), each = n / 2)
  ability <- rnorm(n)
  locs <- c(-1.5, -1.1, -0.7, -0.3, 0, 0.3, 0.7, 1.1, 1.5, 0)
  gen <- function(loc) pmax(0L, pmin(2L, as.integer(round(ability - loc +
                                                          rnorm(n)))))
  x <- sapply(locs, gen)
  colnames(x) <- paste0("i", seq_along(locs))
  # item 10 is 1.5 logits harder for group B: regenerate its group-B column at
  # the harder location (keeps all categories populated -- a shift-and-clamp
  # would collapse the item to a single category and drop it from calibration)
  idxB <- which(g == "B")
  x[idxB, 10] <- pmax(0L, pmin(2L, as.integer(round(
    ability[idxB] - 1.5 + rnorm(length(idxB))))))

  dif <- pcm_dif(x, g)
  expect_equal(attr(dif, "groups"), c("A", "B"))
  expect_equal(nrow(dif), 10L)
  # the planted item has the largest |contrast| and is flagged non-negligible
  expect_equal(which.max(abs(dif$contrast)), 10L)
  expect_true(dif$dif[10] %in% c("B", "C"))
  # the great majority of the invariant items are negligible (A)
  expect_gte(sum(dif$dif[1:9] == "A", na.rm = TRUE), 7L)
})

test_that("pcm_dif validates its group argument", {
  x <- matrix(sample(0:2, 60, TRUE), 20, 3)
  expect_error(pcm_dif(x, rep("A", 20)), "exactly two levels")
  expect_error(pcm_dif(x, rep(c("A", "B"), 5)), "one entry per person")
})

test_that("pcm_dif runs on real grouped data (VerbAgg by gender)", {
  skip_if_not_installed("lme4")
  data(VerbAgg, package = "lme4")
  VerbAgg$score <- as.integer(VerbAgg$resp) - 1L
  gender <- tapply(as.character(VerbAgg$Gender), VerbAgg$id,
                   function(v) v[1])
  w <- reshape(VerbAgg[, c("id", "item", "score")], idvar = "id",
               timevar = "item", direction = "wide")
  X <- as.matrix(w[, -1]); colnames(X) <- levels(VerbAgg$item)
  g <- gender[as.character(w$id)]

  dif <- pcm_dif(X, g)
  expect_equal(nrow(dif), ncol(X))
  expect_true(all(dif$dif[!is.na(dif$dif)] %in% c("A", "B", "C")))
  expect_true(all(c("item", "contrast", "t", "dif") %in% names(dif)))
})
