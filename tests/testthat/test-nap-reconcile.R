# Drift guard (ADR 0003): PhysioAppKit::nap must agree with the provenance-tracked
# PhysioClinStats::scedNAP. One definition, two entry points. Skipped where
# PhysioClinStats is unavailable (e.g. CRAN checks).
library(testthat)
library(PhysioAppKit)

test_that("nap() agrees with PhysioClinStats::scedNAP across cases", {
  skip_if_not_installed("PhysioClinStats")
  skip_if_not_installed("PhysioCore")
  cases <- list(
    list(a = c(0.41, 0.39, 0.43), b = c(0.46, 0.55, 0.66), dir = "increase"),
    list(a = c(10, 12, 11, 13),   b = c(9, 8, 7, 10),       dir = "decrease"),
    list(a = c(2, 2, 3, 2, 1),    b = c(3, 4, 3, 5, 4),     dir = "increase")
  )
  for (cs in cases) {
    mine <- PhysioAppKit::nap(cs$a, cs$b, cs$dir)
    ref <- PhysioCore::resultValue(
      PhysioClinStats::scedNAP(cs$a, cs$b, improvement = cs$dir))$estimate
    expect_equal(unname(mine), unname(ref), tolerance = 1e-9,
                 info = sprintf("direction = %s", cs$dir))
  }
})
