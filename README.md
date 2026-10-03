# PhysioAppKit

**A domain-neutral engine for single-case (N-of-1) analysis in the x-biosignal
ecosystem.**

Application layers for different fields — rehabilitation, sport science,
psychophysiology, and others — share the same single-case machinery: a
distribution-free effect size, a threshold-and-band decision rule, a phase plot,
an honest hypothesis-to-evidence verdict, and a way to turn ordinal ratings into
an interval scale. PhysioAppKit provides that machinery once and knows nothing
about any specific domain; each application layer supplies its own semantics
(ontology, threshold, goal, reasoning, report) and calls this engine.

## Installation

```r
# the containers build on Bioconductor, so its repositories are needed too
install.packages("BiocManager", repos = "https://cloud.r-project.org")
install.packages("PhysioAppKit",
  repos = c("https://x-biosignal.r-universe.dev", BiocManager::repositories()))
```

## Quick start

```r
library(PhysioAppKit)

# Nonoverlap of All Pairs (NAP): a distribution-free single-case effect size.
nap(c(0.41, 0.39, 0.43), c(0.46, 0.55, 0.66), "increase")   # 1

# Single-case A-vs-B analysis: NAP + a meaningful threshold + a decision band.
a <- nonoverlap_analyze(c(0.41, 0.39, 0.43), c(0.46, 0.55, 0.66),
                        threshold = 0.16, direction = "increase")
a$nap               # 1
a$beyond_threshold  # TRUE  (the change exceeds the threshold)

# Rasch: turn ordinal item responses into an interval (logit) scale, so that
# equal raw-score steps are not mistaken for equal real change.
responses <- matrix(                  # 5 persons x 4 dichotomous (0/1) items
  c(1, 1, 0, 0,
    1, 1, 1, 0,
    1, 0, 0, 0,
    1, 1, 1, 1,
    0, 1, 0, 0),
  nrow = 5, byrow = TRUE,
  dimnames = list(paste0("p", 1:5), paste0("item", 1:4))
)
fit <- rasch_measure(responses)   # dependency-free dichotomous Rasch
raw_score_measure(fit$delta)      # raw score -> interval measure
```

## What it provides

| Function | Purpose |
|---|---|
| `nap()`, `interpret_nap()` | Nonoverlap of All Pairs effect size and interpretive bands. |
| `nonoverlap_analyze()` | Single-case A-vs-B analysis: NAP, threshold verdict, decision band. |
| `combine_verdicts()` | Combine per-item results into an honest supported / partial / refuted verdict. |
| `phase_plot()` | A single-case phase plot (baseline vs. intervention, band, threshold line). |
| `rasch_measure()`, `raw_score_measure()` | Dependency-free dichotomous Rasch (ordinal → interval). |
| `rbind_fill()`, `first_last()` | Small container utilities. |

## Used by

Application layers implement their five domain slots and call this engine — for
example **PhysioRehab** (rehabilitation: ICF semantics, MCID threshold,
goal-attainment scaling).

## Relationship to PhysioClinStats

The `nap()` here is a lightweight numeric entry point for orchestration; it is
verified to agree with the provenance-tracked `PhysioClinStats::scedNAP`, so the
two never diverge.

## License

MIT. See `CITATION.cff` for citation details.
