# PhysioAppKit 0.2.0

Polytomous Rasch measurement for ordinal ADL/IADL, ICF-qualifier and Likert data.

* `pcm_measure()`: Partial Credit (Masters 1982) and Rating Scale (Andrich 1978)
  models by JMLE, with person/item measures, Andrich thresholds and
  disordered-threshold detection, infit/outfit, person/item separation
  reliability and the raw-to-interval ogive (`poly_raw_measure()`). Reduces
  exactly to the dichotomous `rasch_measure()` at two categories.
* `pcm_dif()`: differential item functioning between two groups (logit-contrast
  t-test, ETS A/B/C bands).
* `pcm_stack()`: concurrent (stacked) calibration so repeated-measure occasions
  share one item scale, making pre-to-post change interpretable.


# PhysioAppKit 0.1.0

First public release. Domain-neutral single-case engine for physiological
application layers.

* `nap()` / `interpret_nap()`: Nonoverlap of All Pairs effect size + bands.
* `nonoverlap_analyze()`: single-case A-vs-B analysis (NAP, threshold verdict,
  decision band), the shared math behind MCID / SWC / SESOI layers.
* `combine_verdicts()`: honest supported / partial / refuted from per-item flags.
* `phase_plot()`: generic single-case phase plot.
* `rasch_measure()` / `raw_score_measure()`: dependency-free dichotomous Rasch
  (JMLE) turning ordinal ratings into an interval (logit) scale.
* `rbind_fill()`, `first_last()`: container utilities.
