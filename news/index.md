# Changelog

## PhysioAppKit 0.1.0

First public release. Domain-neutral single-case engine for
physiological application layers.

- [`nap()`](https://x-biosignal.github.io/PhysioAppKit/reference/nap.md)
  /
  [`interpret_nap()`](https://x-biosignal.github.io/PhysioAppKit/reference/interpret_nap.md):
  Nonoverlap of All Pairs effect size + bands.
- [`nonoverlap_analyze()`](https://x-biosignal.github.io/PhysioAppKit/reference/nonoverlap_analyze.md):
  single-case A-vs-B analysis (NAP, threshold verdict, decision band),
  the shared math behind MCID / SWC / SESOI layers.
- [`combine_verdicts()`](https://x-biosignal.github.io/PhysioAppKit/reference/combine_verdicts.md):
  honest supported / partial / refuted from per-item flags.
- [`phase_plot()`](https://x-biosignal.github.io/PhysioAppKit/reference/phase_plot.md):
  generic single-case phase plot.
- [`rasch_measure()`](https://x-biosignal.github.io/PhysioAppKit/reference/rasch_measure.md)
  /
  [`raw_score_measure()`](https://x-biosignal.github.io/PhysioAppKit/reference/raw_score_measure.md):
  dependency-free dichotomous Rasch (JMLE) turning ordinal ratings into
  an interval (logit) scale.
- [`rbind_fill()`](https://x-biosignal.github.io/PhysioAppKit/reference/rbind_fill.md),
  [`first_last()`](https://x-biosignal.github.io/PhysioAppKit/reference/first_last.md):
  container utilities.
