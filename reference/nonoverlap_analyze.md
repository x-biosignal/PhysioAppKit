# Single-case A-vs-B analysis: NAP + threshold verdict + decision band

The shared math behind rehab's \`sced_analyze\` (threshold = MCID),
sport's \`sport_analyze\` (threshold = SWC) and psych's condition
contrast (threshold = SESOI).

## Usage

``` r
nonoverlap_analyze(
  a,
  b,
  threshold,
  direction = c("increase", "decrease"),
  band_halfwidth = NULL
)
```

## Arguments

- a, b:

  group A / group B values.

- threshold:

  the change that matters (MCID / SWC / SESOI).

- direction:

  improvement direction.

- band_halfwidth:

  half-width of the decision band; defaults to \`2 \* sd(a)\`.

## Value

a list with nap, tau, interpretation, baseline_mean/sd, latest, delta,
signed, beyond_threshold, adverse (a full threshold the wrong way),
band_lo/hi, band_beyond, n_b, a, b.
