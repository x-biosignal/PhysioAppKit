# Generic single-case phase plot (A vs B, decision band, threshold line)

Generic single-case phase plot (A vs B, decision band, threshold line)

## Usage

``` r
phase_plot(
  x,
  y,
  a_idx,
  b_idx,
  center,
  lo,
  hi,
  thr,
  thr_lab,
  ylab,
  main,
  a_lab = "基準(A)",
  b_lab = "介入(B)",
  b_col = "#d95f0e",
  b_bg = "#fec44f",
  file = NULL
)
```

## Arguments

- x, y:

  point positions and values.

- a_idx, b_idx:

  indices of group A / group B points.

- center, lo, hi:

  baseline mean and decision-band edges.

- thr:

  threshold line value.

- thr_lab:

  label drawn at the threshold line.

- ylab, main:

  axis and title text.

- a_lab, b_lab:

  phase labels for group A and group B.

- b_col, b_bg:

  group-B line and point colours.

- file:

  optional PNG path.

## Value

(invisibly) the file path or NULL.
