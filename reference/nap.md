# Nonoverlap of All Pairs (NAP)

Distribution-free single-case / two-group effect size: the proportion of
A x B pairs that improve (0.5 = complete overlap / no effect).

## Usage

``` r
nap(a, b, direction = c("increase", "decrease"))
```

## Arguments

- a:

  group A (baseline / condition 1) values.

- b:

  group B (follow-up / condition 2) values.

- direction:

  "increase" (higher is better) or "decrease".

## Value

NAP in \`\[0, 1\]\`.
