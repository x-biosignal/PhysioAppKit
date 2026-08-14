# Raw-score -\> Rasch-measure conversion table for a calibrated item set

Shows the non-linear ogive: equal raw-score steps are unequal logit
steps.

## Usage

``` r
raw_score_measure(delta)
```

## Arguments

- delta:

  calibrated item difficulties (centred logits).

## Value

data.frame(raw, measure) for raw = 1 .. (n_items - 1).
