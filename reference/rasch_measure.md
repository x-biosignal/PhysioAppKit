# Fit a dichotomous Rasch model by JMLE (ordinal -\> interval)

Fit a dichotomous Rasch model by JMLE (ordinal -\> interval)

## Usage

``` r
rasch_measure(x, max_iter = 200, tol = 1e-05)
```

## Arguments

- x:

  persons x items matrix of 0/1 responses (NA allowed). Extreme
  persons/items (all-0 or all-1) are dropped from calibration.

- max_iter, tol:

  convergence controls.

## Value

list: \`theta\` (person measures, logits), \`delta\` (item difficulties,
centred), \`raw_person\`, \`converged\`, \`iterations\`.
