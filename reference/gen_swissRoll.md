# Generate Swiss Roll Data

This function generates swiss roll data.

## Usage

``` r
gen_swissroll(n = 500, w = c(-1, 1))
```

## Arguments

- n:

  A numeric value (default: 500) representing the sample size.

- w:

  A numeric vector (default: c(-1, 1)) representing the vertical
  variation.

## Value

A data containing the generated swiss roll data.

## References

Agrafiotis, D. K., & Xu, H. (2002). A self-organizing principle for
learning nonlinear manifolds. *Proceedings of the National Academy of
Sciences*, *99*(25), 15869-15872.

Roweis, S. T., & Saul, L. K. (2000). Nonlinear dimensionality reduction
by locally linear embedding. *Science*, *290*(5500), 2323-2326.

## Examples

``` r
set.seed(20240412)
swissroll <- gen_swissroll(n = 500)
#> ✔ Data generation completed successfully!!!
```
