# Generate 4-D Trefoil Knot Coordinates

This function generates coordinates for a 4-D trefoil knot. The number
of points is determined by the length of the theta and phi sequences.

## Usage

``` r
gen_trefoil4d(n = 500, steps = 5)
```

## Arguments

- n:

  A numeric value (default: 500) representing the sample size.

- steps:

  A numeric value (default: 5) representing the number of steps for the
  theta parameter.

## Value

A data containing 4-D trefoil knot.

## Examples

``` r
set.seed(20240412)
trefoil4d <- gen_trefoil4d(n = 500, steps = 5)
#> ✔ Data generation completed successfully!!!
```
