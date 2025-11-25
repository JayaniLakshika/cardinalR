# Generate 3-D Trefoil Knot Coordinates (Stereographic Projection)

This function generates coordinates for a 3-D trefoil knot by applying a
stereographic projection from 4-D space.

## Usage

``` r
gen_trefoil3d(n = 500, steps = 5)
```

## Arguments

- n:

  A numeric value (default: 500) representing the sample size.

- steps:

  A numeric value (default: 5) representing the number of steps for the
  theta parameter.

## Value

A data containing 3-D trefoil knot.

## Examples

``` r
set.seed(20240412)
trefoil3d <- gen_trefoil3d(n = 500, steps = 5)
#> ✔ Data generation completed successfully!!!
#> ✔ Data generation completed successfully!!!
```
