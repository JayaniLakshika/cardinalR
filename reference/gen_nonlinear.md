# Generate Nonlinear Hyperbola

This function generates a dataset representing a nonlinear hyperbola
structure.

## Usage

``` r
gen_nonlinear(n = 500, hc = 1, non_fac = 0.5)
```

## Arguments

- n:

  A numeric value (default: 500) representing the sample size.

- hc:

  A numeric value (default: 1) representing the hyperbolic component
  which define the steepness and vertical scaling of the hyperbola.
  Larger values of this make the curve more pronounced (sharper
  dips/rises near 0), while smaller values make it flatter.

- non_fac:

  A numeric value (default: 1) representing the nonlinear factor which
  describes the strength of this sinusoidal effect. When this is 0, the
  curve is purely hyperbolic; as it increases, the wave-like
  fluctuations become more prominent.

## Value

A data containing a nonlinear hyperbola structure.

## Examples

``` r
set.seed(20240412)
nonlinear <- gen_nonlinear(n = 500, hc = 1, non_fac = 0.5)
#> ✔ Data generation completed successfully!!!
```
