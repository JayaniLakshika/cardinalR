# Generate Hollow Sphere

This function generates a dataset representing a structure with a sphere
with points on the surface.

## Usage

``` r
gen_hollowsphere(n = 500, p = 4)
```

## Arguments

- n:

  A numeric value (default: 500) representing the sample size.

- p:

  A numeric value (default: 4) representing the number of dimensions.

## Value

A data containing a hollow sphere.

## Examples

``` r
set.seed(20240412)
hollowsphere <- gen_hollowsphere(n = 500)
#> ✔ Data generation completed successfully!!!
```
