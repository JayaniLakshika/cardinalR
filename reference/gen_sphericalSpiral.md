# Generate Spherical Spiral

This function generates a dataset representing a structure with a
spherical spiral.

## Usage

``` r
gen_sphericalspiral(n = 500, spins = 1)
```

## Arguments

- n:

  A numeric value (default: 500) representing the sample size.

- spins:

  A numeric value (default: 1) representing the number of loops of the
  spiral.

## Value

A data containing a spherical spiral.

## Examples

``` r
set.seed(20240412)
sphericalspiral <- gen_sphericalspiral(n = 500, spins = 1)
#> ✔ Data generation completed successfully!!!
```
