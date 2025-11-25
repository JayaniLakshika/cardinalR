# Generate Quadratic

This function generates a dataset representing a structure with a
quadratic pattern.

## Usage

``` r
gen_quadratic(n = 500, range = c(-1, 1))
```

## Arguments

- n:

  A numeric value (default: 500) representing the sample size.

- range:

  A numeric vector (default: c(-1, 1)) representing the range along x1
  axis.

## Value

A data containing a quadratic structure.

## Examples

``` r
set.seed(20240412)
quadratic <- gen_quadratic(n = 500)
#> ✔ Data generation completed successfully!!!
```
