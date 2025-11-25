# Generate Cubic

This function generates a dataset representing a structure with a cubic
pattern.

## Usage

``` r
gen_cubic(n = 500, range = c(-1, 2))
```

## Arguments

- n:

  A numeric value (default: 500) representing the sample size.

- range:

  A numeric vector (default: c(-1, 2)) representing the range along x1
  axis.

## Value

A data containing a cubic structure.

## Examples

``` r
set.seed(20240412)
cubic <- gen_cubic(n = 500)
#> ✔ Data generation completed successfully!!!
```
