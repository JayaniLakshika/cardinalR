# Generate data with curvy shaped branches

This function generates a dataset representing a structure with
non-linear shaped branches.

## Usage

``` r
gen_curvybranches(n = 400, k = 4)
```

## Arguments

- n:

  A numeric value (default: 400) representing the sample size.

- k:

  A numeric value (default: 4) representing the number of branches.

## Value

A data containing non-linear shaped branches.

## Examples

``` r
set.seed(20240412)
curvybranches <- gen_curvybranches(n = 400, k = 4)
#> ✔ Data generation completed successfully!!!
```
