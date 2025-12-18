# Generate data with exponential shaped branches

This function generates a dataset representing a structure with
exponential shaped branches.

## Usage

``` r
gen_expbranches(n = 400, k = 4)
```

## Arguments

- n:

  An integer value (default: 400) representing the sample size.

- k:

  An integer value (default: 4) representing the number of branches.

## Value

A tibble containing exponential shaped branches.

## Examples

``` r
set.seed(20240412)
expbranches <- gen_expbranches(n = 400, k = 4)
#> ✔ Data generation completed successfully!!!
```
