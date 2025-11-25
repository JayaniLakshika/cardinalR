# Generate data with exponential shaped branches

This function generates a dataset representing a structure with
exponential shaped branches.

## Usage

``` r
gen_expbranches(n = 400, k = 4)
```

## Arguments

- n:

  A numeric value (default: 400) representing the sample size.

- k:

  A numeric value (default: 4) representing the number of branches.

## Value

A data containing exponential shaped branches.

## Examples

``` r
set.seed(20240412)
expbranches <- gen_expbranches(n = 400, k = 4)
#> ✔ Data generation completed successfully!!!
```
