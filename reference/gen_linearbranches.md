# Generate data with linear shaped branches

This function generates a dataset representing a structure with linear
shaped branches.

## Usage

``` r
gen_linearbranches(n = 400, k = 4)
```

## Arguments

- n:

  A numeric value (default: 400) representing the sample size.

- k:

  A numeric value (default: 4) representing the number of branches.

## Value

A data containing linear shaped branches.

## Examples

``` r
set.seed(20240412)
linearbranches <- gen_linearbranches(n = 400, k = 4)
#> ✔ Data generation completed successfully!!!
```
