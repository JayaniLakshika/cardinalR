# Generate Long Linear Data

This function generates a dataset consisting of long linear data.

## Usage

``` r
gen_longlinear(n = 500, p = 4)
```

## Arguments

- n:

  A numeric value (default: 500) representing the sample size.

- p:

  A numeric value (default: 4) representing the number of dimensions.

## Value

A data containing the long linear data.

## Examples

``` r
set.seed(20240412)
longlinear <- gen_longlinear(n = 500, p = 4)
#> ✔ Data generation completed successfully!!!
```
