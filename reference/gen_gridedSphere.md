# Generate Grided Sphere

This function generates a dataset representing a structure with a grided
sphere.

## Usage

``` r
gen_gridedsphere(n = 500, p = 4)
```

## Arguments

- n:

  A numeric value (default: 500) representing the sample size.

- p:

  A numeric value (default: 4) representing the number of dimensions.

## Value

A data containing a grided sphere.

## Examples

``` r
set.seed(20240412)
gridedsphere <- gen_gridedsphere(n = 500, p = 4)
#> ✔ Data generation completed successfully!!!
```
