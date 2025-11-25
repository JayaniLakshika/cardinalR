# Generate Gaussian

This function generates a dataset representing a structure with a
Gaussian.

## Usage

``` r
gen_gaussian(n = 500, p = 4, s = diag(p) * 0.01)
```

## Arguments

- n:

  A numeric value (default: 500) representing the sample size.

- p:

  A numeric value (default: 4) representing the number of dimensions.

- s:

  A numeric matrix (default: diag(4) \* 0.01) representing the variance
  of along each dimension.

## Value

A data containing a Gaussian.

## Examples

``` r
set.seed(20240412)
gaussian <- gen_gaussian(n = 500, p = 4, s = diag(4))
#> ✔ Data generation completed successfully!!!
```
