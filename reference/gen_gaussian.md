# Generate Multivariate Gaussian Cloud

This function generates a dataset representing a structure with a
Gaussian.

## Usage

``` r
gen_gaussian(n = 500, p = 4, m = rep(0, p), s = diag(p) * 0.01)
```

## Arguments

- n:

  An integer value (default: 500) representing the sample size.

- p:

  An integer value (default: 4) representing the number of dimensions.

- m:

  A numeric vector (default: c(0, 0, 0, 0)) representing the mean along
  each dimensions.

- s:

  A numeric matrix (default: diag(4) \* 0.01) representing the variance
  of along each dimension.

## Value

A tibble containing a multivariate Gaussian cloud dataset.

## Examples

``` r
set.seed(20240412)
gaussian <- gen_gaussian(n = 500, p = 4, m = rep(0, 4), s = diag(4))
#> ✔ Data generation completed successfully!!!
```
