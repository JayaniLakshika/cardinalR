# Generate Random Noise Dimensions

This function generates random noise dimensions to be added to the
coordinates of a data structure.

## Usage

``` r
gen_noisedims(n = 500, p = 4, m = rep(0, p), s = rep(2, p))
```

## Arguments

- n:

  A numeric value (default: 500) representing the sample size.

- p:

  A numeric value (default: 4) representing the number of dimensions.

- m:

  A numeric vector (default: c(0, 0, 0, 0)) representing the mean along
  each dimensions.

- s:

  A numeric vector (default: c(2, 2, 2, 2)) representing the standard
  deviation along each dimensions.

## Value

A data containing the generated random noise dimensions.

## Examples

``` r
set.seed(20240412)
gen_noisedims(n = 500, p = 4, m = c(0, 0, 0, 0), s = c(2, 2, 2, 2))
#> ✔ 4 noise dimensions have been generated successfully!!!
#> # A tibble: 500 × 4
#>        x1     x2     x3     x4
#>     <dbl>  <dbl>  <dbl>  <dbl>
#>  1 -2.64   1.98   2.29  -0.543
#>  2  1.28  -2.29  -2.56   2.06 
#>  3  2.58   1.58   2.60  -2.39 
#>  4 -2.57   0.775 -0.428  4.20 
#>  5 -0.755 -1.47  -2.52   2.05 
#>  6  0.308  1.86   2.22   0.980
#>  7  3.06   0.516  1.25   2.96 
#>  8 -0.781 -0.472  0.191  1.23 
#>  9  0.929  0.730  2.60   1.14 
#> 10 -5.96  -1.26  -2.23   0.355
#> # ℹ 490 more rows
```
