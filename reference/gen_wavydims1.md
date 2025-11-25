# Generate Random Noise Dimensions With Wavy Pattern

This function generates random noise dimensions by adding wavy patterns.

## Usage

``` r
gen_wavydims1(n = 500, p = 4, theta = seq(pi/6, 12 * pi/6, length.out = 500))
```

## Arguments

- n:

  A numeric value (default: 500) representing the sample size.

- p:

  A numeric value (default: 4) representing the number of dimensions.

- theta:

  A numeric vector representing the nonlinearity along each dimensions.

## Value

A data containing the generated random noise dimensions.

## Examples

``` r
set.seed(20240412)
gen_wavydims1(n = 500, p = 4, theta = seq(pi / 6, 12 * pi / 6, length.out = 500))
#> ✔ Wavy shaped noise dimensions generation completed successfully!!!
#> # A tibble: 500 × 4
#>         x1    x2      x3    x4
#>      <dbl> <dbl>   <dbl> <dbl>
#>  1  0.598   2.11  0.330   1.17
#>  2  0.419   1.43  0.920   1.30
#>  3  0.0740  1.80  0.409   2.02
#>  4  0.683   2.01  0.551   2.39
#>  5  0.531   2.32  1.29    2.12
#>  6  1.30    1.96  0.556   2.09
#>  7  1.23    2.15  0.937   2.08
#>  8 -0.330   1.72  1.47    1.79
#>  9  1.29    1.78  0.695   2.56
#> 10 -0.125   2.62 -0.0498  1.75
#> # ℹ 490 more rows
```
