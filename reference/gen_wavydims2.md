# Generate Random Noise Dimensions With Wavy Pattern

This function generates random noise dimensions by adding wavy patterns.

## Usage

``` r
gen_wavydims2(n = 500, p = 4, x1_vec)
```

## Arguments

- n:

  A numeric value (default: 500) representing the sample size.

- p:

  A numeric value (default: 4) representing the number of dimensions.

- x1_vec:

  A numeric vector representing the first dimension of the data
  structure.

## Value

A data containing the generated random noise dimensions.

## Examples

``` r
set.seed(20240412)
theta <- seq(0, 2 * pi, length.out = 500)
x1 <- sin(pi) * cos(theta)
gen_wavydims2(n = 500, p = 4, x1_vec = x1)
#> ✔ Wavy shaped noise dimensions generation completed successfully!!!
#> # A tibble: 500 × 4
#>           x1        x2       x3       x4
#>        <dbl>     <dbl>    <dbl>    <dbl>
#>  1  0.000910  0.0185    0.0143  -0.0331 
#>  2 -0.00919  -0.00517   0.0170   0.0629 
#>  3  0.0186    0.00205  -0.00405 -0.00339
#>  4  0.0222    0.0312    0.00931 -0.00348
#>  5  0.00516   0.0139    0.00406 -0.00304
#>  6  0.0123    0.0238    0.00255  0.0591 
#>  7  0.00881   0.0309    0.00899  0.0691 
#>  8  0.00412   0.00188   0.0107  -0.0226 
#>  9  0.00292  -0.00113   0.0154   0.0264 
#> 10 -0.0106   -0.000241 -0.00240  0.0336 
#> # ℹ 490 more rows
```
