# Generate Random Noise Dimensions With Wavy Pattern

This function generates random noise dimensions by adding wavy patterns.

## Usage

``` r
gen_wavydims3(n = 500, p = 4, data)
```

## Arguments

- n:

  A numeric value (default: 500) representing the sample size.

- p:

  A numeric value (default: 4) representing the number of dimensions.

- data:

  A matrix representing the first three dimensions of the data
  structure.

## Value

A data containing the generated random noise dimensions.

## Examples

``` r
set.seed(20240412)
df <- gen_scurve(n = 500) |> as.matrix()
#> ✔ Data generation completed successfully!!!
gen_wavydims3(n = 500, p = 3, data = df)
#> ✔ Wavy shaped noise dimensions generation completed successfully!!!
#> # A tibble: 500 × 3
#>        x1    x2      x3
#>     <dbl> <dbl>   <dbl>
#>  1 -0.627 0.795 -1.78  
#>  2  0.602 1.48  -0.217 
#>  3 -0.780 0.346  1.64  
#>  4 -0.968 1.19   0.791 
#>  5  0.607 1.61   1.79  
#>  6  0.237 0.703 -1.97  
#>  7 -0.589 1.38  -1.80  
#>  8 -0.325 0.476  0.0497
#>  9  0.978 0.735 -0.809 
#> 10  0.532 1.85  -0.156 
#> # ℹ 490 more rows
```
