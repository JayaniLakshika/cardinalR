# Remove points within a spherical hole in the middle

Remove points within a spherical hole in the middle

## Usage

``` r
gen_hole(df, anchor = NULL, r = 0.5)
```

## Arguments

- df:

  A tibble of coordinates.

- anchor:

  A numeric vector giving the center of the hole.

- r:

  A numeric value for the hole radius.

## Value

A tibble with the hole removed.

## Examples

``` r
set.seed(20240412)
df <- gen_scurve(n = 1000)
#> ✔ Data generation completed successfully!!!
gen_hole(df, r = 0.5)
#> # A tibble: 970 × 3
#>        x1    x2      x3
#>     <dbl> <dbl>   <dbl>
#>  1 -0.634 1.68  -1.77  
#>  2  0.610 0.171 -0.207 
#>  3 -0.773 0.252  1.63  
#>  4 -0.976 1.81   0.782 
#>  5  0.602 1.57   1.80  
#>  6  0.234 1.29  -1.97  
#>  7 -0.592 1.30  -1.81  
#>  8 -0.319 0.411  0.0522
#>  9  0.983 0.463 -0.817 
#> 10  0.527 1.49  -0.150 
#> # ℹ 960 more rows
```
