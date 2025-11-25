# Randomize Rows of a Data Frame

This function randomly shuffles the rows of a given data frame.

## Usage

``` r
randomize_rows(data)
```

## Arguments

- data:

  A data frame to be randomized.

## Value

A data frame with rows randomly shuffled.

## Examples

``` r
randomize_rows(mobiusgau)
#> # A tibble: 1,000 × 4
#>        x1     x2      x3       x4
#>     <dbl>  <dbl>   <dbl>    <dbl>
#>  1 -0.538  0.538  0.0467  0.0134 
#>  2 -0.194  0.690 -0.0267 -0.0222 
#>  3 -0.540 -0.671 -0.245  -0.0396 
#>  4 -0.242 -0.615  0.120   0.0306 
#>  5 -0.239  0.583 -0.160   0.0226 
#>  6 -0.451  0.599  0.0189 -0.0311 
#>  7  0.199 -0.764 -0.0330 -0.0428 
#>  8  0.329  0.381 -0.0947  0.0290 
#>  9 -0.115  0.903  0.198   0.00934
#> 10 -0.596  0.367 -0.152   0.0207 
#> # ℹ 990 more rows
```
