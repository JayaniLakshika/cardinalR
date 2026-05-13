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
#>        x1      x2        x3      x4
#>     <dbl>   <dbl>     <dbl>   <dbl>
#>  1  0.489  0.0533 -0.000606  0.0217
#>  2  0.387 -0.783  -0.0771    0.0129
#>  3 -0.538  0.538   0.0467    0.0134
#>  4 -0.194  0.690  -0.0267   -0.0222
#>  5 -0.540 -0.671  -0.245    -0.0396
#>  6 -0.242 -0.615   0.120     0.0306
#>  7 -0.239  0.583  -0.160     0.0226
#>  8 -0.451  0.599   0.0189   -0.0311
#>  9  0.199 -0.764  -0.0330   -0.0428
#> 10  0.329  0.381  -0.0947    0.0290
#> # ℹ 990 more rows
```
