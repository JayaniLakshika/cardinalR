# Generate Two Shifted Grid Clusters in High Dimensions

This function generates two grid-shaped clusters in a 2D space, where
one grid is shifted relative to the other. Optionally, additional noise
dimensions can be added to embed the structure in a higher-dimensional
space.

## Usage

``` r
make_twogrid_shift(n = c(500, 500))
```

## Arguments

- n:

  A numeric vector of length 2 specifying the number of points in each
  cluster. Default is `c(500, 500)`.

## Value

A tibble with `n[1] + n[2]` rows and 5 columns:

- `x1, x2, x3, x4`: Numeric coordinates of the points.

- `cluster`: Cluster membership label (factor with 2 levels).

## Examples

``` r
# Generate 2 shifted grid clusters in 4-D
make_twogrid_shift <- make_twogrid_shift()
#> ✔ Data generation completed successfully!!!
#> ✔ Data generation completed successfully!!!
#> ✔ Multiple clusters generation completed successfully!!!
#> ✔ 2 noise dimensions have been generated successfully!!!

```
