# Generate Two Overlapping Grid Clusters in High Dimensions

This function generates a dataset consisting of two overlapping
grid-like clusters in a 2D space, with optional noise dimensions added
to reach higher-dimensional spaces. The overlap is controlled by scaling
factors for the grids.

## Usage

``` r
make_twogrid_overlap(n = c(500, 500))
```

## Arguments

- n:

  A numeric vector of length 2 specifying the number of points in each
  grid cluster.

## Value

A tibble with `n[1] + n[2]` rows and 5 columns:

- `x1,x2, x3, x4` — coordinates of the generated points.

- `cluster` — cluster membership label.

## Examples

``` r
# Generate two overlapping grid clusters in 4-D
df <- make_twogrid_overlap()
#> ✔ Data generation completed successfully!!!
#> ✔ Data generation completed successfully!!!
#> ✔ Multiple clusters generation completed successfully!!!
#> ✔ 2 noise dimensions have been generated successfully!!!
```
