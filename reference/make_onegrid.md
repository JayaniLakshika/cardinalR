# Generate a Single Grid Cluster in High Dimensions

This function generates a dataset consisting of one grid-like cluster (a
structured cube grid) in 2D, with optional Gaussian noise dimensions
added to extend the dataset into higher dimensions.

## Usage

``` r
make_onegrid(n = 500)
```

## Arguments

- n:

  Integer, the number of points in the grid cluster. Must be positive.
  Default is `500`.

## Value

A tibble containing the generated dataset with columns:

- `x1, x2, x3, x4` — coordinates of the data points.

- `cluster` — cluster assignment (always 1 for the grid).

## Examples

``` r
# Default: 500 points, 4D space (grid in 2D + 2 noise dimensions)
onegrid <- make_onegrid()
#> ✔ Data generation completed successfully!!!
#> ✔ Multiple clusters generation completed successfully!!!
#> ✔ 2 noise dimensions have been generated successfully!!!

```
