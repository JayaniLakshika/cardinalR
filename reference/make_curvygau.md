# Generate Curvy Quadratic and Gaussian Clusters

This function generates synthetic high-dimensional data containing two
clusters: one quadratic-shaped cluster and one Gaussian-shaped cluster.
The clusters are positioned apart in feature space with different
scaling factors.

## Usage

``` r
make_curvygau(n = c(200, 100), p = 4)
```

## Arguments

- n:

  A numeric vector of length 2, specifying the number of observations in
  each cluster. All values must be positive.

- p:

  Integer. Number of dimensions. Must be at least 3.

## Value

A tibble containing \\n\[1\] + n\[2\]\\ rows and \\p\\ columns, with
generated features (`x1, x2, ..., xp`) and a `cluster` label.

## Examples

``` r
# Generate 2 clusters in 4D: one quadratic, one Gaussian
curvygau <- make_curvygau()
#> ✔ Data generation completed successfully!!!
#> ✔ Data generation completed successfully!!!
#> ✔ 2 noise dimensions have been generated successfully!!!
#> ✔ Multiple clusters generation completed successfully!!!
```
