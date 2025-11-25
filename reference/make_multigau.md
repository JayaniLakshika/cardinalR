# Generate Multiple Gaussian Clusters

This function generates a dataset consisting of multiple Gaussian
clusters.

## Usage

``` r
make_multigau(n = c(300, 200, 500), p = 4, k = 3, loc = NULL, scale = NULL)
```

## Arguments

- n:

  A numeric vector (default: c(300, 200, 500)) representing the sample
  sizes.

- p:

  A numeric value (default: 4) representing the number of dimensions.

- k:

  A numeric value (default: 5) representing the number of clusters.

- loc:

  A numeric matrix (default: NULL) representing the locations/centroids
  of clusters.

- scale:

  A numeric vector (default: NULL) representing the scaling factors of
  clusters.

## Value

A data containing the Gaussian clusters.

## Examples

``` r
loc_matrix <- matrix(c(0, 0, 0, 0,
5, 9, 0, 0,
3, 4, 10, 7
), nrow = 3, byrow = TRUE)
multigau <- make_multigau(n = c(300, 200, 500),
p = 4, k = 3,
loc = loc_matrix, scale = c(0.2, 1.5, 0.5))
#> ✔ Data generation completed successfully!!!
#> ✔ Data generation completed successfully!!!
#> ✔ Data generation completed successfully!!!
#> ✔ Multiple clusters generation completed successfully!!!
```
