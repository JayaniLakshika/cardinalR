# Generate Multiple Clusters

This function generates a dataset with multiple clusters in
high-dimensional space. Each cluster can have a different shape, scale,
rotation, and centroid, allowing the construction of complex synthetic
datasets.

## Usage

``` r
gen_multicluster(
  n = c(200, 300, 500),
  k = 3,
  loc = matrix(c(0, 0, 0, 0, 5, 9, 0, 0, 3, 4, 10, 7), nrow = 3, byrow = TRUE),
  scale = c(3, 1, 2),
  shape = c("gaussian", "bluntedcorn", "unifcube"),
  rotation = NULL,
  add_bkg = FALSE,
  ...
)
```

## Arguments

- n:

  A numeric vector (default: c(200, 500, 300)) representing the sample
  sizes for each cluster. Must have length `k`.

- k:

  A numeric value (default: 3) representing the number of clusters.

- loc:

  A numeric matrix giving the centroids of the clusters. The number of
  rows must equal `k`; the number of columns should match the maximum
  dimensionality across all shapes.

- scale:

  A numeric vector (default: c(3, 1, 2)) giving the scaling factors for
  each cluster. Must have length `k`.

- shape:

  A character vector (default: c("gaussian", "cone", "unifcube"))
  specifying the generator function to use for each cluster. Must have
  length `k`.

- rotation:

  A list of rotation matrices (one per cluster), or `NULL`. Each matrix
  must be square with dimension equal to the total number of structural
  dimensions in the dataset. Rotation matrices can be generated using
  [`gen_rotation`](https://jayanilakshika.github.io/cardinalR/reference/gen_rotation.md)
  for convenience.

- add_bkg:

  Logical (default: FALSE). If `TRUE`, adds background noise sampled
  from a multivariate normal distribution centered on the dataset mean
  with standard deviations matching the observed spread.

- ...:

  Additional arguments passed to the cluster generator functions.

## Value

A tibble containing all generated clusters, with columns `x1, x2, ...`
for dimensions and a `cluster` label.

## Examples

``` r
set.seed(20240412)

# Example rotation matrices for 4D space
rot1 <- gen_rotation(p = 4, planes_angles = list(list(plane = c(1, 2), angle = 60),
                                                list(plane = c(3, 4), angle = 90)))
rot2 <- gen_rotation(p = 4, planes_angles = list(list(plane = c(1, 3), angle = 30)))
rot3 <- gen_rotation(p = 4, planes_angles = list(list(plane = c(2, 4), angle = 45)))

clust_data <- gen_multicluster(
  n = c(200, 300, 500),
  k = 3,
  loc = matrix(c(
    0, 0, 0, 0,
    5, 9, 0, 0,
    3, 4, 10, 7
  ), nrow = 3, byrow = TRUE),
  scale = c(3, 1, 2),
  shape = c("gaussian", "cone", "unifcube"),
  rotation = list(rot1, rot2, rot3),
  add_bkg = FALSE
)
#> ✔ Data generation completed successfully!!!
#> ✔ Data generation completed successfully!!!
#> ✔ Data generation completed successfully!!!
#> ✔ Multiple clusters generation completed successfully!!!
```
