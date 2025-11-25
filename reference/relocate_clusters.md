# Relocate Clusters in High-Dimensional Space

This function relocates clusters in a dataset by centering each cluster
and shifting it based on a given transformation matrix.

## Usage

``` r
relocate_clusters(data, vert_mat)
```

## Arguments

- data:

  A tibble or data frame containing clustered data. It must have a
  \`cluster\` column indicating cluster membership.

- vert_mat:

  A matrix specifying the translation vectors for each cluster. The
  number of rows must match the number of clusters.

## Value

A tibble containing the relocated clusters with randomized row order.

## Examples

``` r
set.seed(20240412)
df <- tibble::tibble(
x1 = rnorm(12),
x2 = rnorm(12),
x3 = rnorm(12),
x4 = rnorm(12),
cluster = rep(1:3, each = 4)
)

# Create a 3x4 matrix to define new cluster centers
vert_mat <- matrix(c(
  5, 0, 0, 0,   # Shift cluster 1
  0, 5, 0, 0,   # Shift cluster 2
  0, 0, 5, 0    # Shift cluster 3
), nrow = 3, byrow = TRUE)
# Apply relocation
relocated_df <- relocate_clusters(df, vert_mat)
```
