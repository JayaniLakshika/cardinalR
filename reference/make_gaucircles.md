# Generate Concentric Circles with a Gaussian Cluster in High Dimensions

This function generates a dataset consisting of multiple circular
clusters together with a single Gaussian cluster in a \\p\\-dimensional
space. The circles are placed concentrically at the origin with varying
scales, while the Gaussian cluster serves as an additional background or
center cluster.

## Usage

``` r
make_gaucircles(
  n = c(200, 100, 100),
  p = 4,
  num_circles = 2,
  scale_circles = c(1, 2)
)
```

## Arguments

- n:

  An integer vector of length `num_circles + 1`, giving the number of
  points in each cluster (circles first, followed by the Gaussian).
  Default is `c(200, 100, 100)`.

- p:

  Integer, the dimensionality of the embedding space. Must be at
  least 3. Default is `4`.

- num_circles:

  Integer, the number of circular clusters to generate. Default is `2`.

- scale_circles:

  Numeric vector of length `num_circles`, giving the scale (radius) of
  each circular cluster. Default is `c(1, 2)`.

## Value

A data frame (or tibble, depending on
[`gen_multicluster()`](https://jayanilakshika.github.io/cardinalR/reference/gen_multicluster.md))
containing the generated dataset with cluster assignments.

## Examples

``` r
# Two circles (radii 1 and 2) plus one Gaussian cluster in 4-D
gaucircles <- make_gaucircles()
#> ✔ Data generation completed successfully!!!
#> ✔ Data generation completed successfully!!!
#> ✔ Data generation completed successfully!!!
#> ✔ Multiple clusters generation completed successfully!!!

```
