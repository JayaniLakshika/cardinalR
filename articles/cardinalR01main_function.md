# Quick start

This quick-start guide demonstrates how to generate multi-cluster
high-dimensional data. We simulate three distinct $4\text{-}D$ clusters
with different shapes, scales, and rotations.

``` r
library(cardinalR)
library(langevitour)
```

Each cluster can be rotated in a different way across specified
$2\text{-}D$ planes.

``` r
rot1 <- gen_rotation(p = 4, planes_angles = list(list(plane = c(1, 2), angle = 60),
                                                list(plane = c(3, 4), angle = 90)))
rot2 <- gen_rotation(p = 4, planes_angles = list(list(plane = c(1, 3), angle = 30)))
rot3 <- gen_rotation(p = 4, planes_angles = list(list(plane = c(2, 4), angle = 45)))
```

We use
[`gen_multicluster()`](https://jayanilakshika.github.io/cardinalR/reference/gen_multicluster.md)
to generate 3 clusters with varying shapes and positions in $4\text{-}D$
space.

``` r
clust_data <- gen_multicluster(n = c(200, 300, 500), k = 3,
  loc = matrix(c(
    0, 0, 0, 0,
    5, 9, 0, 0,
    3, 4, 10, 7
  ), nrow = 3, byrow = TRUE),
  scale = c(2, 5, 1),
  shape = c("gaussian", "cone", "unifcube"),
  rotation = list(rot1, rot2, rot3),
  add_bkg = FALSE
)

langevitour(clust_data |> dplyr::select(-cluster), 
            pointSize = 2, group = clust_data$cluster)
```
