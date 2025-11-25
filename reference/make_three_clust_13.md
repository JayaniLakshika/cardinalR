# Generate a Three-Cluster Dataset in High Dimensions

This function generates a dataset consisting of three distinct clusters
in a 4-dimensional space. Each cluster is generated with a specified
shape and location using the underlying \`gen_multicluster()\` function.

## Usage

``` r
make_three_clust_13(n = c(700, 300, 500))
```

## Arguments

- n:

  An integer vector of length 3 specifying the number of points in each
  cluster. Default is `c(700, 300, 500)`.

## Value

A data frame (or tibble, depending on \`gen_multicluster()\`) containing
the generated dataset with cluster assignments.

## Examples

``` r
# Generate default three clusters in 4-D
three_clust_13 <- make_three_clust_13()
#> ✔ Data generation completed successfully!!!
#> ✔ Data generation completed successfully!!!
#> ✔ Data generation completed successfully!!!
#> ✔ Multiple clusters generation completed successfully!!!
```
