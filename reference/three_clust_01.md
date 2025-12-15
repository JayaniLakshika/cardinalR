# Three-Cluster Dataset in 4-D

The \`three_clust_01\` dataset contains three distinct clusters in a 4-D
space.

## Usage

``` r
data(three_clust_01)
```

## Format

A data frame with 1500 rows and 5 columns:

- cluster, x1, x2, x3, x4:

  High-dimensional coordinates

## Source

This dataset is generated for example purposes.

## Examples

``` r
# Load the mobiusgau dataset
data(three_clust_01)

# Display the first few rows of the dataset
head(three_clust_01)
#> # A tibble: 6 × 5
#>       x1      x2     x3     x4 cluster 
#>    <dbl>   <dbl>  <dbl>  <dbl> <chr>   
#> 1  1.25   0.154   0.557 -0.419 cluster2
#> 2 -0.586 -0.806  -0.151  0.303 cluster1
#> 3  0.197  0.404  -0.176  0.186 cluster1
#> 4  1.35  -0.226   0.643  0.862 cluster2
#> 5  1.05   0.0521  0.401 -0.172 cluster2
#> 6  0.434  0.668   0.394 -0.394 cluster1
```
