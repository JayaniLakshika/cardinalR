# Three-Cluster Dataset in 4-D

The \`three_clust_02\` dataset contains three distinct clusters in a 4-D
space.

## Usage

``` r
data(three_clust_02)
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
data(three_clust_02)

# Display the first few rows of the dataset
head(three_clust_02)
#> # A tibble: 6 × 5
#>      x1      x2     x3     x4 cluster 
#>   <dbl>   <dbl>  <dbl>  <dbl> <chr>   
#> 1 0.738  0.931   2.55   1.62  cluster3
#> 2 0.776  1.17    2.53   1.76  cluster3
#> 3 1.22   0.0332  0.504 -0.227 cluster2
#> 4 0.946  1.19    2.41   1.63  cluster3
#> 5 0.709  1.97   -0.378  0.214 cluster1
#> 6 1.54  -0.0282  0.494 -0.335 cluster2
```
