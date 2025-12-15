# Three-Cluster Dataset in 4-D

The \`three_clust_04\` dataset contains three distinct clusters in a 4-D
space.

## Usage

``` r
data(three_clust_04)
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
data(three_clust_04)

# Display the first few rows of the dataset
head(three_clust_04)
#> # A tibble: 6 × 5
#>       x1     x2     x3     x4 cluster 
#>    <dbl>  <dbl>  <dbl>  <dbl> <chr>   
#> 1  0.600 -0.630 -1.44   0.435 cluster1
#> 2  0.546  1.33   2.19   1.63  cluster3
#> 3  0.614  0.586  2.24   1.73  cluster3
#> 4 -0.791 -0.704 -0.595  0.124 cluster1
#> 5  0.505  0.778  0.719 -0.394 cluster1
#> 6  0.722  0.842  2.96   1.64  cluster3
```
