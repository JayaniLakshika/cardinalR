# Three-Cluster Dataset in 4-D

The \`three_clust_03\` dataset contains three distinct clusters in a 4-D
space.

## Usage

``` r
data(three_clust_03)
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
data(three_clust_03)

# Display the first few rows of the dataset
head(three_clust_03)
#> # A tibble: 6 × 5
#>      x1    x2    x3       x4 cluster 
#>   <dbl> <dbl> <dbl>    <dbl> <chr>   
#> 1 0.109 0.134 0.845 -0.00255 cluster1
#> 2 1.15  0.899 2.23   1.58    cluster3
#> 3 0.353 0.863 2.39   1.48    cluster3
#> 4 0.266 0.872 2.47   1.70    cluster3
#> 5 0.750 1.07  2.11   1.54    cluster3
#> 6 1.33  0.107 0.494  0.0838  cluster2
```
