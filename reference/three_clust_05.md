# Three-Cluster Dataset in 4-D

The \`three_clust_05\` dataset contains three distinct clusters in a 4-D
space.

## Usage

``` r
data(three_clust_05)
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
data(three_clust_05)

# Display the first few rows of the dataset
head(three_clust_05)
#> # A tibble: 6 × 5
#>       x1     x2     x3      x4 cluster 
#>    <dbl>  <dbl>  <dbl>   <dbl> <chr>   
#> 1  1.00   1.21   2.55   1.78   cluster3
#> 2  0.724  0.792  0.156 -0.0194 cluster1
#> 3  1.07  -0.166  0.557 -0.104  cluster1
#> 4  0.941 -0.495  0.282 -0.0836 cluster1
#> 5  0.725  1.18   2.79   1.78   cluster3
#> 6 -0.901 -0.143 -0.304  0.0513 cluster1
```
