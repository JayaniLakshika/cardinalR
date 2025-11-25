# Generate Cluster Locations

This function generate locations for any number of clusters in any
dimensions.

## Usage

``` r
gen_clustloc(p = 4, k = 3)
```

## Arguments

- p:

  A numeric value (default: 4) representing the number of dimensions.

- k:

  A numeric value (default: 3) representing the number of clusters.

## Value

A matrix of the locations.

## Examples

``` r
set.seed(20240412)
gen_clustloc(p = 4, k = 3)
#>            [,1]       [,2]        [,3]
#> [1,]  1.8727238 -0.7444274 -1.12829641
#> [2,] -0.9968225  0.9259866  0.07083593
#> [3,]  0.7820890 -0.1252220 -0.65686699
#> [4,]  1.5232486 -0.4366755 -1.08657315
```
