# Generate Gaussian cluster with the Mobius Cluster

This function generates a dataset consisting of a mobius cluster and
Gaussian cluster.

## Usage

``` r
make_mobiusgau(n = c(200, 100), p = 4)
```

## Arguments

- n:

  A numeric vector (default: c(200, 100)) representing the sample sizes.

- p:

  A numeric value (default: 4) representing the number of dimensions.

## Value

A data containing the mobius cluster and Gaussian cluster.

## Examples

``` r
mobgau <- make_mobiusgau(n = c(200, 100), p = 4)
#> ✔ Data generation completed successfully!!!
#> ✔ Data generation completed successfully!!!
#> ✔ 1 noise dimensions have been generated successfully!!!
#> ✔ Multiple clusters generation completed successfully!!!
```
