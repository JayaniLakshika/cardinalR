# Generate Cube with grid points

This function generates a grid dataset with specified grid points along
each axes.

## Usage

``` r
gen_gridcube(n = 500, p = 4)
```

## Arguments

- n:

  A numeric vector (default: 500) representing the sample size.

- p:

  A numeric value (default: 4) representing the number of dimensions.

## Value

A data containing the cube with grid points.

## Examples

``` r
set.seed(20240412)
gridcube <- gen_gridcube(n = 500, p = 4)
#> ✔ Data generation completed successfully!!!
```
