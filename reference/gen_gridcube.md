# Generate Cube with grid points

This function generates a grid dataset with specified grid points along
each axes.

## Usage

``` r
gen_gridcube(n = 500, p = 4)
```

## Arguments

- n:

  An integer vector (default: 500) representing the sample size.

- p:

  An integer value (default: 4) representing the number of dimensions.

## Value

A tibble containing the cube with grid points.

## Examples

``` r
set.seed(20240412)
gridcube <- gen_gridcube(n = 500, p = 4)
#> ✔ Data generation completed successfully!!!
```
