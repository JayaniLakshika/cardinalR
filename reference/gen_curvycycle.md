# Generate Curvy Cell Cycle in p-d

This function generates a dataset representing a structure with a curvy
cell cycle.

## Usage

``` r
gen_curvycycle(n = 500, p = 4)
```

## Arguments

- n:

  A numeric value (default: 500) representing the sample size.

- p:

  A numeric value (default: 4) representing the number of dimensions.

## Value

A data containing a curvy cell cycle.

## Examples

``` r
set.seed(20240412)
curvycycle <- gen_curvycycle(n = 500, p = 4)
#> ✔ Data generation completed successfully!!!
```
