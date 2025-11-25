# Generate Cube with Hole

This function generates a dataset representing a cube with a hole.

## Usage

``` r
gen_unifcubehole(n = 5000, p = 4, r_hole = 0.5)
```

## Arguments

- n:

  A numeric value (default: 500) representing the sample size.

- p:

  A numeric value (default: 4) representing the number of dimensions.

- r_hole:

  A numeric value (default: 0.5) representing the radius of the hole.

## Value

A data containing the cube data with a hole.

## Examples

``` r
set.seed(20240412)
cubehole <- gen_unifcubehole(n = 1000, p = 4)
#> ✔ Data generation completed successfully!!!
#> ✔ Data generation completed successfully!!!
```
