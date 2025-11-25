# Generate Curvy Cylinder

This function generates a dataset representing a structure with a curvy
cylinder.

## Usage

``` r
gen_curvycylinder(n = 500, h = 10)
```

## Arguments

- n:

  A numeric value (default: 500) representing the sample size.

- h:

  A numeric value (default: 10) representing the height of the cylinder.

## Value

A data containing a curvy cylinder.

## Examples

``` r
set.seed(20240412)
curvycylinder <- gen_curvycylinder(n = 500, h = 10)
#> ✔ Data generation completed successfully!!!
```
