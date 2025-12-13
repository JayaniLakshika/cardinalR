# Generate Blunted Cone

This function generates a dataset representing a cone with the option of
a sharp or blunted apex.

## Usage

``` r
gen_cone(n = 500, p = 4, h = 5, ratio = 0.5)
```

## Arguments

- n:

  A numeric value (default: 500) representing the sample size.

- p:

  A numeric value (default: 4) representing the number of dimensions.

- h:

  A numeric value (default: 5) representing the height of the cone.

- ratio:

  A numeric value (default: 0.5) representing the radius tip to radius
  base ratio of the cone. Should be less than 1.

## Value

A data containing the cone with the option of a sharp or blunted apex.

## Examples

``` r
set.seed(20240412)
cone <- gen_cone(n = 500, p = 4, h = 5, ratio = 0.5)
#> ✔ Data generation completed successfully!!!
```
