# Generate Triangular Based Pyramid

This function generates a dataset representing a triangular based
pyramid.

## Usage

``` r
gen_pyrtri(n = 500, p = 4, h = 5, l = 3, rt = 0.5)
```

## Arguments

- n:

  A numeric value (default: 500) representing the sample size.

- p:

  A numeric value (default: 4) representing the number of dimensions.

- h:

  A numeric value (default: 5) representing the height of the pyramid.

- l:

  A numeric value (default: 3) representing the base length of the
  pyramid.

- rt:

  A numeric value (default: 0.5) representing the tip radius of the
  pyramid.

## Value

A data containing the triangular based pyramid.

## Examples

``` r
set.seed(20240412)
pyrtri <- gen_pyrtri(n = 500, p = 4, h = 5, l = 3, rt = 0.5)
#> ✔ Data generation completed successfully!!!
```
