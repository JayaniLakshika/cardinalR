# Generate Rectangular Based Pyramid

This function generates a dataset representing a rectangular based
pyramid.

## Usage

``` r
gen_pyrrect(n = 500, p = 4, h = 5, l_vec = c(3, 2), rt = 0.5)
```

## Arguments

- n:

  A numeric value (default: 500) representing the sample size.

- p:

  A numeric value (default: 4) representing the number of dimensions.

- h:

  A numeric value (default: 5) representing the height of the pyramid.

- l_vec:

  A numeric vector (default: c(3, 2)) representing the base lengths
  along the and y of the pyramid.

- rt:

  A numeric value (default: 0.5) representing the tip radius of the
  pyramid.

## Value

A data containing the rectangular based pyramid.

## Examples

``` r
set.seed(20240412)
pyrrect <- gen_pyrrect(n = 500, p = 4, h = 5, l_vec = c(3, 2), rt = 0.5)
#> ✔ Data generation completed successfully!!!
```
