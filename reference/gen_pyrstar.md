# Generate Star Based Pyramid

This function generates a dataset representing a star based pyramid.

## Usage

``` r
gen_pyrstar(n = 500, p = 4, h = 5, rb = 3)
```

## Arguments

- n:

  A numeric value (default: 500) representing the sample size.

- p:

  A numeric value (default: 4) representing the number of dimensions.

- h:

  A numeric value (default: 5) representing the height of the pyramid.

- rb:

  A numeric value (default: 3) representing the base radius of the
  pyramid.

## Value

A data containing the star based pyramid.

## Examples

``` r
set.seed(20240412)
pyrstar <- gen_pyrstar(n = 500, p = 4, h = 5, rb = 3)
#> ✔ Data generation completed successfully!!!
```
