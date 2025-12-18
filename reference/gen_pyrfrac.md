# Generate p-D Triangular Pyramid With Triangular Pyramid shaped holes

This function generates p-D triangular pyramid with triangular pyramid
shaped holes.

## Usage

``` r
gen_pyrfrac(n = 500, p = 4)
```

## Arguments

- n:

  An integer value (default: 500) representing the sample size.

- p:

  An integer value (default: 4) representing the number of dimensions.

## Value

A data containing a triangular pyramid with triangular pyramid shaped
holes.

## Examples

``` r
set.seed(20240412)
pyrfrac <- gen_pyrfrac(n = 500, p = 3)
#> ✔ Data generation completed successfully!!!
```
