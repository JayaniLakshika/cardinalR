# Generate Circle in p-d

This function generates a dataset representing a structure with a
circle.

## Usage

``` r
gen_circle(n = 500, p = 4)
```

## Arguments

- n:

  An integer value (default: 500) representing the sample size.

- p:

  An integer value (default: 4) representing the number of dimensions.

## Value

A data containing a circle.

## Examples

``` r
set.seed(20240412)
circle <- gen_circle(n = 500, p = 4)
#> ✔ Data generation completed successfully!!!
```
