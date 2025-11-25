# Generate Conical Spiral

This function generates a dataset representing a conical spiral
structure.

## Usage

``` r
gen_conicspiral(n = 500, spins = 1)
```

## Arguments

- n:

  A numeric value (default: 500) representing the sample size.

- spins:

  A numeric value (default: 1) representing the number of loops of the
  spiral.

## Value

A data containing a conical spiral structure.

## Examples

``` r
set.seed(20240412)
conicspiral <- gen_conicspiral(n = 500, spins = 1)
#> ✔ Data generation completed successfully!!!
```
