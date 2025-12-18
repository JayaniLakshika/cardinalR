# Generate a 3-D Mobius

This function generates a dataset representing a structure with a
mobius.

## Usage

``` r
gen_mobius(n = 500)
```

## Arguments

- n:

  An integer value (default: 500) representing the sample size.

## Value

A tibble containing a mobius structure.

## Examples

``` r
set.seed(20240412)
mobius <- gen_mobius(n = 500)
#> ✔ Data generation completed successfully!!!
```
