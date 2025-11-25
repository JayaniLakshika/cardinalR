# Generate Uniform Sphere

This function generates a dataset representing a structure with a
uniform sphere.

## Usage

``` r
gen_unifsphere(n = 500, r = 1)
```

## Arguments

- n:

  A numeric value (default: 500) representing the sample size.

- r:

  A numeric vector (default: 1) representing the radius of the sphere.

## Value

A data containing a uniform sphere.

## Examples

``` r
set.seed(20240412)
unifsphere <- gen_unifsphere(n = 500)
#> ✔ Data generation completed successfully!!!
```
