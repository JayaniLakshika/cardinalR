# Generate Small Spheres Within a Big Sphere

This function generates a dataset representing a structure with a small
and big spheres.

## Usage

``` r
gen_clusteredspheres(n = c(1000, 100), k = 3, r = c(15, 3), loc = 10/sqrt(3))
```

## Arguments

- n:

  A numeric vector (default: c(1000, 100)) representing the sample sizes
  of the big and small spheres respectively.

- k:

  A numeric value (default: 3) representing the number of small spheres.

- r:

  A numeric vector (default: c(15, 3)) representing the radius of the
  big and small spheres respectively.

- loc:

  A numeric value (default: 10 / sqrt(3) representing how far the small
  spheres are placed from each other.

## Value

A data containing small spheres within a big sphere.

## Examples

``` r
set.seed(20240412)
clusteredspheres <- gen_clusteredspheres(n = c(1000, 100), k = 3,
r = c(15, 3), loc = 10 / sqrt(3))
#> ✔ Data generation completed successfully!!!
#> ✔ Data generation completed successfully!!!
#> ✔ Data generation completed successfully!!!
```
