# Generate Small Spheres Within a Big Sphere

This function generates a dataset representing a structure with a small
and big spheres.

## Usage

``` r
gen_clusteredspheres(
  n_vec = c(1000, 100),
  k_small = 3,
  r_vec = c(15, 3),
  sep = 10/sqrt(3)
)
```

## Arguments

- n_vec:

  A numeric vector (default: c(1000, 100)) representing the sample sizes
  of the big and small spheres respectively.

- k_small:

  A numeric value (default: 3) representing the number of small spheres.

- r_vec:

  A numeric vector (default: c(15, 3)) representing the radius of the
  big and small spheres respectively.

- sep:

  A numeric value (default: 10 / sqrt(3) representing how far the small
  spheres are placed from each other.

## Value

A data containing small spheres within a big sphere.

## Examples

``` r
set.seed(20240412)
clusteredspheres <- gen_clusteredspheres(n_vec = c(1000, 100), k_small = 3,
r_vec = c(15, 3), sep = 10 / sqrt(3))
#> ✔ Data generation completed successfully!!!
#> ✔ Data generation completed successfully!!!
#> ✔ Data generation completed successfully!!!
```
