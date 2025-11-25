# Generate Rotations

This function generates a rotation matrix.

## Usage

``` r
gen_rotation(p = 4, planes_angles)
```

## Arguments

- p:

  A numeric value (default: 4) representing the number of dimensions.

- planes_angles:

  A numeric list which contains plane and the corresponding angle along
  that plane.

## Value

A matrix containing the rotations.

## Examples

``` r
set.seed(20240412)
rotations_4d <- list(
  list(plane = c(1, 2), angle = 60), # Rotation in the (1, 2) plane
  list(plane = c(3, 4), angle = 90)  # Rotation in the (3, 4) plane
)
gen_rotation(p = 4, planes_angles = rotations_4d)
#>           [,1]       [,2]         [,3]          [,4]
#> [1,] 0.5000000 -0.8660254 0.000000e+00  0.000000e+00
#> [2,] 0.8660254  0.5000000 0.000000e+00  0.000000e+00
#> [3,] 0.0000000  0.0000000 6.123234e-17 -1.000000e+00
#> [4,] 0.0000000  0.0000000 1.000000e+00  6.123234e-17
```
