# Example data structures

``` r
library(cardinalR)
library(langevitour)
```

This illustrates a variety of example data structures that can be
generated. These synthetic datasets represent common and challenging
shapes found.

## Gaussian

A simple spherical Gaussian cluster with very small variance in
$4\text{-}D$ space.

``` r
gau_data <- gen_gaussian(n = 500, p = 4, s = diag(4) * 0.01)

langevitour(gau_data, pointSize = 2)
```

## Cube with a hole

Uniform points in a hypercube with a hollow center, useful for detecting
voids or gaps.

``` r
cube_grd_data <- gen_unifcubehole(n = 5000, p = 6)

langevitour(cube_grd_data, pointSize = 2)
```

## Circular

A curvilinear loop structure that forms a closed cycle in $4\text{-}D$.

``` r
curvy_cyc_data <- gen_curvycycle(n = 500, p = 4) 

langevitour(curvy_cyc_data, pointSize = 2)
```

## Branching

A tree-like curvilinear structure with six branches in $6\text{-}D$,
ideal for studying bifurcation patterns.

``` r
tree_data <- gen_orgcurvybranches(n = 600, p = 5, k = 6) 

langevitour(tree_data, pointSize = 2)
```

## Cone

A pointed cone shape in $4\text{-}D$, controlled by height and radius
ratio.

``` r
cone_data <- gen_cone(n = 500, h = 5, ratio = 0.5) 

langevitour(cone_data, pointSize = 2)
```

## Conic spiral

A spiral winding around a conical surface in $5\text{-}D$ space.

``` r
spiral_data <- gen_conicspiral(n = 500, spins = 2)

langevitour(spiral_data, pointSize = 2)
```

## Grided sphere

A regularly spaced sphere with high point density, useful for manifold
learning.

``` r
sphere_data <- gen_gridedsphere(n = 500, p = 4)

langevitour(sphere_data, pointSize = 2)
```

## S-curve with a hole

An S-shaped manifold in $8\text{-}D$ with a missing section, to evaluate
resilience to structural gaps.

``` r
scurve_data <- gen_scurvehole(n = 600) 

langevitour(scurve_data, pointSize = 2)
```

## Pyramid with a hole

A $4\text{-}D$ pyramid-shaped structure with a void at the center.

``` r
pyr_data <- gen_pyrfrac(n = 1000, p = 4)

langevitour(pyr_data, pointSize = 2)
```
