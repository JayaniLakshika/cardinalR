# Additional functions

``` r
library(cardinalR)
```

These are helper functions included in the package.

## Generating background noise

The
[`gen_bkgnoise()`](https://jayanilakshika.github.io/cardinalR/reference/gen_bkgnoise.md)
function allows users to generate multivariate Gaussian noise to serve
as background data in high-dimensional spaces.

``` r
# Example: Generate 4D background noise
bkg_data <- gen_bkgnoise(n = 500, p = 4, 
                         m = c(0, 0, 0, 0), s = c(2, 2, 2, 2))
head(bkg_data)
#> # A tibble: 6 × 4
#>        x1      x2     x3     x4
#>     <dbl>   <dbl>  <dbl>  <dbl>
#> 1 -2.80   -1.42   -0.673 -1.16 
#> 2  0.511   1.32   -0.432 -0.338
#> 3 -4.87    0.582   1.24  -3.84 
#> 4 -0.0111  0.396  -2.57  -3.07 
#> 5  1.24   -2.41   -2.60  -2.23 
#> 6  2.30   -0.0796 -0.754  3.20
```

The generated data has independent dimensions with specified means (`m`)
and standard deviations (`s`).

## Randomizing rows

[`randomize_rows()`](https://jayanilakshika.github.io/cardinalR/reference/randomize_rows.md)
ensures the rows of the input data is randomized.

``` r
randomized_data <- randomize_rows(bkg_data)
head(randomized_data)
#> # A tibble: 6 × 4
#>       x1     x2      x3     x4
#>    <dbl>  <dbl>   <dbl>  <dbl>
#> 1 -1.84  -0.905  1.11   -1.04 
#> 2  2.36  -3.49  -2.98   -0.247
#> 3 -0.492  2.86   0.381  -2.50 
#> 4 -0.424 -3.13  -3.56   -0.177
#> 5  3.79   1.10   0.0810 -0.861
#> 6  1.02  -0.397  3.29    3.21
```

## Relocating clusters

[`relocate_clusters()`](https://jayanilakshika.github.io/cardinalR/reference/relocate_clusters.md)
allows users to translate clusters in any dimension(s). This is achieved
by centering each cluster (subtracting its mean) and then adding a
translation vector from a provided matrix (`vert_mat`).

``` r
df <- tibble::tibble(
  x1 = rnorm(12),
  x2 = rnorm(12),
  x3 = rnorm(12),
  x4 = rnorm(12),
  cluster = rep(1:3, each = 4)
)

vert_mat <- matrix(c(
  5, 0, 0, 0,
  0, 5, 0, 0,
  0, 0, 5, 0
), nrow = 3, byrow = TRUE)

relocated_df <- relocate_clusters(df, vert_mat)
head(relocated_df)
#> # A tibble: 6 × 5
#>        x1     x2      x3     x4 cluster
#>     <dbl>  <dbl>   <dbl>  <dbl>   <int>
#> 1  5.86   -0.435 -0.339   0.243       1
#> 2 -0.794   5.35  -0.0382  0.133       2
#> 3  1.01   -0.652  5.06   -0.592       3
#> 4  2.93    0.728  0.218  -1.56        1
#> 5  0.821   4.67  -0.266  -0.773       2
#> 6  0.0601  4.58  -0.269   1.40        2
```

## Generating Rotation Matrices

The
[`gen_rotation()`](https://jayanilakshika.github.io/cardinalR/reference/gen_rotation.md)
function creates a rotation matrix in high-dimensional space for given
planes and angles.

``` r

rotations_4d <- list(
  list(plane = c(1, 2), angle = 60),
  list(plane = c(3, 4), angle = 90)
)

rot_mat <- gen_rotation(p = 4, planes_angles = rotations_4d)
rot_mat
#>           [,1]       [,2]         [,3]          [,4]
#> [1,] 0.5000000 -0.8660254 0.000000e+00  0.000000e+00
#> [2,] 0.8660254  0.5000000 0.000000e+00  0.000000e+00
#> [3,] 0.0000000  0.0000000 6.123234e-17 -1.000000e+00
#> [4,] 0.0000000  0.0000000 1.000000e+00  6.123234e-17
```

## Normalize data

When combining clusters or transforming data geometrically, magnitudes
can differ drastically. The
[`normalize_data()`](https://jayanilakshika.github.io/cardinalR/reference/normalize_data.md)
function rescales the entire dataset to fit within (\[-1, 1\]) based on
its maximum absolute value.

``` r
norm_data <- normalize_data(bkg_data)
head(norm_data)
#>             x1          x2          x3          x4
#> 1 -0.450293102 -0.22884604 -0.10829694 -0.18606977
#> 2  0.082117097  0.21297520 -0.06942023 -0.05439019
#> 3 -0.783892057  0.09363561  0.19977349 -0.61727879
#> 4 -0.001791881  0.06366882 -0.41297879 -0.49346293
#> 5  0.199908717 -0.38710048 -0.41814604 -0.35853835
#> 6  0.369361251 -0.01280627 -0.12117958  0.51390085
```

## Generating cluster locations

To place clusters in different positions,
[`gen_clustloc()`](https://jayanilakshika.github.io/cardinalR/reference/gen_clustloc.md)
generates points forming a **simplex-like arrangement** ensuring each
cluster center is equidistant from others as much as possible.

``` r

centers <- gen_clustloc(p = 4, k = 5)
head(centers)
#>            [,1]       [,2]        [,3]       [,4]      [,5]
#> [1,]  0.9586298 -0.7847655 -0.67325802 -0.5253323 1.0247260
#> [2,]  0.2409763 -0.2831468 -1.86798385 -0.5513311 2.4614855
#> [3,] -0.3976958  1.2621977 -0.43202643 -0.5647853 0.1323098
#> [4,]  0.1294284 -0.6167717  0.01451185  0.3221455 0.1506859
```

## Numeric generators

Two helper functions,
[`gen_nproduct()`](https://jayanilakshika.github.io/cardinalR/reference/gen_nproduct.md)
and
[`gen_nsum()`](https://jayanilakshika.github.io/cardinalR/reference/gen_nsum.md),
generate numeric vectors of positive integers that approximately satisfy
a user-specified target product or sum, respectively.

The function `gen_nsum(n, k)` divides a total sum `n` into `k` positive
integers. It first assigns an equal base value to each element and then
randomly distributes any remainder, ensuring the elements sum exactly to
`n`.

``` r
gen_nsum(n = 100, k = 3)
#> [1] 34 33 33
```

The function `gen_nproduct(n, p)` aims to produce `p` positive integers
whose product is approximately `n`. It starts with all elements equal to
the rounded $p^{th}$ root of `n` and iteratively adjusts elements up or
down in a randomized manner until the product is within a small
tolerance of `n`. This accommodates the fact that exact integer
solutions for a given product are often impossible.

``` r
gen_nproduct(n = 500, p = 4)
#> [1] 4 5 5 5
```
