# Generate Multiple Interlocked curvycycle in High-Dimensional Space

This function generates \\k\\ interlocked circular clusters in a
\\p\\-dimensional space. The curvycycle are constructed using
[`gen_multicluster()`](https://jayanilakshika.github.io/cardinalR/reference/gen_multicluster.md),
with each curvycycle positioned in a different coordinate plane and
slightly offset so that they interlock with a central curvycycle
(hub-like structure).

## Usage

``` r
make_klink_curvycycle(n = c(200, 100), p = 4, k = 2, offset = 0.5)
```

## Arguments

- n:

  An integer vector of length \\k\\ giving the number of points in each
  curvycycle. Default is `c(200, 100)`.

- p:

  Integer, the dimensionality of the embedding space. Must be at
  least 3. Default is `4`.

- k:

  Integer, the number of curvycycle to generate. Default is `2`.

- offset:

  Numeric, the amount of positional shift applied to each curvycycle
  along the second coordinate axis to prevent complete overlap. Default
  is `0.5`.

## Value

A data frame (or tibble, depending on
[`gen_multicluster()`](https://jayanilakshika.github.io/cardinalR/reference/gen_multicluster.md))
containing the generated points and cluster assignments.

## Examples

``` r
# Generate two interlocked curvycycle in 4-D
twolink_curvycycle <- make_klink_curvycycle()
#> ✔ Data generation completed successfully!!!
#> ✔ Data generation completed successfully!!!
#> ✔ Multiple clusters generation completed successfully!!!
```
