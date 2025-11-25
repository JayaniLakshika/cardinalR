# Generate Multiple Interlocked Circles in High-Dimensional Space

This function generates \\k\\ interlocked circular clusters in a
\\p\\-dimensional space. The circles are constructed using
[`gen_multicluster()`](https://jayanilakshika.github.io/cardinalR/reference/gen_multicluster.md),
with each circle positioned in a different coordinate plane and slightly
offset so that they interlock with a central circle (hub-like
structure).

## Usage

``` r
make_klink_circles(n = c(200, 100), p = 4, k = 2, offset = 0.5)
```

## Arguments

- n:

  An integer vector of length \\k\\ giving the number of points in each
  circle. Default is `c(200, 100)`.

- p:

  Integer, the dimensionality of the embedding space. Must be at
  least 3. Default is `4`.

- k:

  Integer, the number of circles to generate. Default is `2`.

- offset:

  Numeric, the amount of positional shift applied to each circle along
  the second coordinate axis to prevent complete overlap. Default is
  `0.5`.

## Value

A data frame (or tibble, depending on
[`gen_multicluster()`](https://jayanilakshika.github.io/cardinalR/reference/gen_multicluster.md))
containing the generated points and cluster assignments.

## Examples

``` r
# Generate two interlocked circles in 4-D
twolink_circles <- make_klink_circles()
#> ✔ Data generation completed successfully!!!
#> ✔ Data generation completed successfully!!!
#> ✔ Multiple clusters generation completed successfully!!!
```
