# Generate a Chain of Interlocked Circles in High-Dimensional Space

This function generates \\k\\ interlocked circular clusters in a
\\p\\-dimensional space. Unlike
[`make_klink_circles()`](https://jayanilakshika.github.io/cardinalR/reference/make_klink_circles.md),
the circles are arranged in a \*\*chain-like structure\*\*, where each
circle interlocks only with its immediate neighbor, resembling links in
a chain.

## Usage

``` r
make_chain_circles(n = c(200, 100), p = 4, k = 2, offset = 0.5, angle = 90)
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

  Numeric, the positional shift applied to each circle along its linking
  axis to ensure interlocking instead of overlap. Default is `0.5`.

- angle:

  Numeric, the rotation angle (in degrees) used when placing each
  subsequent circle into its respective plane. Default is `90`.

## Value

A data frame (or tibble, depending on
[`gen_multicluster()`](https://jayanilakshika.github.io/cardinalR/reference/gen_multicluster.md))
containing the generated points and cluster assignments.

## Examples

``` r
# Generate two chain-linked circles in 4-D
twochain_circles <- make_chain_circles()
#> ✔ Data generation completed successfully!!!
#> ✔ Data generation completed successfully!!!
#> ✔ Multiple clusters generation completed successfully!!!
```
