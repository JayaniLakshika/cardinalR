# Generate a Chain of Interlocked curvycycle in High-Dimensional Space

This function generates \\k\\ interlocked circular clusters in a
\\p\\-dimensional space. Unlike
[`make_klink_curvycycle()`](https://jayanilakshika.github.io/cardinalR/reference/make_klink_curvycycle.md),
the curvycycle are arranged in a \*\*chain-like structure\*\*, where
each curvycycle interlocks only with its immediate neighbor, resembling
links in a chain.

## Usage

``` r
make_chain_curvycycle(n = c(200, 100), p = 4, k = 2, offset = 0.5, angle = 90)
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

  Numeric, the positional shift applied to each curvycycle along its
  linking axis to ensure interlocking instead of overlap. Default is
  `0.5`.

- angle:

  Numeric, the rotation angle (in degrees) used when placing each
  subsequent curvycycle into its respective plane. Default is `90`.

## Value

A data frame (or tibble, depending on
[`gen_multicluster()`](https://jayanilakshika.github.io/cardinalR/reference/gen_multicluster.md))
containing the generated points and cluster assignments.

## Examples

``` r
# Generate two chain-linked curvycycle in 4-D
twochain_curvycycle <- make_chain_curvycycle()
#> ✔ Data generation completed successfully!!!
#> ✔ Data generation completed successfully!!!
#> ✔ Multiple clusters generation completed successfully!!!
```
