# Generate Parallel Multi-Shape Clusters

This function generates synthetic high-dimensional data consisting of
\\k\\ clusters of a specified shape (e.g., crescents), arranged in
parallel along alternating dimensions. The first cluster is shifted
along the first dimension, the second along the third dimension, the
third along the first dimension again, and so on.

## Usage

``` r
make_shape_para(n = c(500, 300), k = 2, shift = 1, shape = "crescent")
```

## Arguments

- n:

  A numeric vector of length \\k\\, specifying the number of
  observations in each cluster. All values must be positive.

- k:

  Integer. Number of clusters to generate. Must be greater than 1.

- shift:

  Numeric. The distance between cluster centers along the alternating
  dimensions (default is \`0.4\`).

- shape:

  Character string. Shape of the clusters to generate (e.g.,
  \`"crescent"\`, \`"gridcube"\`, etc.). Must be a single value.

## Value

A tibble containing \\\sum n\\ rows and \\p\\ columns, with the
generated features (\`x1, x2, x3, x4\`) and a \`cluster\` label.

## Examples

``` r
# Generate 2 crescent-shaped clusters in 4D
twocrescent <- make_shape_para(n = c(500, 300), k = 2, shape = "crescent")
#> ✔ Data generation completed successfully!!!
#> ✔ Data generation completed successfully!!!
#> ✔ Multiple clusters generation completed successfully!!!
#> ✔ 2 noise dimensions have been generated successfully!!!

```
