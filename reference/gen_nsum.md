# Generates a vector of positive integers whose summation is approximately equal to a target value.

This function takes a target integer \`n\` and the number of clusters
\`k\`, and returns a vector \`n_vec\` of length \`k\` containing
positive integers. The goal is to have the summation of the elements in
\`n_vec\` be as close as possible to \`n\`, especially when \`n\` is not
a perfect multiplier of \`k\`.

## Usage

``` r
gen_nsum(n = 500, k = 4)
```

## Arguments

- n:

  The target positive integer value for the summation of the output
  vector.

- k:

  The number of dimensions (the length of the output vector). Must be a
  positive integer.

## Value

A sorted vector of positive integers of length \`k\`. The summation of
the elements in this vector will be approximately equal to \`n\`. If
\`n\` is a perfectly divisible by \`k\`, the elements will be equal.

## Examples

``` r
gen_nsum(500, 6) # Example with n=500, p=6
#> [1] 83 84 84 83 83 83
gen_nsum(700, 4) # Example with n=700, p=4
#> [1] 175 175 175 175
gen_nsum(625, 5) # Example with n=625 (perfect division)
#> [1] 125 125 125 125 125
gen_nsum(30, 3)  # Example with n=30, p=3
#> [1] 10 10 10
```
