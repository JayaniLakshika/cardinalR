# Generates a vector of positive integers whose product is approximately equal to a target value.

This function takes a target integer \`n\` and the number of dimensions
\`p\`, and returns a vector \`n_vec\` of length \`p\` containing
positive integers. The goal is to have the product of the elements in
\`n_vec\` be as close as possible to \`n\`, especially when \`n\` is not
a perfect p-th power.

## Usage

``` r
gen_nproduct(n = 500, p = 4)
```

## Arguments

- n:

  The target positive integer value for the product of the output
  vector.

- p:

  The number of dimensions (the length of the output vector). Must be a
  positive integer.

## Value

A sorted vector of positive integers of length \`p\`. The product of the
elements in this vector will be approximately equal to \`n\`. If \`n\`
is a perfect p-th power, the elements will be equal.

## Examples

``` r
gen_nproduct(500, 6) # Example with n=500, p=6
#> [1] 1 2 2 5 5 5
gen_nproduct(700, 4) # Example with n=700, p=4
#> [1] 4 5 5 7
gen_nproduct(625, 4) # Example with n=625 (perfect power)
#> [1] 5 5 5 5
gen_nproduct(30, 3)  # Example with n=30, p=3
#> [1] 2 3 5
gen_nproduct(7, 2)   # Example where exact product might be hard
#> [1] 2 3
```
