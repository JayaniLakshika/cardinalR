# Generate data with curvy shaped branches in a initial point

This function generates a dataset representing a structure with curvy
shaped branches.

## Usage

``` r
gen_orgcurvybranches(n = 400, p = 4, k = 4, allow_share = TRUE)
```

## Arguments

- n:

  A numeric value (default: 400) representing the sample size.

- p:

  A numeric value (default: 4) representing the number of dimensions.

- k:

  A numeric value (default: 4) representing the number of branches.

- allow_share:

  A logical value (default: TRUE). If TRUE, multiple branches may share
  the same 2D subspace. If FALSE, branches are sampled without
  replacement from all possible 2D subspaces until exhausted.

## Value

A data containing curvy shaped branches originated in one point.

## Examples

``` r
set.seed(20240412)
orgcurvybranches <- gen_orgcurvybranches(n = 400, k = 4)
#> ✔ 2 noise dimensions have been generated successfully!!!
#> ✔ 2 noise dimensions have been generated successfully!!!
#> ✔ 2 noise dimensions have been generated successfully!!!
#> ✔ 2 noise dimensions have been generated successfully!!!
#> ✔ Data generation completed successfully!!!
```
