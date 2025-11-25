# Generate S-curve Data with a Hole

This function generates S-curve data with a hole by filtering out
samples that are not close to a specified anchor point.

## Usage

``` r
gen_scurvehole(n = 500, r_hole = 0.5)
```

## Arguments

- n:

  A numeric value (default: 500) representing the sample size.

- r_hole:

  A numeric value (default: 0.5) representing the radius of the hole.

## Value

A data containing the generated S-curve data with a hole.

## References

Wang, Y., Huang, H., Rudin, C., & Shaposhnik, Y. (2021). Understanding
how dimension reduction tools work: an empirical approach to deciphering
t-SNE, UMAP, TriMAP, and PaCMAP for data visualization. *J Mach. Learn.
Res*, *22*, 1-73.

## See also

the [PaCMAP homepage](https://github.com/YingfanWang/PaCMAP).

## Examples

``` r
set.seed(20240412)
scurvehole <- gen_scurvehole(n = 1000)
#> ✔ Data generation completed successfully!!!
#> ✔ Data generation completed successfully!!!
```
