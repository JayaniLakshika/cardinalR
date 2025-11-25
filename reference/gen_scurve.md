# Generate S-curve Data

This function generates S-curve data.

## Usage

``` r
gen_scurve(n = 500)
```

## Arguments

- n:

  A numeric value (default: 500) representing the sample size.

## Value

A data containing the generated S-curve data.

## References

Buitinck, L., Louppe, G., Blondel, M., Pedregosa, F., Mueller, A.,
Grisel, O., ... & Varoquaux, G. (2013). API design for machine learning
software: experiences from the scikit-learn project. *arXiv preprint*
*arXiv:1309.0238*.

## Examples

``` r
set.seed(20240412)
scurve <- gen_scurve(n = 500)
#> ✔ Data generation completed successfully!!!
```
