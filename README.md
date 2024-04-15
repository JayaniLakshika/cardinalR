
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cardinalR <img src="man/figures/logo.png" align="right" height="150" alt="" />

The `cardinalR` package provides functionality for generating simulation
high-dimensional datasets for use in various Nonlinear dimension
reduction techniques.

## Installation

You can install the development version of `cardinalR` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("JayaniLakshika/cardinalR")
```

## Example

``` r
library(cardinalR)
```

``` r
head(mobius_clust_data, 5)
#> # A tibble: 5 × 5
#>       x1    x2     x3       x4       x5
#>    <dbl> <dbl>  <dbl>    <dbl>    <dbl>
#> 1 -1.02   3.57 -0.373  0.00687 -0.0139 
#> 2  0.793 -3.56  0.620 -0.0261  -0.0457 
#> 3 -1.91  -3.91 -0.533 -0.0232  -0.0122 
#> 4 -2.79   2.94 -0.364  0.0415   0.0214 
#> 5 -1.70  -3.59  0.277 -0.00686  0.00410
```

<table style="width:100%">
<tr>
<td align="center">
<img src="man/figures/mobius_1.png" height="200" alt="" />
</td>
<td align="center">
<img src="man/figures/mobius_2.png" height="200" alt="" />
</td>
<td align="center">
<img src="man/figures/mobius_3.png" height="200" alt="" />
</td>
</tr>
</table>

You can find the high-dimensional view in
[here](https://youtu.be/731aZxDifCs).

tSNE (t-distributed Stochastic Neighbor Embedding) and UMAP (Uniform
Manifold Approximation and Projection) representations of
`mobius_clust_data` are shown below.

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" style="display: block; margin: auto;" />

## About the name

**C**ollection of v**ar**ious high-**d**imens**i**o**nal** data
structures in **R**

## Copyright

This package is licensed under the [MIT
license](https://github.com/JayaniLakshika/cardinalR/tree/main?tab=MIT-2-ov-file).
