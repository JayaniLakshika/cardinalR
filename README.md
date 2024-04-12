
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cardinalR <img src="man/figures/logo.png" align="right" height="250" alt="" />

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
mobius_clust_data <-  mobius_clust(n = 500, num_noise = 2, min_n = -0.05, 
                                   max_n = 0.05)

head(mobius_clust_data, 5)
#>           [,1]       [,2]        [,3]         [,4]         [,5]
#> [1,]  3.164352 -3.1579512 -0.16601543  0.023294189  0.006899329
#> [2,]  2.317015 -3.5780339 -0.24598113  0.041812066  0.027856200
#> [3,]  3.063388  3.4620065  0.09776815  0.038146483 -0.025419387
#> [4,]  2.834203  0.6149248 -0.20932330  0.023618497 -0.028356911
#> [5,] -3.953907 -1.5987889 -0.66295592 -0.005704091  0.041557090
```

<table style="width:100%">
<tr>
<td align="center">
<img src="man/figures/mobius_1.png" height="100" alt="" />
</td>
<td align="center">
<img src="man/figures/mobius_2.png" height="100" alt="" />
</td>
<td align="center">
<img src="man/figures/mobius_3.png" height="100" alt="" />
</td>
</tr>
</table>

You can find the high-dimensional view in
[here](https://youtu.be/731aZxDifCs).

## About the name

**C**ollection of v**ar**ious high-**d**imens**i**o**nal** data
structures in **R**

## Copyright

This package is licensed under the [MIT
license](https://github.com/JayaniLakshika/cardinalR/tree/main?tab=MIT-2-ov-file).
