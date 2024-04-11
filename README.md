
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cardinalR <img src="man/figures/logo.png" align="right" height="250" width="auto" alt="" />

The `cardinalR` provides functionality for generating simulation
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
#>             [,1]        [,2]         [,3]        [,4]        [,5]
#> [1,] -3.84611861 -1.25704741 -0.638967081 -0.02949529  0.04362883
#> [2,]  1.20362120 -3.58687775  0.385511981 -0.02135688  0.04524982
#> [3,]  0.47560693  4.07085478  0.015786283 -0.02049019 -0.04610679
#> [4,] -0.08629218  3.30104260 -0.336331289  0.04988311  0.04108780
#> [5,]  4.67023931  0.03962249 -0.004886119 -0.01175689 -0.04458614
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

You can find the high-dimensional view
[here](https://youtu.be/731aZxDifCs).

## About the name

**C**ollection of v**ar**ious high-**d**imens**i**o**nal** data
structures in **R**

## Copyright

This package is licensed under the [MIT
license](https://github.com/JayaniLakshika/cardinalR/tree/main?tab=MIT-2-ov-file).
