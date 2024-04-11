
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cardinalR <img src="man/figures/logo.png" align="right" height="250" width="auto" alt="" />

The `cardinalR` provides functionality for generating simulation
high-dimensional datasets for use in various Nonlinear dimension
reduction techniqu.

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
#>           [,1]      [,2]         [,3]         [,4]        [,5]
#> [1,]  2.486879  3.509532  0.176066187  0.004353183  0.04879121
#> [2,]  2.002586  3.537843  0.001453752  0.034651974  0.04873449
#> [3,]  4.718657 -1.977447 -0.059712873  0.032223992  0.04165662
#> [4,] -2.584498  3.148256 -0.373358654 -0.039561103 -0.01725440
#> [5,] -3.911566 -1.322548  0.604768408 -0.024144310  0.02965819
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
