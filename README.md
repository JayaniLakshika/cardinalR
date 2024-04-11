
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cardinalR <img src="man/figures/logo.png" align="right" height="250" width="auto" alt="" />

The `cardinalR` provides functionality for generating simulation
high-dimensional datasets for use in various Nonlinear dimensionality
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
#>             [,1]      [,2]        [,3]        [,4]         [,5]
#> [1,]  4.14938980  1.314429  0.43507573 -0.04438254 -0.005752487
#> [2,]  0.43150826  4.084147 -0.03339845  0.04392610 -0.037474740
#> [3,] -0.05899146 -3.861268  0.15298222 -0.03167411 -0.026738660
#> [4,]  2.62806362 -1.797529  0.30105461 -0.04480876 -0.005235957
#> [5,] -2.70225381  2.682575 -0.63236840 -0.03620112  0.035464684
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
