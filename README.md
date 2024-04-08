
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cardinalR <img src="man/figures/logo.png" align="right" height="250" alt="" />

<!-- badges: start -->
<!-- badges: end -->

The `cardinalR` provides functionality for generating various
high-dimensional datasets.

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
mobius_clust_data <-  mobius_cluster_with_noise(sample_size = 500, num_noise_dims = 2, 
                         min_noise = -0.05, max_noise = 0.05)

head(mobius_clust_data, 5)
#>           [,1]       [,2]         [,3]          [,4]        [,5]
#> [1,]  4.934994 -0.4576561 -0.066051801 -0.0144460023  0.03307315
#> [2,] -1.711057 -4.1963798 -0.826138823 -0.0227761611  0.04526907
#> [3,] -3.976282  1.3225514 -0.003296925 -0.0004828004 -0.02307185
#> [4,]  3.809345  0.3222411  0.066091474  0.0381951931 -0.02016529
#> [5,]  4.389242  2.3650781  0.301491229 -0.0478621226  0.00923650
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
