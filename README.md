
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cardinalR <img src="man/figures/logo.png" align="right" height="250" width="auto" alt="" />

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
#>            [,1]      [,2]        [,3]        [,4]        [,5]
#> [1,]  3.6190498 -3.212243  0.07545382  0.04050106  0.03858323
#> [2,]  0.7685641 -4.293308 -0.32906750 -0.03478474 -0.02713460
#> [3,]  2.1550013 -4.222857 -0.39926915  0.03916184  0.01308043
#> [4,]  1.4419803  3.710829 -0.03566859 -0.02257588 -0.02419622
#> [5,] -3.8336371  1.199429 -0.43352123  0.02474006  0.03636155
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
