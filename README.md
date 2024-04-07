
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cardinalR <img src="man/figures/logo.png" align="right" height="139" alt="" />

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
#>           [,1]      [,2]        [,3]        [,4]         [,5]
#> [1,]  2.711220 -1.130782  0.01026218 -0.04407727 -0.003484334
#> [2,]  2.194750  2.514627 -0.52575579 -0.03058854 -0.023599587
#> [3,]  1.849512 -2.791968  0.30483860  0.01893805  0.007192585
#> [4,] -1.225577  4.115612  0.57627492 -0.02055877  0.048656527
#> [5,] -2.454406 -2.294214  1.12465202  0.01771089  0.002811003
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
