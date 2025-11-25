# cardinalR

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/cardinalR)](https://CRAN.R-project.org/package=cardinalR)
[![Downloads](http://cranlogs.r-pkg.org/badges/cardinalR)](https://cran.r-project.org/package=cardinalR)

The `cardinalR` package provides a collection of functions to generate a
large variety of structures in high dimensions.

## Installation

You can install the released version of `cardinalR` from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("cardinalR") 
```

The development version from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("JayaniLakshika/cardinalR")
```

## Example

``` r
library(cardinalR)
```

``` r
head(mobiusgau, 5)
#> # A tibble: 5 × 4
#>        x1       x2       x3      x4
#>     <dbl>    <dbl>    <dbl>   <dbl>
#> 1  0.517  -0.579   -0.00951 -0.0401
#> 2  0.0142 -0.00841  0.0147   0.0579
#> 3 -0.402   0.506   -0.196   -0.0325
#> 4 -0.218  -0.684    0.0347   0.0497
#> 5  0.225   0.699    0.00575  0.0251
```

To view the data in high-dimensional space

``` r
langevitour(mobiusgau)
```

Following shows three 2-D projections from the 4-D `mobiusgau` data.

|                                                                      |                                                                      |                                                                      |
|:--------------------------------------------------------------------:|:--------------------------------------------------------------------:|:--------------------------------------------------------------------:|
| ![Mobius Gaussian data projection 1](reference/figures/mobius_1.png) | ![Mobius Gaussian data projection 2](reference/figures/mobius_2.png) | ![Mobius Gaussian data projection 3](reference/figures/mobius_3.png) |

You can find the high-dimensional view in
[here](https://youtu.be/D2drIAnz4pM).

tSNE (t-distributed Stochastic Neighbor Embedding) and UMAP (Uniform
Manifold Approximation and Projection) representations of `mobiusgau`
are shown below. Figures a–c illustrate the t-SNE projections and
figures d–f present the corresponding UMAP projections.

![tSNE and UMAP layouts with different hyperparameter
choices.](reference/figures/README-unnamed-chunk-6-1.png)

## About the name

**c**ollection of v**ar**ious high-**d**imens**i**o**nal** data
structures in **R**

## Copyright

This package is licensed under the [MIT
license](https://github.com/JayaniLakshika/cardinalR/tree/main?tab=MIT-2-ov-file).
