# Mobius clust dataset with a noise dimension

The \`mobiusgau\` dataset contains a 3-dimensional Mobius and Gaussian
cluster with added noise dimension. Each data point is represented by
five dimensions (x1 to x4).

## Usage

``` r
data(mobiusgau)
```

## Format

A data frame with 1000 rows and 4 columns:

- x1, x2, x3, x4:

  High-dimensional coordinates

## Source

This dataset is generated for illustrative purposes.

## Examples

``` r
# Load the mobiusgau dataset
data(mobiusgau)

# Display the first few rows of the dataset
head(mobiusgau)
#> # A tibble: 6 × 4
#>        x1       x2       x3      x4
#>     <dbl>    <dbl>    <dbl>   <dbl>
#> 1  0.517  -0.579   -0.00951 -0.0401
#> 2  0.0142 -0.00841  0.0147   0.0579
#> 3 -0.402   0.506   -0.196   -0.0325
#> 4 -0.218  -0.684    0.0347   0.0497
#> 5  0.225   0.699    0.00575  0.0251
#> 6 -0.0651 -0.929   -0.199    0.0112
```
