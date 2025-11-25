# UMAP embedding for mobiusgau dataset which with noise dimensions UMAP parameters set to n-neigbors: 5 and min-dist: 0.9.

The \`mobiusgau_umap3\` dataset contains the UMAP (Uniform Manifold
Approximation and Projection) embeddings of a five-dimensional
mobiusgau. Each data point is represented by two UMAP coordinates (emb1
and emb2).

## Usage

``` r
data(mobiusgau_umap3)
```

## Format

\## \`mobiusgau_umap3\` A data frame with 1000 rows and 4 columns:

- emb1:

  Numeric, first UMAP 2D embeddings.

- emb2:

  Numeric, second UMAP 2D embeddings.

## Source

This dataset is generated for illustrative purposes.

## Examples

``` r
# Load the mobiusgau_umap3 dataset
data(mobiusgau_umap3)

# Display the first few rows of the dataset
head(mobiusgau_umap3)
#> # A tibble: 6 × 2
#>     emb1  emb2
#>    <dbl> <dbl>
#> 1   7.54  6.00
#> 2  -1.00  1.63
#> 3  -6.45 -4.81
#> 4  10.8  -3.05
#> 5 -10.5   3.37
#> 6   9.22  8.22
```
