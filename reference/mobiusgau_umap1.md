# UMAP embedding for mobiusgau dataset which with noise dimensions UMAP parameters set to n-neigbors: 15 and min-dist: 0.1.

The \`mobiusgau_umap1\` dataset contains the UMAP (Uniform Manifold
Approximation and Projection) embeddings of a five-dimensional
mobiusgau. Each data point is represented by two UMAP coordinates (emb1
and emb2).

## Usage

``` r
data(mobiusgau_umap1)
```

## Format

\## \`mobiusgau_umap1\` A data frame with 1000 rows and 4 columns:

- emb1:

  Numeric, first UMAP 2D embeddings.

- emb2:

  Numeric, second UMAP 2D embeddings.

## Source

This dataset is generated for illustrative purposes.

## Examples

``` r
# Load the mobiusgau_umap1 dataset
data(mobiusgau_umap1)

# Display the first few rows of the dataset
head(mobiusgau_umap1)
#> # A tibble: 6 × 2
#>     emb1   emb2
#>    <dbl>  <dbl>
#> 1  4.05   6.68 
#> 2  0.311 -0.488
#> 3 -3.20  -4.35 
#> 4  6.42   2.83 
#> 5 -6.16  -0.825
#> 6  6.93   4.01 
```
