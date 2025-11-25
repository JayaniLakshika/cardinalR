# UMAP embedding for mobiusgau dataset which with noise dimensions UMAP parameters set to n-neigbors: 30 and min-dist: 0.08.

The \`mobiusgau_umap2\` dataset contains the UMAP (Uniform Manifold
Approximation and Projection) embeddings of a five-dimensional
mobiusgau. Each data point is represented by two UMAP coordinates (emb1
and emb2).

## Usage

``` r
data(mobiusgau_umap2)
```

## Format

\## \`mobiusgau_umap2\` A data frame with 1000 rows and 4 columns:

- emb1:

  Numeric, first UMAP 2D embeddings.

- emb2:

  Numeric, second UMAP 2D embeddings.

## Source

This dataset is generated for illustrative purposes.

## Examples

``` r
# Load the mobiusgau_umap2 dataset
data(mobiusgau_umap2)

# Display the first few rows of the dataset
head(mobiusgau_umap2)
#> # A tibble: 6 × 2
#>      emb1   emb2
#>     <dbl>  <dbl>
#> 1  3.83    3.83 
#> 2 -0.0482  0.411
#> 3 -3.80   -3.87 
#> 4  4.85   -0.758
#> 5 -5.44    0.681
#> 6  5.40    0.795
```
