# tSNE embedding for mobiusgau dataset which with noise dimensions tSNE parameters set to perplexity: 15.

The \`mobiusgau_tsne1\` dataset contains the tSNE (t-distributed
Stochastic Neighbor Embedding) embeddings of a five-dimensional
mobiusgau. Each data point is represented by two tSNE coordinates (emb1
and emb2).

## Usage

``` r
data(mobiusgau_tsne1)
```

## Format

\## \`mobiusgau_tsne1\` A data frame with 1000 rows and 4 columns:

- emb1:

  Numeric, first tSNE 2D embeddings.

- emb2:

  Numeric, second tSNE 2D embeddings.

## Source

This dataset is generated for illustrative purposes.

## Examples

``` r
# Load the mobiusgau_tsne1 dataset
data(mobiusgau_tsne1)

# Display the first few rows of the dataset
head(mobiusgau_tsne1)
#> # A tibble: 6 × 2
#>     emb1   emb2
#>    <dbl>  <dbl>
#> 1 -32.1   -4.40
#> 2  26.9  -13.3 
#> 3  11.8   12.6 
#> 4 -16.7   16.5 
#> 5   1.04  -6.20
#> 6 -26.6   14.4 
```
