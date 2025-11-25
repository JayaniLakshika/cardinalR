# tSNE embedding for mobiusgau dataset which with noise dimensions tSNE parameters set to perplexity: 30.

The \`mobiusgau_tsne2\` dataset contains the tSNE (t-distributed
Stochastic Neighbor Embedding) embeddings of a five-dimensional
mobiusgau. Each data point is represented by two tSNE coordinates (emb1
and emb2).

## Usage

``` r
data(mobiusgau_tsne2)
```

## Format

\## \`mobiusgau_tsne2\` A data frame with 1000 rows and 4 columns:

- emb1:

  Numeric, first tSNE 2D embeddings.

- emb2:

  Numeric, second tSNE 2D embeddings.

## Source

This dataset is generated for illustrative purposes.

## Examples

``` r
# Load the mobiusgau_tsne2 dataset
data(mobiusgau_tsne2)

# Display the first few rows of the dataset
head(mobiusgau_tsne2)
#> # A tibble: 6 × 2
#>     emb1   emb2
#>    <dbl>  <dbl>
#> 1   2.31 -14.9 
#> 2  30.2   -6.75
#> 3 -16.0   13.3 
#> 4 -14.1  -16.5 
#> 5  -1.77  19.2 
#> 6 -10.1  -22.2 
```
