# tSNE embedding for mobiusgau dataset which with noise dimensions tSNE parameters set to perplexity: 5.

The \`mobiusgau_tsne3\` dataset contains the tSNE (t-distributed
Stochastic Neighbor Embedding) embeddings of a five-dimensional
mobiusgau. Each data point is represented by two tSNE coordinates (emb1
and emb2).

## Usage

``` r
data(mobiusgau_tsne3)
```

## Format

\## \`mobiusgau_tsne3\` A data frame with 1000 rows and 4 columns:

- emb1:

  Numeric, first tSNE 2D embeddings.

- emb2:

  Numeric, second tSNE 2D embeddings.

## Source

This dataset is generated for illustrative purposes.

## Examples

``` r
# Load the mobiusgau_tsne1 dataset
data(mobiusgau_tsne3)

# Display the first few rows of the dataset
head(mobiusgau_tsne3)
#> # A tibble: 6 × 2
#>     emb1    emb2
#>    <dbl>   <dbl>
#> 1 -44.6  -21.0  
#> 2  -4.40   0.817
#> 3  25.3   -6.54 
#> 4  -3.63 -42.6  
#> 5  15.9   41.9  
#> 6 -11.5  -57.3  
```
