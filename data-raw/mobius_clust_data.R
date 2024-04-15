library(cardinalR)
library(tibble)
library(Rtsne)
library(umap)

set.seed(20240412)

## To generate mobius clust data

mobius_clust_data <- mobius_clust(
  n = 500, num_noise = 2, min_n = -0.05,
  max_n = 0.05
)

colnames(mobius_clust_data) <- paste0("x", seq_len(NCOL(mobius_clust_data)))

mobius_clust_data <- tibble::as_tibble(mobius_clust_data)

usethis::use_data(mobius_clust_data, overwrite = TRUE)


## Fit umap with different parameter settings
### Param 1
umap_fit <- umap(mobius_clust_data, n_neighbors = 15, min_dist = 0.1, n_components = 2)

umap_layout <- umap_fit$layout

colnames(umap_layout) <- c("emb1", "emb2")

mobius_clust_data_umap_param1 <- umap_layout |>
  as_tibble()

usethis::use_data(mobius_clust_data_umap_param1, overwrite = TRUE)

### Param 2
umap_fit <- umap(mobius_clust_data, n_neighbors = 30, min_dist = 0.08, n_components = 2)

umap_layout <- umap_fit$layout

colnames(umap_layout) <- c("emb1", "emb2")

mobius_clust_data_umap_param2 <- umap_layout |>
  as_tibble()

usethis::use_data(mobius_clust_data_umap_param2, overwrite = TRUE)

### Param 3
umap_fit <- umap(mobius_clust_data, n_neighbors = 5, min_dist = 0.9, n_components = 2)

umap_layout <- umap_fit$layout

colnames(umap_layout) <- c("emb1", "emb2")

mobius_clust_data_umap_param3 <- umap_layout |>
  as_tibble()

usethis::use_data(mobius_clust_data_umap_param3, overwrite = TRUE)

## Fit tsne with different parameter settings
### Param 1
tsne_fit <- Rtsne(mobius_clust_data,
  perplexity = 15, pca = FALSE,
  pca_center = FALSE, normalize = FALSE
)

tsne_layout <- tsne_fit$Y

colnames(tsne_layout) <- c("emb1", "emb2")

mobius_clust_data_tsne_param1 <- umap_layout |>
  as_tibble()

usethis::use_data(mobius_clust_data_tsne_param1, overwrite = TRUE)

### Param 2
tsne_fit <- Rtsne(mobius_clust_data,
  perplexity = 30, pca = FALSE,
  pca_center = FALSE, normalize = FALSE
)

tsne_layout <- tsne_fit$Y

colnames(tsne_layout) <- c("emb1", "emb2")

mobius_clust_data_tsne_param2 <- umap_layout |>
  as_tibble()

usethis::use_data(mobius_clust_data_tsne_param2, overwrite = TRUE)

### Param 3
tsne_fit <- Rtsne(mobius_clust_data,
  perplexity = 5, pca = FALSE,
  pca_center = FALSE, normalize = FALSE
)

tsne_layout <- tsne_fit$Y

colnames(tsne_layout) <- c("emb1", "emb2")

mobius_clust_data_tsne_param3 <- umap_layout |>
  as_tibble()

usethis::use_data(mobius_clust_data_tsne_param3, overwrite = TRUE)
