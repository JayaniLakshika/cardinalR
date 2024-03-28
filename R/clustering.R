#' Generate synthetic data with Gaussian clusters
#'
#' This function generates synthetic data with Gaussian clusters.
#'
#' @param sample_size The number of data points to generate.
#' @param with_seed Optional seed value for reproducibility.
#' @param num_clusters The number of clusters to generate.
#' @param mean_matrix A matrix specifying the means for each cluster.
#' Each row corresponds to a cluster, and each column corresponds to a dimension.
#' @param var_vec A vector specifying the variance for each dimension.
#' @param num_dims The number of dimensions for the data.
#' @param num_noise_dims The number of additional noise dimensions to add.
#' @param min_noise The minimum value for noise generation.
#' @param max_noise The maximum value for noise generation.
#'
#' @return A data frame containing the synthetic data with Gaussian clusters.
#' @examples
#'
#' gaussian_clusters(sample_size = 300, with_seed = NULL, num_clusters = 5,
#' mean_matrix = rbind(c(1,0,0,0), c(0,1,0,0), c(0,0,1,0),
#' c(0,0,0,1), c(0,0,0,0)), var_vec = c(0.05, 0.05, 0.05, 0.05, 0.05),
#' num_dims = 4, num_noise_dims = 0, min_noise = -0.05, max_noise = 0.05)
#'
#' @export
gaussian_clusters <- function(sample_size, with_seed = NULL, num_clusters,
                              mean_matrix, var_vec, num_dims, num_noise_dims,
                              min_noise, max_noise) {

  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  if (sample_size < num_clusters) {
    stop('Number of clusters exceed the number of observations.')

  }

  if ((num_dims == 0) | (num_dims == 1)) {
    stop('There should be at least two dimensions.')

  }

  if (dim(mean_matrix)[1] != length(var_vec)) {
    stop('The length of mean and variance vectors are different.')

  }

  if (dim(mean_matrix)[1] != num_clusters) {
    stop('There is not enough mean values for clusters.')

  }

  if (dim(mean_matrix)[2] != num_dims) {
    stop('There is not enough mean values for dimensions.')

  }

  if (length(var_vec) != num_clusters) {
    stop('There is not enough varaiance values for clusters.')

  }

  # To check that the assigned n is divided by three
  if ((sample_size%%num_clusters) != 0) {
    warning("The sample size should be a product of number of clusters.")
    cluster_size <- floor(sample_size/num_clusters)

  } else {
    cluster_size <- sample_size/num_clusters
  }


  # To generate empty tibble
  column_names <- paste0(rep("x", num_dims), 1:num_dims)
  df <- tibble::tibble(!!!stats::setNames(rep(list(NULL), length(column_names)), column_names))

  for (i in 1:num_clusters) {

    # To filter the mean values for specific cluster
    mean_val_for_cluster <- mean_matrix |>
      tibble::as_tibble(.name_repair = "unique") |>
      dplyr::filter(dplyr::row_number() == i) |>
      unlist(use.names = FALSE)

    # To filter the variance values for specific cluster
    variance_val_for_cluster <- var_vec[i]

    # Initialize an empty list to store the vectors with column
    # values
    dim_val_list <- list()

    for (j in 1:num_dims) {

      dim_val_list[[column_names[j]]] <- stats::rnorm(cluster_size, mean = mean_val_for_cluster[j],
                                                      sd = variance_val_for_cluster)

    }
    # To generate a tibble for a cluster
    df_cluster <- tibble::as_tibble(dim_val_list)

    df <- dplyr::bind_rows(df, df_cluster)

  }

  if (num_noise_dims != 0) {

    # To generate column names for noise dimensions
    column_names <- paste0(rep("x", num_noise_dims), (NCOL(df) + 1):((NCOL(df) + 1) + num_noise_dims))

    # Initialize an empty list to store the vectors with column
    # values
    noise_dim_val_list <- list()

    for (j in 1:num_noise_dims) {
      if ((j%%2) == 0) {
        noise_dim_val_list[[column_names[j]]] <- stats::runif(n,
                                                       min = min_noise, max = max_noise)
      } else {
        noise_dim_val_list[[column_names[j]]] <- (-1) * stats::runif(n,
                                                              min = min_noise, max = max_noise)
      }


    }

    df_noise <- tibble::as_tibble(noise_dim_val_list)
    df <- dplyr::bind_cols(df, df_noise)

    df

  } else {

    df

  }
}
