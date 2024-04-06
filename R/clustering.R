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

    df <- rbind(df, df_cluster)

  }

  if (num_noise_dims != 0) {

    noise_mat <- gen_noise_dims(n = dim(df)[1], num_noise_dims = num_noise_dims,
                                min_noise = min_noise, max_noise = max_noise)
    df <- cbind(df, noise_mat)

    df

  } else {

    df

  }
}


clusters_different_shapes <- function(sample_size = 300, with_seed = NULL, num_gussian_clusters = 4, num_non_gaussian_clusters = 2,
                                      cluster_sd_gau = 0.05, cluster_sd_non_gau = 0.1, num_dims = 7, a = 2, b = 4) {


  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  num_clusters <- num_gussian_clusters + num_non_gaussian_clusters

  # To check that the assigned sample_size is divided by three
  if ((sample_size%%num_clusters) != 0) {
    warning("The sample size should be a product of three.")
    cluster_size <- floor(sample_size/num_clusters)

  } else {
    cluster_size <- sample_size/num_clusters
  }

  ## Generate Gaussian clusters

  # Create a vector of possible values (0 and 1)
  values <- c(0, 1)

  # Create an expanded grid with 0's and 1's
  mean_val_grid <- tidyr::expand_grid(!!!setNames(rep(list(values), num_dims),
                                                  paste0("mean_dim", 1:num_dims)))

  # To select combinations for assigned number of clusters

  mean_val_grid_gau <- mean_val_grid %>%
    dplyr::slice_sample(n = num_gussian_clusters)

  mean_val_grid_non_gau <- mean_val_grid %>%
    dplyr::slice_sample(n = num_non_gaussian_clusters)


  # To generate empty tibble
  column_names <- paste0(rep("x", num_dims), 1:num_dims)
  df <- tibble(!!!setNames(rep(list(NULL), length(column_names)), column_names))

  for (i in 1:num_gussian_clusters) {

    # To filter the mean values for specific cluster
    mean_val_for_cluster <- mean_val_grid_gau %>%
      dplyr::filter(dplyr::row_number() == i) %>%
      unlist(use.names = FALSE)

    # Initialize an empty list to store the vectors with column
    # values
    dim_val_list <- list()

    for (j in 1:num_dims) {

      dim_val_list[[column_names[j]]] <- stats::rnorm(cluster_size, mean = mean_val_for_cluster[j],
                                               sd = cluster_sd_gau)

    }
    # To generate a tibble for a cluster
    df_gau_cluster <- tibble::as_tibble(dim_val_list)

    df <- rbind(df, df_gau_cluster)

  }

  phi <- stats::runif(cluster_size, max = 2*pi)
  rho <- sqrt(stats::runif(cluster_size))

  for (i in 1:num_non_gaussian_clusters) {

    # To filter the mean values for specific cluster
    presence_of_elipse_cluster <- mean_val_grid_non_gau %>%
      dplyr::filter(dplyr::row_number() == i) %>%
      unlist(use.names = FALSE)

    # Initialize an empty list to store the vectors with column
    # values
    dim_val_list_n <- list()

    for (j in 1:num_dims) {
      if(presence_of_elipse_cluster[j] == 1){
        dim_val_list_n[[column_names[j]]] <- sqrt(a)*rho*cos(phi) + b
        ## Surface of poolar coordinate
      } else {
        dim_val_list_n[[column_names[j]]] <- stats::rnorm(cluster_size, mean = 0,
                                                   sd = cluster_sd_non_gau)

      }



    }
    # To generate a tibble for a cluster
    df_non_gau_cluster <- tibble::as_tibble(dim_val_list_n)

    df <- rbind(df, df_non_gau_cluster)

  }

  df

}

cluster_and_curvilinear__with_noise_and_bkg_noise <- function(sample_size = 260, with_seed = NULL, num_of_noise_dim = 8,
                                                              min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by 2
  if ((sample_size%%2) != 0) {
    stop("The sample size should be a product of 2.")

  } else {
    cluster_size <- (sample_size - sample_size * 0.3)/2
  }

  theta = stats::runif(cluster_size, 0.20,0.60 * pi)
  x = cos(theta) + stats::rnorm(cluster_size, 10, 0.03)
  y = sin(theta) + stats::rnorm(cluster_size, 10, 0.03)

  z <- rep(0, cluster_size) + stats::rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - stats::rnorm(cluster_size, 10, 0.03)

  df1 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x = stats::rnorm(cluster_size, 10, 0.05)
  y = stats::rnorm(cluster_size, 10, 0.05)

  z <- rep(0, cluster_size) + stats::rnorm(cluster_size, 10, 0.05)
  w <- rep(0, cluster_size) - stats::rnorm(cluster_size, 10, 0.05)

  df2 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x = stats::rnorm(sample_size * 0.3, 11, 0.5)
  y = stats::rnorm(sample_size * 0.3, 11, 0.5)

  z <- rep(0, sample_size * 0.3) + stats::rnorm(sample_size * 0.3, 10, 0.05)
  w <- rep(0, sample_size * 0.3) - stats::rnorm(sample_size * 0.3, 10, 0.05)

  df3 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  df <- rbind(df1, df2, df3)
  names(df) <- paste0(rep("x", NCOL(df)), 1:NCOL(df))

  if (num_noise_dims != 0) {

    noise_mat <- gen_noise_dims(n = dim(df)[1], num_noise_dims = num_noise_dims,
                                min_noise = min_noise, max_noise = max_noise)
    df <- cbind(df, noise_mat)

    df

  } else {

    df

  }

}

one_doublet_with_noise <- function(sample_size = 110, with_seed = NULL, num_of_noise_dim = 6,
                                   min_noise = -0.05, max_noise = 0.05) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }


  # To check that the assigned sample_size is divided by 2.2
  if (((sample_size * 10)%%22) != 0) { #sample_size%%2.2
    stop("The sample size should be a product of 2.2.")

  } else {
    cluster_size <- (sample_size * 10)/22
  }


  df1 <- tibble::tibble(x=stats::rnorm(cluster_size, mean = 0, sd = 0.05), y=stats::rnorm(cluster_size, mean = 1, sd = 0.05), z=stats::rnorm(cluster_size, mean = 0, sd = 0.05), w=stats::rnorm(cluster_size, mean = 0, sd = 0.05))

  df2 <- tibble::tibble(x=stats::rnorm(cluster_size, mean = 1, sd = 0.05), y=stats::rnorm(cluster_size, mean = 0, sd = 0.05), z=stats::rnorm(cluster_size, mean = 0, sd = 0.05), w=stats::rnorm(cluster_size, mean = 0, sd = 0.05))

  df3_new <- (df1 + df2) / 2
  #get a sample of 10
  samp <- sample(nrow(df3_new), cluster_size * 0.20) ## 20% from the original dataset

  #data in the sample
  df3 <- df3_new[samp,]


  df <- rbind(df1, df2, df3)
  df <- df |>
    dplyr::rename(x1 = x, x2 = y, x3 = z, x4 = w)

  if (num_noise_dims != 0) {

    noise_mat <- gen_noise_dims(n = dim(df)[1], num_noise_dims = num_noise_dims,
                                min_noise = min_noise, max_noise = max_noise)
    df <- cbind(df, noise_mat)

    df

  } else {

    df

  }

}

three_doublets_with_noise <- function(sample_size = 210, with_seed = NULL, num_of_noise_dim = 8,
                                      min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by three
  if ((sample_size%%4.2) != 0) {
    warning("The sample size should be a product of number of clusters.")
    cluster_size <- floor(sample_size/4.2)

  } else {
    cluster_size <- sample_size/4.2
  }


  df1 <- tibble::tibble(x1=stats::rnorm(cluster_size, mean = 3, sd = 0.05), x2 = stats::rnorm(cluster_size, mean = 1, sd = 0.05), x3=stats::rnorm(cluster_size, mean = 1, sd = 0.05), x4=stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                        x5=stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                        x6=stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                        x7=stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                        x8=stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                        x9=stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                        x10=stats::rnorm(cluster_size, mean = 1, sd = 0.05))

  df2 <- tibble::tibble(x1=stats::rnorm(cluster_size, mean = 1, sd = 0.05), x2=stats::rnorm(cluster_size, mean = 1, sd = 0.05), x3=stats::rnorm(cluster_size, mean = 1, sd = 0.05), x4=stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                        x5=stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                        x6=stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                        x7=stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                        x8=stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                        x9=stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                        x10=stats::rnorm(cluster_size, mean = 1, sd = 0.05))

  df3_new <- (df1 + df2) / 2
  #get a sample of 10
  samp <- sample(nrow(df3_new), cluster_size * 0.40) ## 20% from the original dataset

  #data in the sample
  df3 <- df3_new[samp,]

  df4 <- tibble::tibble(x1=stats::rnorm(cluster_size, mean = 1, sd = 0.05), x2=stats::rnorm(cluster_size, mean = 1, sd = 0.05), x3=stats::rnorm(cluster_size, mean = 1, sd = 0.05), x4=stats::rnorm(cluster_size, mean = 3, sd = 0.05),
                        x5=stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                        x6=stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                        x7=stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                        x8=stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                        x9=stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                        x10=stats::rnorm(cluster_size, mean = 1, sd = 0.05))

  df5_new <- (df2 + df4) / 2

  #get a sample of 10
  samp1 <- sample(nrow(df5_new), cluster_size * 0.30) ## 20% from the original dataset

  #data in the sample
  df5 <- df5_new[samp1,]

  df6_new <- (df1 + df4) / 2

  #get a sample of 10
  samp2 <- sample(nrow(df6_new), cluster_size * 0.50) ## 20% from the original dataset

  #data in the sample
  df6 <- df6_new[samp2,]

  df <- rbind(df1, df2, df3, df4, df5, df6)

  if (num_noise_dims != 0) {

    noise_mat <- gen_noise_dims(n = dim(df)[1], num_noise_dims = num_noise_dims,
                                min_noise = min_noise, max_noise = max_noise)
    df <- cbind(df, noise_mat)

    df

  } else {

    df

  }

}


one_doublet_four_clusters_with_noise <- function(sample_size = 210, with_seed = NULL, num_of_noise_dim = 8,
                                                 min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by 4.4
  if (((sample_size * 10)%%44) != 0) { #sample_size%%4.4
    stop("The sample size should be a product of 4.4.")

  } else {
    cluster_size <- (sample_size * 10)/44
  }


  df1 <- tibble::tibble(x1=stats::rnorm(cluster_size, mean = 0, sd = 0.05), x2 = stats::rnorm(cluster_size, mean = 0, sd = 0.05), x3=stats::rnorm(cluster_size, mean = 0, sd = 0.05), x4=stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                        x5=stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                        x6=stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                        x7=stats::rnorm(cluster_size, mean = 1, sd = 0.05))

  df2 <- tibble::tibble(x1=stats::rnorm(cluster_size, mean = 0, sd = 0.05), x2=stats::rnorm(cluster_size, mean = 0, sd = 0.05), x3=stats::rnorm(cluster_size, mean = 0, sd = 0.05), x4=stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                        x5=stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                        x6=stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                        x7=stats::rnorm(cluster_size, mean = 0, sd = 0.05))

  df3_new <- (df1 + df2) / 2
  #get a sample of 10
  samp <- sample(nrow(df3_new), cluster_size * 0.40) ## 20% from the original dataset

  #data in the sample
  df3 <- df3_new[samp,]

  df4 <- tibble::tibble(x1=stats::rnorm(cluster_size, mean = 0, sd = 0.05), x2=stats::rnorm(cluster_size, mean = 0, sd = 0.05), x3=stats::rnorm(cluster_size, mean = 1, sd = 0.05), x4=stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                        x5=stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                        x6=stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                        x7=stats::rnorm(cluster_size, mean = 0, sd = 0.05))


  df5 <- tibble::tibble(x1=stats::rnorm(cluster_size, mean = 0, sd = 0.05), x2=stats::rnorm(cluster_size, mean = 0, sd = 0.05), x3=stats::rnorm(cluster_size, mean = 0, sd = 0.05), x4=stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                        x5=stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                        x6=stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                        x7=stats::rnorm(cluster_size, mean = 0, sd = 0.05))

  df <- rbind(df1, df2, df3, df4, df5)

  if (num_noise_dims != 0) {

    noise_mat <- gen_noise_dims(n = dim(df)[1], num_noise_dims = num_noise_dims,
                                min_noise = min_noise, max_noise = max_noise)
    df <- cbind(df, noise_mat)

    df

  } else {

    df

  }

}


one_doublet_dfifferent_var_clusters_with_noise <- function(sample_size = 260, with_seed = NULL, num_of_noise_dim = 0,
                                                           min_noise = -0.05, max_noise = 0.05) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by 2.6
  if (((sample_size * 10)%%26) != 0) {
    stop("The sample size should be a product of 2.6.")

  } else {
    cluster_size <- sample_size/2.6
  }


  df1 <- tibble::tibble(x1=stats::rnorm(cluster_size, mean = 1, sd = 0.1), x2 = stats::rnorm(cluster_size, mean = 0, sd = 0.08), x3=stats::rnorm(cluster_size, mean = 0, sd = 0.05), x4=stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                        x5=stats::rnorm(cluster_size, mean = 0, sd = 0.08),
                        x6=stats::rnorm(cluster_size, mean = 0, sd = 0.08),
                        x7=stats::rnorm(cluster_size, mean = 1, sd = 0.08),
                        x8=stats::rnorm(cluster_size, mean = 1, sd = 0.02),
                        x9=stats::rnorm(cluster_size, mean = 0, sd = 0.02),
                        x10=stats::rnorm(cluster_size, mean = 0, sd = 0.02))

  df2 <- tibble::tibble(x1=stats::rnorm(cluster_size, mean = 0, sd = 0.02), x2=stats::rnorm(cluster_size, mean = 0, sd = 0.02), x3=stats::rnorm(cluster_size, mean = 0, sd = 0.02), x4=stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                        x5=stats::rnorm(cluster_size, mean = 1, sd = 0.02),
                        x6=stats::rnorm(cluster_size, mean = 0, sd = 0.02),
                        x7=stats::rnorm(cluster_size, mean = 0, sd = 0.02),
                        x8=stats::rnorm(cluster_size, mean = 1, sd = 0.02),
                        x9=stats::rnorm(cluster_size, mean = 0, sd = 0.02),
                        x10=stats::rnorm(cluster_size, mean = 0, sd = 0.02))

  df3_new <- (df1 + df2) / 2
  #get a sample of 10
  samp <- sample(nrow(df3_new), cluster_size * 0.60) ## 20% from the original dataset

  #data in the sample
  df3 <- df3_new[samp,]

  df <- rbind(df1, df2, df3)

  if (num_noise_dims != 0) {

    noise_mat <- gen_noise_dims(n = dim(df)[1], num_noise_dims = num_noise_dims,
                                min_noise = min_noise, max_noise = max_noise)
    df <- cbind(df, noise_mat)

    df

  } else {

    df

  }

}


one_doublet_dfifferent_pattern_clusters_with_noise <- function(sample_size = 280, with_seed = NULL, num_of_noise_dim = 8,
                                                               min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by three
  if ((sample_size%%2.8) != 0) {
    warning("The sample size should be a product of number of clusters.")
    cluster_size <- floor(sample_size/2.8)

  } else {
    cluster_size <- sample_size/2.8
  }


  theta = stats::runif(cluster_size, 0.20, 0.60 * pi)

  df1 <- tibble::tibble(
    x1 = cos(theta) + stats::rnorm(cluster_size, 1, 0.5),
    x2 = sin(theta) + stats::rnorm(cluster_size, 1, 0.03),

    x3 = cos(theta) + stats::rnorm(cluster_size, 1, 0.03),
    x4 = sin(theta) + stats::rnorm(cluster_size, 1, 0.03),

    x5 = cos(theta) + stats::rnorm(cluster_size, 1, 0.03),
    x6 = sin(theta) + stats::rnorm(cluster_size, 1, 0.03),

    x7 = cos(theta) + stats::rnorm(cluster_size, 1, 0.05),
    x8 = sin(theta) + stats::rnorm(cluster_size, 1, 0.03),

    x9 = cos(theta) + stats::rnorm(cluster_size, 1, 0.3),
    x10 = sin(theta) + stats::rnorm(cluster_size, 1, 0.03))

  df2 <- tibble::tibble(x1=stats::rnorm(cluster_size, mean = 1, sd = 0.1), x2 = stats::rnorm(cluster_size, mean = 0, sd = 0.08), x3=stats::rnorm(cluster_size, mean = 0, sd = 0.05), x4=stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                        x5=stats::rnorm(cluster_size, mean = 0, sd = 0.08),
                        x6=stats::rnorm(cluster_size, mean = 0, sd = 0.08),
                        x7=stats::rnorm(cluster_size, mean = 1, sd = 0.08),
                        x8=stats::rnorm(cluster_size, mean = 1, sd = 0.02),
                        x9=stats::rnorm(cluster_size, mean = 0, sd = 0.02),
                        x10=stats::rnorm(cluster_size, mean = 0, sd = 0.02))


  df3_new <- (df1 + df2) / 2
  #get a sample of 10
  samp <- sample(nrow(df3_new), cluster_size * 0.80) ## 20% from the original dataset

  #data in the sample
  df3 <- df3_new[samp,]

  df <- rbind(df1, df2, df3)

  if (num_noise_dims != 0) {

    noise_mat <- gen_noise_dims(n = dim(df)[1], num_noise_dims = num_noise_dims,
                                min_noise = min_noise, max_noise = max_noise)
    df <- cbind(df, noise_mat)

    df

  } else {

    df

  }

}


two_doublets_parallel_with_noise <- function(sample_size = 440, with_seed = NULL, num_of_noise_dim = 0,
                                             min_noise = -0.05, max_noise = 0.05) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by 4.4
  if (((sample_size * 10)%%44) != 0) { #sample_size%%4.4
    stop("The sample size should be a product of 4.4.")

  } else {
    cluster_size <- (sample_size * 10)/44
  }


  df1 <- tibble::tibble(x1=stats::rnorm(cluster_size, mean = 1, sd = 0.05), x2 = stats::rnorm(cluster_size, mean = 0, sd = 0.05), x3=stats::rnorm(cluster_size, mean = 0, sd = 0.05), x4=stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                        x5=stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                        x6=stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                        x7=stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                        x8=stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                        x9=stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                        x10=stats::rnorm(cluster_size, mean = 0, sd = 0.05))

  df2 <- tibble::tibble(x1=stats::rnorm(cluster_size, mean = 0, sd = 0.05), x2=stats::rnorm(cluster_size, mean = 0, sd = 0.05), x3=stats::rnorm(cluster_size, mean = 0, sd = 0.05), x4=stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                        x5=stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                        x6=stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                        x7=stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                        x8=stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                        x9=stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                        x10=stats::rnorm(cluster_size, mean = 0, sd = 0.05))

  df3_new <- (df1 + df2) / 2
  #get a sample of 10
  samp <- sample(nrow(df3_new), cluster_size * 0.20) ## 20% from the original dataset

  #data in the sample
  df3 <- df3_new[samp,]

  df4 <- tibble::tibble(x1=stats::rnorm(cluster_size, mean = -1, sd = 0.05), x2 = stats::rnorm(cluster_size, mean = 0, sd = 0.05), x3=stats::rnorm(cluster_size, mean = 0, sd = 0.05), x4=stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                        x5=stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                        x6=stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                        x7=stats::rnorm(cluster_size, mean = -1, sd = 0.05),
                        x8=stats::rnorm(cluster_size, mean = -1, sd = 0.05),
                        x9=stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                        x10=stats::rnorm(cluster_size, mean = 0, sd = 0.05))

  df5 <- tibble::tibble(x1=stats::rnorm(cluster_size, mean = 0, sd = 0.05), x2=stats::rnorm(cluster_size, mean = 0, sd = 0.05), x3=stats::rnorm(cluster_size, mean = 0, sd = 0.05), x4=stats::rnorm(cluster_size, mean = -1, sd = 0.05),
                        x5=stats::rnorm(cluster_size, mean = -1, sd = 0.05),
                        x6=stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                        x7=stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                        x8=stats::rnorm(cluster_size, mean = -1, sd = 0.05),
                        x9=stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                        x10=stats::rnorm(cluster_size, mean = 0, sd = 0.05))

  df6_new <- (df4 + df5) / 2
  #get a sample of 10
  samp1 <- sample(nrow(df6_new), cluster_size * 0.20) ## 20% from the original dataset

  #data in the sample
  df6 <- df6_new[samp1,]

  df <- rbind(df1, df2, df3, df4, df5, df6)

  if (num_noise_dims != 0) {

    noise_mat <- gen_noise_dims(n = dim(df)[1], num_noise_dims = num_noise_dims,
                                min_noise = min_noise, max_noise = max_noise)
    df <- cbind(df, noise_mat)

    df

  } else {

    df

  }

}

one_doublets_with_bkg_noise <- function(sample_size = 250, with_seed = NULL, num_of_noise_dim = 8,
                                        min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by three
  if ((sample_size%%2.5) != 0) {
    warning("The sample size should be a product of number of clusters.")
    cluster_size <- floor(sample_size/2.5)

  } else {
    cluster_size <- sample_size/2.5
  }


  df1 <- tibble::tibble(x1=stats::rnorm(cluster_size, mean = 1, sd = 0.05), x2 = stats::rnorm(cluster_size, mean = 0, sd = 0.05), x3=stats::rnorm(cluster_size, mean = 0, sd = 0.05), x4=stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                        x5=stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                        x6=stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                        x7=stats::rnorm(cluster_size, mean = 1, sd = 0.05))

  df2 <- tibble::tibble(x1=stats::rnorm(cluster_size, mean = 0, sd = 0.05), x2=stats::rnorm(cluster_size, mean = 0, sd = 0.05), x3=stats::rnorm(cluster_size, mean = 0, sd = 0.05), x4=stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                        x5=stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                        x6=stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                        x7=stats::rnorm(cluster_size, mean = 0, sd = 0.05))

  df3_new <- (df1 + df2) / 2
  #get a sample of 10
  samp <- sample(nrow(df3_new), cluster_size * 0.20) ## 20% from the original dataset

  #data in the sample
  df3 <- df3_new[samp,]

  df4_new <- tibble::tibble(x1=stats::rnorm(cluster_size, mean = 0, sd = 0.2), x2 = stats::rnorm(cluster_size, mean = 0, sd = 0.5), x3=stats::rnorm(cluster_size, mean = 0.5, sd = 0.5), x4=stats::rnorm(cluster_size, mean = 0.2, sd = 0.5),
                            x5=stats::rnorm(cluster_size, mean = 0.2, sd = 0.3),
                            x6=stats::rnorm(cluster_size, mean = 0, sd = 0.5),
                            x7=stats::rnorm(cluster_size, mean = 0, sd = 0.3))

  #get a sample of 10
  samp1 <- sample(nrow(df4_new), cluster_size * 0.30) ## 20% from the original dataset

  #data in the sample
  df4 <- df4_new[samp1,]

  df <- rbind(df1, df2, df3, df4)

  if (num_noise_dims != 0) {

    noise_mat <- gen_noise_dims(n = dim(df)[1], num_noise_dims = num_noise_dims,
                                min_noise = min_noise, max_noise = max_noise)
    df <- cbind(df, noise_mat)

    df

  } else {

    df

  }

}

curvy_branching_with_noise <- function(sample_size = 100, with_seed = NULL, num_of_noise_dim = 8,
                                       min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by three
  if ((sample_size%%2) != 0) {
    warning("The sample size should be a product of number of clusters.")
    cluster_size <- floor(sample_size/2)

  } else {
    cluster_size <- sample_size/2
  }


  theta = stats::runif(cluster_size, 0.20, 0.90 * pi)

  df1 <- tibble::tibble(
    x1 = cos(theta) + stats::rnorm(cluster_size, 1, 0.06),
    x2 = sin(theta) + stats::rnorm(cluster_size, 1, 0.06),

    x3 = cos(theta) + stats::rnorm(cluster_size, 1, 0.06),
    x4 = sin(theta) + stats::rnorm(cluster_size, 1, 0.06)
  )

  theta1 = stats::runif(cluster_size, 0.20, 0.90 * pi)

  df2 <- tibble::tibble(
    x1 = cos(-theta1) + stats::rnorm(cluster_size, 1, 0.06),
    x2 = sin(-theta1) + stats::rnorm(cluster_size, 1, 0.06),

    x3 = cos(-theta1) + stats::rnorm(cluster_size, 1, 0.06),
    x4 = sin(-theta1) + stats::rnorm(cluster_size, 1, 0.06)
  )

  df <- rbind(df1, df2)

  if (num_noise_dims != 0) {

    noise_mat <- gen_noise_dims(n = dim(df)[1], num_noise_dims = num_noise_dims,
                                min_noise = min_noise, max_noise = max_noise)
    df <- cbind(df, noise_mat)

    df

  } else {

    df

  }

}


two_doublets_with_bkg_noise <- function(sample_size = 200, with_seed = NULL, num_of_noise_dim = 8,
                                        min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by three
  if ((sample_size%%4) != 0) {
    warning("The sample size should be a product of number of clusters.")
    cluster_size <- floor(sample_size/4)

  } else {
    cluster_size <- sample_size/4
  }

  df1 <- tibble::tibble(x1=stats::rnorm(cluster_size, mean = 0, sd = 0.05), x2=stats::rnorm(cluster_size, mean = 0, sd = 0.05), x3=stats::rnorm(cluster_size, mean = 0, sd = 0.05), x4=stats::rnorm(cluster_size, mean = 0, sd = 0.05))
  df2 <- tibble::tibble(x1=stats::rnorm(cluster_size, mean = 1, sd = 0.05), x2=stats::rnorm(cluster_size, mean = 0, sd = 0.05), x3=stats::rnorm(cluster_size, mean = 0, sd = 0.05), x4=stats::rnorm(cluster_size, mean = 0, sd = 0.05))

  df6_new <- (df1 + df2) / 2
  #get a sample of 10
  samp <- sample(nrow(df6_new), cluster_size * 0.20) ## 20% from the original dataset

  #data in the sample
  df6 <- df6_new[samp,]


  df3 <- tibble::tibble(x1=stats::rnorm(cluster_size, mean = 0, sd = 0.05), x2=stats::rnorm(cluster_size, mean = 1, sd = 0.05), x3=stats::rnorm(cluster_size, mean = 0, sd = 0.05), x4=stats::rnorm(cluster_size, mean = 0, sd = 0.05))

  df7_new <- (df1 + df3) / 2
  #get a sample of 10
  samp <- sample(nrow(df7_new), cluster_size * 0.20) ## 20% from the original dataset

  #data in the sample
  df7 <- df7_new[samp,]

  df4 <- tibble::tibble(x1=stats::rnorm(cluster_size * 0.6, mean = 0, sd = 0.5), x2=stats::rnorm(cluster_size * 0.6, mean = 0, sd = 0.5), x3=stats::rnorm(cluster_size * 0.6, mean = 0, sd = 0.5), x4=stats::rnorm(cluster_size * 0.6, mean = 0, sd = 0.5))


  df <- rbind(df1, df2, df3, df6, df7, df4)

  if (num_noise_dims != 0) {

    noise_mat <- gen_noise_dims(n = dim(df)[1], num_noise_dims = num_noise_dims,
                                min_noise = min_noise, max_noise = max_noise)
    df <- cbind(df, noise_mat)

    df

  } else {

    df

  }

}


two_nonlinear_with_noise <- function(sample_size = 200, with_seed = NULL, num_of_noise_dim = 8,
                                     min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by three
  if ((sample_size%%2) != 0) {
    warning("The sample size should be a product of number of clusters.")
    cluster_size <- floor(sample_size/2)

  } else {
    cluster_size <- sample_size/2
  }


  x <- stats::runif(cluster_size, -8, 1.5)
  y <- -(exp(x) + stats::runif(cluster_size, 0, 1)) + stats::runif(cluster_size, 0, 0.7)

  z <- -(exp(x) + stats::runif(cluster_size, 0, 1)) + stats::runif(cluster_size, 0, 0.7)
  w <- -(exp(x) + stats::runif(cluster_size, 0, 1)) + stats::runif(cluster_size, 0, 0.7)

  df1 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x <- stats::runif(cluster_size, -8, 1.5)
  y <- 3 - (exp(x) + stats::runif(cluster_size, 0, 1)) + stats::runif(cluster_size, 0, 0.7)

  z <- 3 - (exp(x) + stats::runif(cluster_size, 0, 1)) + stats::runif(cluster_size, 0, 0.7)
  w <- 3 - (exp(x) + stats::runif(cluster_size, 0, 1)) + stats::runif(cluster_size, 0, 0.7)

  df2 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  df <- rbind(df1, df2)

  if (num_noise_dims != 0) {

    noise_mat <- gen_noise_dims(n = dim(df)[1], num_noise_dims = num_noise_dims,
                                min_noise = min_noise, max_noise = max_noise)
    df <- cbind(df, noise_mat)

    df

  } else {

    df

  }

}

two_curvilinear_with_noise <- function(sample_size = 150, with_seed = NULL, num_of_noise_dim = 8,
                                       min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by three
  if ((sample_size%%2) != 0) {
    warning("The sample size should be a product of number of clusters.")
    cluster_size <- floor(sample_size/2)

  } else {
    cluster_size <- sample_size/2
  }


  theta = stats::runif(cluster_size, 0.20, 0.90 * pi)

  df1 <- tibble::tibble(
    x1 = cos(theta) + stats::rnorm(cluster_size, 1, 0.06),
    x2 = sin(theta) + stats::rnorm(cluster_size, 1, 0.06),

    x3 = cos(theta) + stats::rnorm(cluster_size, 1, 0.06),
    x4 = sin(theta) + stats::rnorm(cluster_size, 1, 0.06)
  )

  theta1 = stats::runif(cluster_size, 0.20, 0.90 * pi)

  df2 <- tibble::tibble(
    x1 = 1 + cos(theta1) + stats::rnorm(cluster_size, 1, 0.06),
    x2 = 1 + sin(theta1) + stats::rnorm(cluster_size, 1, 0.06),

    x3 = 1 + cos(theta1) + stats::rnorm(cluster_size, 1, 0.06),
    x4 = 1 + sin(theta1) + stats::rnorm(cluster_size, 1, 0.06)
  )

  df <- rbind(df1, df2)


  if (num_noise_dims != 0) {

    noise_mat <- gen_noise_dims(n = dim(df)[1], num_noise_dims = num_noise_dims,
                                min_noise = min_noise, max_noise = max_noise)
    df <- cbind(df, noise_mat)

    df

  } else {

    df

  }

}

two_curvilinear_diff_with_noise <- function(sample_size = 150, with_seed = NULL, num_of_noise_dim = 8,
                                            min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by three
  if ((sample_size%%2) != 0) {
    warning("The sample size should be a product of number of clusters.")
    cluster_size <- floor(sample_size/2)

  } else {
    cluster_size <- sample_size/2
  }


  theta = stats::runif(cluster_size, 0.40, 0.70 * pi)

  df1 <- tibble::tibble(
    x1 = cos(theta) + stats::rnorm(cluster_size, 1, 0.06),
    x2 = sin(theta) + stats::rnorm(cluster_size, 1, 0.06),

    x3 = cos(theta) + stats::rnorm(cluster_size, 1, 0.06),
    x4 = sin(theta) + stats::rnorm(cluster_size, 1, 0.06)
  )

  theta1 = stats::runif(cluster_size, 0.20, 0.90 * pi)

  df2 <- tibble::tibble(
    x1 = 1 + cos(theta1) + stats::rnorm(cluster_size, 1, 0.06),
    x2 = 1 + sin(theta1) + stats::rnorm(cluster_size, 1, 0.06),

    x3 = cos(theta1) + stats::rnorm(cluster_size, 1, 0.06),
    x4 = sin(theta1) + stats::rnorm(cluster_size, 1, 0.06)
  )

  df <- rbind(df1, df2)

  if (num_noise_dims != 0) {

    noise_mat <- gen_noise_dims(n = dim(df)[1], num_noise_dims = num_noise_dims,
                                min_noise = min_noise, max_noise = max_noise)
    df <- cbind(df, noise_mat)

    df

  } else {

    df

  }

}

two_linear_diff_with_noise <- function(sample_size = 150, with_seed = NULL, num_of_noise_dim = 8,
                                       min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by three
  if ((sample_size%%3) != 0) {
    warning("The sample size should be a product of number of clusters.")
    cluster_size <- floor(sample_size/3)

  } else {
    cluster_size <- sample_size/3
  }


  df_2_split <- snedata::long_cluster_data(n = cluster_size) %>%
    group_by(color) %>%
    group_split()

  df_2_split_1 <- df_2_split[[1]]
  df_2_split_1$x <- df_2_split_1$x - 20
  df_2_split_1$y <- df_2_split_1$y - 20

  df_2_split_3 <- df_2_split[[1]]
  df_2_split_3$x <- df_2_split_3$x + 10
  df_2_split_3$y <- df_2_split_3$y + 10

  df <- rbind(df_2_split_1, df_2_split[[2]], df_2_split_3) %>%
    dplyr::select(-color)

  names(df) <- paste0(rep("x",2), 1:2)

  if (num_noise_dims != 0) {

    noise_mat <- gen_noise_dims(n = dim(df)[1], num_noise_dims = num_noise_dims,
                                min_noise = min_noise, max_noise = max_noise)
    df <- cbind(df, noise_mat)

    df

  } else {

    df

  }

}


three_linear_with_noise <- function(sample_size = 150, with_seed = NULL, num_of_noise_dim = 8,
                                    min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by three
  if ((sample_size%%3) != 0) {
    warning("The sample size should be a product of number of clusters.")
    cluster_size <- floor(sample_size/3)

  } else {
    cluster_size <- sample_size/3
  }


  df_2_split <- snedata::long_cluster_data(n = cluster_size) %>%
    group_by(color) %>%
    group_split()

  df_2_split_1 <- df_2_split[[1]]
  df_2_split_1$x <- df_2_split_1$x - 20
  df_2_split_1$y <- df_2_split_1$y - 20

  df_2_split_3 <- df_2_split[[1]]
  df_2_split_3$x <- df_2_split_3$x - 10
  df_2_split_3$y <- df_2_split_3$y + 10

  df <- rbind(df_2_split_1, df_2_split[[2]], df_2_split_3) %>%
    dplyr::select(-color)

  names(df) <- paste0(rep("x",2), 1:2)

  if (num_noise_dims != 0) {

    noise_mat <- gen_noise_dims(n = dim(df)[1], num_noise_dims = num_noise_dims,
                                min_noise = min_noise, max_noise = max_noise)
    df <- cbind(df, noise_mat)

    df

  } else {

    df

  }

}

three_nonlinear_with_noise <- function(sample_size = 150, with_seed = NULL, num_of_noise_dim = 8,
                                       min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by three
  if ((sample_size%%3) != 0) {
    warning("The sample size should be a product of number of clusters.")
    cluster_size <- floor(sample_size/3)

  } else {
    cluster_size <- sample_size/3
  }


  phi <- stats::runif(cluster_size, max = 2*pi)
  rho <- sqrt(stats::runif(cluster_size))

  theta = stats::runif(cluster_size, 0,1.80 * pi)
  x = theta
  y = sin(theta)

  df1 <- tibble::tibble(x1=x, x2=y, x3=sqrt(1)*rho*cos(phi) + 4, x4=sqrt(1)*rho*sin(phi) + 4)
  df2 <- tibble::tibble(x1=x+1, x2=y+1, x3=sqrt(1)*rho*cos(phi) + 6, x4=sqrt(1)*rho*sin(phi) + 6)
  df3 <- tibble::tibble(x1=x-1, x2=y-1, x3=sqrt(1)*rho*cos(phi) + 8, x4=sqrt(1)*rho*sin(phi) + 8)

  df <- rbind(df1, df2, df3)

  if (num_noise_dims != 0) {

    noise_mat <- gen_noise_dims(n = dim(df)[1], num_noise_dims = num_noise_dims,
                                min_noise = min_noise, max_noise = max_noise)
    df <- cbind(df, noise_mat)

    df

  } else {

    df

  }

}

three_cluster_mirror_with_noise <- function(sample_size = 150, with_seed = NULL, num_of_noise_dim = 8,
                                            min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by three
  if ((sample_size%%6) != 0) {
    warning("The sample size should be a product of number of clusters.")
    cluster_size <- floor(sample_size/6)

  } else {
    cluster_size <- sample_size/6
  }


  df1 <- tibble::tibble(x1=stats::rnorm(cluster_size, mean = 0, sd = 0.05), x2=stats::rnorm(cluster_size, mean = 0, sd = 0.05), x3=stats::rnorm(cluster_size, mean = 0, sd = 0.05), x4=stats::rnorm(cluster_size, mean = 0, sd = 0.05))

  df2 <- tibble::tibble(x1=stats::rnorm(cluster_size, mean = 1, sd = 0.05), x2=stats::rnorm(cluster_size, mean = 0, sd = 0.05), x3=stats::rnorm(cluster_size, mean = 0, sd = 0.05), x4=stats::rnorm(cluster_size, mean = 0, sd = 0.05))

  df3 <- tibble::tibble(x1=stats::rnorm(cluster_size, mean = 0, sd = 0.05), x2=stats::rnorm(cluster_size, mean = 1, sd = 0.05), x3=stats::rnorm(cluster_size, mean = 0, sd = 0.05), x4=stats::rnorm(cluster_size, mean = 0, sd = 0.05))

  df_1 <- rbind(df1, df2, df3)

  df_2 <- df_1 + 2
  df <- rbind(df_1, df_2)

  if (num_noise_dims != 0) {

    noise_mat <- gen_noise_dims(n = dim(df)[1], num_noise_dims = num_noise_dims,
                                min_noise = min_noise, max_noise = max_noise)
    df <- cbind(df, noise_mat)

    df

  } else {

    df

  }

}

four_long_clusters_with_bkg_noise <- function(sample_size = 200, with_seed = NULL, num_of_noise_dim = 8,
                                              min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by three
  if ((sample_size%%5) != 0) {
    warning("The sample size should be a product of number of clusters.")
    cluster_size <- floor(sample_size/5)

  } else {
    cluster_size <- sample_size/5
  }


  df_2_split <- snedata::long_cluster_data(n = cluster_size) %>%
    group_by(color) %>%
    group_split()

  df_2_split_1 <- df_2_split[[1]]
  df_2_split_1$x <- df_2_split_1$x - 20
  df_2_split_1$y <- df_2_split_1$y - 20

  df_2_split_3 <- df_2_split[[1]]
  df_2_split_3$x <- df_2_split_3$x - 10
  df_2_split_3$y <- df_2_split_3$y + 10

  df_2_split_4 <- df_2_split[[1]]
  df_2_split_4$x <- df_2_split_4$x + 20
  df_2_split_4$y <- df_2_split_4$y + 30

  df1 <- rbind(df_2_split_1, df_2_split[[2]], df_2_split_3, df_2_split_4) %>%
    select(-color)

  names(df1) <- paste0(rep("x",2), 1:2)


  if (num_noise_dims != 0) {

    noise_mat <- gen_noise_dims(n = dim(df1)[1], num_noise_dims = num_noise_dims,
                                min_noise = min_noise, max_noise = max_noise)
    df1 <- cbind(df1, noise_mat)

  }

  ## To add background noise
  column_names_bkg <- paste0(rep("x", NCOL(df1)), 1:NCOL(df1))

  noise_bkg_val_list <- list()

  for (j in 1:NCOL(df1)) {
    noise_bkg_val_list[[column_names_bkg[j]]] <- stats::rnorm(cluster_size, mean = 0, sd = 10)


  }

  df2 <- tibble::as_tibble(noise_bkg_val_list)


  df <- rbind(df1, df2)

  df

}


curvy_branching_cluster_with_bkg_noise <- function(sample_size = 200, with_seed = NULL, num_of_noise_dim = 8,
                                                   min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by three
  if ((sample_size%%4) != 0) {
    warning("The sample size should be a product of number of clusters.")
    cluster_size <- floor(sample_size/4)

  } else {
    cluster_size <- sample_size/4
  }


  theta = stats::runif(cluster_size, 0.20, 0.90 * pi)

  df1 <- tibble::tibble(
    x1 = cos(theta) + stats::rnorm(cluster_size, 1, 0.06),
    x2 = sin(theta) + stats::rnorm(cluster_size, 1, 0.06),

    x3 = cos(theta) + stats::rnorm(cluster_size, 1, 0.06),
    x4 = sin(theta) + stats::rnorm(cluster_size, 1, 0.06)
  )

  theta1 = stats::runif(cluster_size, 0.20, 0.90 * pi)

  df2 <- tibble::tibble(
    x1 = cos(-theta1) + stats::rnorm(cluster_size, 1, 0.06),
    x2 = sin(-theta1) + stats::rnorm(cluster_size, 1, 0.06),

    x3 = cos(-theta1) + stats::rnorm(cluster_size, 1, 0.06),
    x4 = sin(-theta1) + stats::rnorm(cluster_size, 1, 0.06)
  )


  df3 <- tibble::tibble(x1 = stats::rnorm(cluster_size, mean = 1, sd = 0.08), x2 = stats::rnorm(cluster_size, mean = 1, sd = 0.08), x3=stats::rnorm(cluster_size, mean = 1, sd = 0.08), x4=stats::rnorm(cluster_size, mean = 1, sd = 0.08))

  df4 <- tibble::tibble(x1 = stats::rnorm(cluster_size, mean = 1, sd = 1), x2 = stats::rnorm(cluster_size, mean = 1, sd = 1), x3=stats::rnorm(cluster_size, mean = 1, sd = 1), x4=stats::rnorm(cluster_size, mean = 1, sd = 1))


  df <- rbind(df1, df2, df3, df4)

  if (num_noise_dims != 0) {

    noise_mat <- gen_noise_dims(n = dim(df)[1], num_noise_dims = num_noise_dims,
                                min_noise = min_noise, max_noise = max_noise)
    df <- cbind(df, noise_mat)

    df

  } else {

    df

  }

}

gaussian_clusters_diff_points <- function(n = 400, cluster_size_vec = c(50, 100, 200, 50), with_seed = NULL, num_clusters = 4, mean_matrix = rbind(c(1,0,0,0,0,0), c(0,1,0,0,0,0), c(0,0,1,0,0,0), c(0,0,0,1,0,0)),
                                          var_vec = c(0.02, 0.05, 0.06, 0.1), num_dims = 6, num_noise_dims = 4,
                                          min_noise = -0.05, max_noise = 0.05) {

  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  if (n < num_clusters) {
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

  # # To check that the assigned n is divided by three
  # if ((n%%num_clusters) != 0) {
  #   warning("The sample size should be a product of number of clusters.")
  #   cluster_size <- floor(n/num_clusters)
  #
  # } else {
  #   cluster_size <- n/num_clusters
  # }

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

    num_points_cluster <- cluster_size_vec[i]

    # Initialize an empty list to store the vectors with column
    # values
    dim_val_list <- list()

    for (j in 1:num_dims) {

      dim_val_list[[column_names[j]]] <- stats::rnorm(num_points_cluster, mean = mean_val_for_cluster[j],
                                                      sd = variance_val_for_cluster)

    }
    # To generate a tibble for a cluster
    df_cluster <- tibble::as_tibble(dim_val_list)

    df <- rbind(df, df_cluster)

  }

  if (num_noise_dims != 0) {

    noise_mat <- gen_noise_dims(n = dim(df)[1], num_noise_dims = num_noise_dims,
                                min_noise = min_noise, max_noise = max_noise)
    df <- cbind(df, noise_mat)

    df

  } else {

    df

  }

}

#' Generate Cluster and Curvilinear Data with Noise
#'
#' This function generates data with two clusters, one following a curvilinear pattern and the other distributed randomly.
#'
#' @param sample_size The total number of data points to be generated.
#' @param cluster_size_vec A vector specifying the number of points for each cluster.
#'                         If not provided, the sample_size is divided equally
#'                         between the two clusters.
#' @param num_noise_dims The number of additional noise dimensions to be generated.
#' @param min_noise The minimum value for the noise added to the data points.
#' @param max_noise The maximum value for the noise added to the data points.
#'
#' @return A matrix containing the generated data, with each row representing a data point.
#' @export
#'
#' @examples
#'
#' # Generate cluster and curvilinear data with custom parameters
#' data <- cluster_and_curvilinear_with_noise(sample_size = 300,
#' cluster_size_vec = c(100, 200), num_noise_dims = 3, min_noise = -0.05,
#' max_noise = 0.05)
cluster_and_curvilinear_with_noise <- function(sample_size, cluster_size_vec = NULL,
                                               num_noise_dims, min_noise, max_noise) {

  ## If the number of points for each cluster is not defined
  if (is.null(cluster_size_vec)) {

    # To check that the assigned sample_size is divided by two
    if ((sample_size%%2) != 0) {
      warning("The sample size should be a product of two.")
      cluster_size <- floor(sample_size/2)
      cluster_size_vec <- append(cluster_size, (sample_size - cluster_size))

    } else {
      cluster_size <- sample_size/2
      cluster_size_vec <- rep(cluster_size, 2)
    }

  }

  theta = stats::runif(cluster_size_vec[1], 0.20,0.60 * pi)
  x = cos(theta) + stats::rnorm(cluster_size_vec[1], 10, 0.03)
  y = sin(theta) + stats::rnorm(cluster_size_vec[1], 10, 0.03)
  z <- rep(0, cluster_size_vec[1]) + stats::rnorm(cluster_size_vec[1], 10, 0.03)
  w <- rep(0, cluster_size_vec[1]) - stats::rnorm(cluster_size_vec[1], 10, 0.03)

  df1 <- matrix(c(x, y, z, w), ncol = 4)

  x = stats::rnorm(cluster_size_vec[2], 10, 0.05)
  y = stats::rnorm(cluster_size_vec[2], 10, 0.05)
  z <- rep(0, cluster_size_vec[2]) + stats::rnorm(cluster_size_vec[2], 10, 0.05)
  w <- rep(0, cluster_size_vec[2]) - stats::rnorm(cluster_size_vec[2], 10, 0.05)

  df2 <- matrix(c(x, y, z, w), ncol = 4)

  df <- rbind(df1, df2)

  if (num_noise_dims != 0) {

    noise_mat <- gen_noise_dims(n = dim(df)[1], num_noise_dims = num_noise_dims,
                                min_noise = min_noise, max_noise = max_noise)
    df <- cbind(df, noise_mat)

    df

  } else {

    df

  }

}

#' Generate Curvy Branching Cluster Data
#'
#' This function generates curvy branching cluster data with three clusters of different shapes.
#'
#' @param sample_size The total number of data points to be generated.
#' @param cluster_size_vec A vector specifying the number of points for each cluster.
#'                         If not provided, the sample_size is divided equally
#'                         among the clusters.
#' @param num_noise_dims The number of additional noise dimensions to be generated.
#' @param min_noise The minimum value for the noise added to the data points.
#' @param max_noise The maximum value for the noise added to the data points.
#'
#' @return A matrix containing the generated data, with each row representing a data point.
#' @export
#'
#' @examples
#'
#' # Generate curvy branching cluster data with custom parameters
#' data <- curvy_branching_cluster(sample_size = 300, cluster_size_vec = c(100, 150, 50),
#' num_noise_dims = 6, min_noise = -0.05, max_noise = 0.05)
curvy_branching_cluster <- function(sample_size, cluster_size_vec = NULL,
                                    num_noise_dims, min_noise, max_noise) {
  ## If the number of points for each cluster is not defined
  if (is.null(cluster_size_vec)) {

    # To check that the assigned sample_size is divided by three
    if ((sample_size%%3) != 0) {
      warning("The sample size should be a product of three.")
      cluster_size <- floor(sample_size/3)
      cluster_size_vec <- append(rep(cluster_size, 2), (sample_size - cluster_size * 2))

    } else {
      cluster_size <- sample_size/3
      cluster_size_vec <- rep(cluster_size, 3)
    }

  }

  theta <- stats::runif(cluster_size_vec[1], 0.20, 0.90 * pi)

  df1 <- matrix(c(
    cos(theta) + stats::rnorm(cluster_size_vec[1], 1, 0.06),
    sin(theta) + stats::rnorm(cluster_size_vec[1], 1, 0.06),
    cos(theta) + stats::rnorm(cluster_size_vec[1], 1, 0.06),
    sin(theta) + stats::rnorm(cluster_size_vec[1], 1, 0.06)
  ), ncol = 4)


  theta1 <- stats::runif(cluster_size_vec[3], 0.20, 0.90 * pi)

  df2 <- matrix(c(
    cos(-theta1) + stats::rnorm(cluster_size_vec[3], 1, 0.06),
    sin(-theta1) + stats::rnorm(cluster_size_vec[3], 1, 0.06),
    cos(-theta1) + stats::rnorm(cluster_size_vec[3], 1, 0.06),
    sin(-theta1) + stats::rnorm(cluster_size_vec[3], 1, 0.06)
  ), ncol = 4)

  df3 <- matrix(c(stats::rnorm(cluster_size_vec[2], mean = 1, sd = 0.08),
                  stats::rnorm(cluster_size_vec[2], mean = 1, sd = 0.08),
                  stats::rnorm(cluster_size_vec[2], mean = 1, sd = 0.08),
                  stats::rnorm(cluster_size_vec[2], mean = 1, sd = 0.08)),
                ncol = 4)

  df <- rbind(df1, df2, df3)

  if (num_noise_dims != 0) {

    noise_mat <- gen_noise_dims(n = dim(df)[1], num_noise_dims = num_noise_dims,
                                min_noise = min_noise, max_noise = max_noise)
    df <- cbind(df, noise_mat)

    df

  } else {

    df

  }


}


clusters_different_shapes_diff_num_points <- function(sample_size = 400, with_seed = NULL, cluster_size_vec = c(50, 50, 50, 50, 100, 100), num_gussian_clusters = 4, num_non_gaussian_clusters = 2,
                                                      cluster_sd_gau = 0.05, cluster_sd_non_gau = 0.1, num_dims = 7, a = 2, b = 4) {


  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  num_clusters <- num_gussian_clusters + num_non_gaussian_clusters

  ## Generate Gaussian clusters

  # Create a vector of possible values (0 and 1)
  values <- c(0, 1)

  # Create an expanded grid with 0's and 1's
  mean_val_grid <- tidyr::expand_grid(!!!setNames(rep(list(values), num_dims),
                                                  paste0("mean_dim", 1:num_dims)))

  # To select combinations for assigned number of clusters

  mean_val_grid_gau <- mean_val_grid |>
    dplyr::slice_sample(n = num_gussian_clusters)

  mean_val_grid_non_gau <- mean_val_grid |>
    dplyr::slice_sample(n = num_non_gaussian_clusters)


  # To generate empty tibble
  column_names <- paste0(rep("x", num_dims), 1:num_dims)
  df <- tibble(!!!setNames(rep(list(NULL), length(column_names)), column_names))

  for (i in 1:num_gussian_clusters) {

    # To filter the mean values for specific cluster
    mean_val_for_cluster <- mean_val_grid_gau |>
      dplyr::filter(dplyr::row_number() == i) |>
      unlist(use.names = FALSE)

    # Initialize an empty list to store the vectors with column
    # values
    dim_val_list <- list()

    for (j in 1:num_dims) {

      dim_val_list[[column_names[j]]] <- stats::rnorm(cluster_size_vec[i], mean = mean_val_for_cluster[j],
                                               sd = cluster_sd_gau)

    }
    # To generate a tibble for a cluster
    df_gau_cluster <- tibble::as_tibble(dim_val_list)

    df <- rbind(df, df_gau_cluster)

  }

  for (i in 1:num_non_gaussian_clusters) {

    phi <- stats::runif(cluster_size_vec[(num_clusters - i)], max = 2*pi)
    rho <- sqrt(stats::runif(cluster_size_vec[(num_clusters - i)]))

    # To filter the mean values for specific cluster
    presence_of_elipse_cluster <- mean_val_grid_non_gau |>
      dplyr::filter(dplyr::row_number() == i) |>
      unlist(use.names = FALSE)

    # Initialize an empty list to store the vectors with column
    # values
    dim_val_list_n <- list()

    for (j in 1:num_dims) {
      if(presence_of_elipse_cluster[j] == 1){
        dim_val_list_n[[column_names[j]]] <- sqrt(a)*rho*cos(phi) + b
        ## Surface of poolar coordinate
      } else {
        dim_val_list_n[[column_names[j]]] <- stats::rnorm(cluster_size_vec[(num_clusters - i)], mean = 0,
                                                   sd = cluster_sd_non_gau)

      }



    }
    # To generate a tibble for a cluster
    df_non_gau_cluster <- tibble::as_tibble(dim_val_list_n)

    df <- rbind(df, df_non_gau_cluster)

  }

  df

}
