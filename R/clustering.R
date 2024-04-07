#' Generate synthetic data with Gaussian clusters
#'
#' Generate Gaussian Clusters
#'
#' This function generates Gaussian clusters with specified parameters.
#'
#' @param sample_size The total number of data points to be generated.
#' @param num_clusters The number of clusters to generate.
#' @param mean_matrix A matrix where each row represents the mean vector for a cluster.
#' @param var_vec A vector specifying the variance for each cluster.
#' @param num_dims The number of effective dimensions for the data points.
#' @param num_noise_dims The number of additional noise dimensions to be generated.
#' @param min_noise The minimum value for the noise added to the data points.
#' @param max_noise The maximum value for the noise added to the data points.
#'
#' @return A matrix containing the generated Gaussian clusters.
#' @export
#'
#' @examples
#'
#' gaussian_clusters(sample_size = 300, num_clusters = 5,
#' mean_matrix = rbind(c(1,0,0,0), c(0,1,0,0), c(0,0,1,0),
#' c(0,0,0,1), c(0,0,0,0)), var_vec = c(0.05, 0.05, 0.05, 0.05, 0.05),
#' num_dims = 4, num_noise_dims = 2, min_noise = -0.05, max_noise = 0.05)
gaussian_clusters <- function(sample_size, num_clusters,
                              mean_matrix, var_vec, num_dims, num_noise_dims,
                              min_noise, max_noise) {


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

  df <- matrix(nrow = 0, ncol = num_dims)

  for (i in 1:num_clusters) {

    # To filter the mean values for specific cluster
    mean_val_for_cluster <- mean_matrix[i, ]

    # To filter the variance values for specific cluster
    variance_val_for_cluster <- var_vec[i]

    # Initialize an empty list to store the vectors with column
    # values
    dim_val_list <- list()

    for (j in 1:num_dims) {

      dim_val_list[[j]] <- stats::rnorm(cluster_size, mean = mean_val_for_cluster[j],
                                                      sd = variance_val_for_cluster)

    }
    # To generate a tibble for a cluster
    df_cluster <- matrix(unlist(dim_val_list), ncol = length(dim_val_list))

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

#' Generate Gaussian Clusters with Different Points
#'
#' This function generates Gaussian clusters with different numbers of points per cluster.
#'
#' @param sample_size The total number of data points to be generated.
#' @param cluster_size_vec A vector specifying the number of points in each cluster.
#' @param num_clusters The number of clusters to generate.
#' @param mean_matrix A matrix where each row represents the mean vector for a cluster.
#' @param var_vec A vector specifying the variance for each cluster.
#' @param num_dims The number of effective dimensions for the data points.
#' @param num_noise_dims The number of additional noise dimensions to be generated.
#' @param min_noise The minimum value for the noise added to the data points.
#' @param max_noise The maximum value for the noise added to the data points.
#'
#' @return A matrix containing the generated Gaussian clusters with different points.
#' @export
#'
#' @examples
#'
#' # Generate Gaussian clusters with custom parameters
#' data <- gaussian_clusters_diff_points(sample_size = 400, cluster_size_vec = c(50, 100, 200, 50),
#'                                       num_clusters = 4, mean_matrix =
#'                                       rbind(c(1,0,0,0,0,0), c(0,1,0,0,0,0),
#'                                       c(0,0,1,0,0,0), c(0,0,0,1,0,0)),
#'                                       var_vec = c(0.02, 0.05, 0.06, 0.1),
#'                                       num_dims = 6, num_noise_dims = 4,
#'                                       min_noise = -0.05, max_noise = 0.05)
gaussian_clusters_diff_points <- function(sample_size, cluster_size_vec, num_clusters,
                                          mean_matrix, var_vec, num_dims,
                                          num_noise_dims, min_noise, max_noise) {

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

  df <- matrix(nrow = 0, ncol = num_dims)

  for (i in 1:num_clusters) {

    # To filter the mean values for specific cluster
    mean_val_for_cluster <- mean_matrix[i, ]

    # To filter the variance values for specific cluster
    variance_val_for_cluster <- var_vec[i]

    num_points_cluster <- cluster_size_vec[i]

    # Initialize an empty list to store the vectors with column
    # values
    dim_val_list <- list()

    for (j in 1:num_dims) {

      dim_val_list[[j]] <- stats::rnorm(num_points_cluster, mean = mean_val_for_cluster[j],
                                                      sd = variance_val_for_cluster)

    }
    # To generate a tibble for a cluster
    df_cluster <- matrix(unlist(dim_val_list), ncol = length(dim_val_list))

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


#' Generate Clusters with Different Shapes
#'
#' This function generates clusters with different shapes, including both Gaussian and non-Gaussian clusters.
#'
#' @param sample_size The total number of data points to be generated.
#' @param num_gussian_clusters The number of Gaussian clusters to generate.
#' @param num_non_gaussian_clusters The number of non-Gaussian clusters to generate.
#' @param cluster_sd_gau The standard deviation for the Gaussian clusters.
#' @param cluster_sd_non_gau The standard deviation for the non-Gaussian clusters.
#' @param num_dims The number of dimensions for the data points.
#' @param a The scaling factor for the non-Gaussian cluster shape.
#' @param b The translation factor for the non-Gaussian cluster shape.
#'
#' @return A matrix containing the generated clusters with different shapes.
#' @export
#'
#' @examples
#' # Generate clusters with default parameters
#' data <- clusters_different_shapes(sample_size = 300, num_gussian_clusters = 4,
#' num_non_gaussian_clusters = 2, cluster_sd_gau = 0.05, cluster_sd_non_gau = 0.1,
#' num_dims = 7, a = 2, b = 4)
clusters_different_shapes <- function(sample_size, num_gussian_clusters,
                                      num_non_gaussian_clusters, cluster_sd_gau,
                                      cluster_sd_non_gau, num_dims, a, b) {


  num_clusters <- num_gussian_clusters + num_non_gaussian_clusters

  # To check that the assigned sample_size is divided by the number of clusters.
  if ((sample_size%%num_clusters) != 0) {
    warning("The sample size should be a product of number of clusters.")
    cluster_size <- floor(sample_size/num_clusters)

  } else {
    cluster_size <- sample_size/num_clusters
  }

  ## Generate Gaussian clusters

  # Create a vector of possible values (0 and 1)
  values <- c(0, 1)

  # Create an expanded grid with 0's and 1's
  mean_val_grid <- as.matrix(expand.grid(rep(list(values), num_dims)))

  # To select combinations for assigned number of clusters

  mean_val_grid_gau <- mean_val_grid[sample(NROW(mean_val_grid),
                                            size=num_gussian_clusters, replace=FALSE),]

  mean_val_grid_non_gau <- mean_val_grid[sample(NROW(mean_val_grid),
          size=num_non_gaussian_clusters, replace=FALSE),]

  df <- matrix(nrow = 0, ncol = num_dims)

  for (i in 1:num_gussian_clusters) {

    # To filter the mean values for specific cluster
    mean_val_for_cluster <- mean_val_grid_gau[i, ]

    # Initialize an empty list to store the vectors with column
    # values
    dim_val_list <- list()

    for (j in 1:num_dims) {

      dim_val_list[[j]] <- stats::rnorm(cluster_size, mean = mean_val_for_cluster[j],
                                               sd = cluster_sd_gau)

    }
    # To generate a tibble for a cluster
    df_gau_cluster <- matrix(unlist(dim_val_list), ncol = length(dim_val_list))

    df <- rbind(df, df_gau_cluster)

  }

  phi <- stats::runif(cluster_size, max = 2*pi)
  rho <- sqrt(stats::runif(cluster_size))

  for (i in 1:num_non_gaussian_clusters) {

    # To filter the mean values for specific cluster
    presence_of_elipse_cluster <- mean_val_grid_non_gau[i, ]

    # Initialize an empty list to store the vectors with column
    # values
    dim_val_list_n <- list()

    for (j in 1:num_dims) {
      if(presence_of_elipse_cluster[j] == 1){
        dim_val_list_n[[j]] <- sqrt(a)*rho*cos(phi) + b
        ## Surface of poolar coordinate
      } else {
        dim_val_list_n[[j]] <- stats::rnorm(cluster_size, mean = 0,
                                                   sd = cluster_sd_non_gau)

      }



    }
    # To generate a tibble for a cluster
    df_non_gau_cluster <- matrix(unlist(dim_val_list_n), ncol = length(dim_val_list_n))

    df <- rbind(df, df_non_gau_cluster)

  }

  df

}

#' Generate Clusters with Different Shapes and Different Number of Points
#'
#' This function generates clusters with different shapes, including both Gaussian and non-Gaussian clusters,
#' with different numbers of points in each cluster.
#'
#' @param sample_size The total number of data points to be generated.
#' @param cluster_size_vec A vector specifying the number of points for each cluster.
#' @param num_gussian_clusters The number of Gaussian clusters to generate.
#' @param num_non_gaussian_clusters The number of non-Gaussian clusters to generate.
#' @param cluster_sd_gau The standard deviation for the Gaussian clusters.
#' @param cluster_sd_non_gau The standard deviation for the non-Gaussian clusters.
#' @param num_dims The number of dimensions for the data points.
#' @param a The scaling factor for the non-Gaussian cluster shape.
#' @param b The translation factor for the non-Gaussian cluster shape.
#'
#' @return A matrix containing the generated clusters with different shapes and
#' different numbers of points.
#' @export
#'
#' @examples
#' # Generate clusters with default parameters
#' data <- clusters_different_shapes_diff_num_points(sample_size = 400,
#' cluster_size_vec = c(50, 50, 50, 50, 100, 100), num_gussian_clusters = 4,
#' num_non_gaussian_clusters = 2, cluster_sd_gau = 0.05, cluster_sd_non_gau = 0.1,
#' num_dims = 7, a = 2, b = 4)
clusters_different_shapes_diff_num_points <- function(sample_size, cluster_size_vec,
                                                      num_gussian_clusters,
                                                      num_non_gaussian_clusters,
                                                      cluster_sd_gau,
                                                      cluster_sd_non_gau, num_dims,
                                                      a, b) {


  num_clusters <- num_gussian_clusters + num_non_gaussian_clusters

  ## Generate Gaussian clusters

  # Create a vector of possible values (0 and 1)
  values <- c(0, 1)

  # Create an expanded grid with 0's and 1's
  mean_val_grid <- as.matrix(expand.grid(rep(list(values), num_dims)))

  # To select combinations for assigned number of clusters
  mean_val_grid_gau <- mean_val_grid[sample(NROW(mean_val_grid),
                                            size=num_gussian_clusters, replace=FALSE),]

  mean_val_grid_non_gau <- mean_val_grid[sample(NROW(mean_val_grid),
                                                size=num_non_gaussian_clusters, replace=FALSE),]

  df <- matrix(nrow = 0, ncol = num_dims)

  for (i in 1:num_gussian_clusters) {

    # To filter the mean values for specific cluster
    mean_val_for_cluster <- mean_val_grid_gau[i,]

    # Initialize an empty list to store the vectors with column
    # values
    dim_val_list <- list()

    for (j in 1:num_dims) {

      dim_val_list[[j]] <- stats::rnorm(cluster_size_vec[i], mean = mean_val_for_cluster[j],
                                                      sd = cluster_sd_gau)

    }
    # To generate a tibble for a cluster
    df_gau_cluster <- matrix(unlist(dim_val_list), ncol = length(dim_val_list))

    df <- rbind(df, df_gau_cluster)

  }

  for (i in 1:num_non_gaussian_clusters) {

    phi <- stats::runif(cluster_size_vec[(num_clusters - i)], max = 2*pi)
    rho <- sqrt(stats::runif(cluster_size_vec[(num_clusters - i)]))

    # To filter the mean values for specific cluster
    presence_of_elipse_cluster <- mean_val_grid_non_gau[i, ]

    # Initialize an empty list to store the vectors with column
    # values
    dim_val_list_n <- list()

    for (j in 1:num_dims) {
      if(presence_of_elipse_cluster[j] == 1){
        dim_val_list_n[[j]] <- sqrt(a)*rho*cos(phi) + b
        ## Surface of poolar coordinate
      } else {
        dim_val_list_n[[j]] <- stats::rnorm(cluster_size_vec[(num_clusters - i)], mean = 0,
                                                          sd = cluster_sd_non_gau)

      }



    }
    # To generate a tibble for a cluster
    df_non_gau_cluster <- matrix(unlist(dim_val_list_n), ncol = length(dim_val_list_n))

    df <- rbind(df, df_non_gau_cluster)

  }

  df

}

#' Generate Clusters and Curvilinear Data with Noise
#'
#' This function generates data with clusters and curvilinear patterns along with added background noise.
#'
#' @param sample_size The total number of data points to be generated.
#' @param num_noise_dims The number of additional noise dimensions to be generated.
#' @param min_noise The minimum value for the noise added to the data points.
#' @param max_noise The maximum value for the noise added to the data points.
#'
#' @return A matrix containing the generated data, with each row representing a data point.
#' @export
#'
#' @examples
#'
#' # Generate clusters and curvilinear data with noise with custom parameters
#' data <- cluster_and_curvilinear__with_noise_and_bkg_noise(sample_size = 260,
#' num_noise_dims = 4, min_noise = -0.05, max_noise = 0.05)
cluster_and_curvilinear__with_noise_and_bkg_noise <- function(sample_size,
                                                              num_noise_dims,
                                                              min_noise, max_noise) {

  # To check that the assigned sample_size is divided by two
  if ((sample_size%%2) != 0) {
    stop("The sample size should be a product of two.")

  } else {
    cluster_size <- (sample_size - sample_size * 0.3)/2
  }

  theta <- stats::runif(cluster_size, 0.20,0.60 * pi)
  x <- cos(theta) + stats::rnorm(cluster_size, 10, 0.03)
  y <- sin(theta) + stats::rnorm(cluster_size, 10, 0.03)
  z <- rep(0, cluster_size) + stats::rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - stats::rnorm(cluster_size, 10, 0.03)
  df1 <- matrix(c(x, y, z, w), ncol = 4)

  x <- stats::rnorm(cluster_size, 10, 0.05)
  y <- stats::rnorm(cluster_size, 10, 0.05)
  z <- rep(0, cluster_size) + stats::rnorm(cluster_size, 10, 0.05)
  w <- rep(0, cluster_size) - stats::rnorm(cluster_size, 10, 0.05)
  df2 <- matrix(c(x, y, z, w), ncol = 4)

  x <- stats::rnorm(sample_size * 0.3, 11, 0.5)
  y <- stats::rnorm(sample_size * 0.3, 11, 0.5)
  z <- rep(0, sample_size * 0.3) + stats::rnorm(sample_size * 0.3, 10, 0.05)
  w <- rep(0, sample_size * 0.3) - stats::rnorm(sample_size * 0.3, 10, 0.05)
  df3 <- matrix(c(x, y, z, w), ncol = 4)

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

#' Generate Doublets with Noise
#'
#' This function generates data with one set of doublets (pairs of clusters)
#' along with added background noise.
#'
#' @param sample_size The total number of data points to be generated.
#' @param num_noise_dims The number of additional noise dimensions to be generated.
#' @param min_noise The minimum value for the noise added to the data points.
#' @param max_noise The maximum value for the noise added to the data points.
#'
#' @return A matrix containing the generated data, with each row representing a data point.
#' @export
#'
#' @examples
#'
#' # Generate doublets with noise with custom parameters
#' data <- one_doublet_with_noise(sample_size = 220, num_noise_dims = 4,
#' min_noise = -0.05, max_noise = 0.05)
one_doublet_with_noise <- function(sample_size, num_noise_dims, min_noise,
                                   max_noise) {


  # To check that the assigned sample_size is divided by 2.2
  if (((sample_size * 10)%%22) != 0) { #sample_size%%2.2
    stop("The sample size should be a product of 2.2.")

  } else {
    cluster_size <- (sample_size * 10)/22
  }


  df1 <- matrix(c(stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05)), ncol = 4)

  df2 <- matrix(c(stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05)), ncol = 4)

  df3_new <- (df1 + df2) / 2
  #get a sample of 10
  samp <- sample(NROW(df3_new), cluster_size * 0.20) ## 20% from the original dataset

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

#' Generate Doublets with Three Clusters and Noise
#'
#' This function generates data with three sets of doublets (pairs of clusters)
#' along with added background noise.
#'
#' @param sample_size The total number of data points to be generated.
#' @param num_noise_dims The number of additional noise dimensions to be generated.
#' @param min_noise The minimum value for the noise added to the data points.
#' @param max_noise The maximum value for the noise added to the data points.
#'
#' @return A matrix containing the generated data, with each row representing a data point.
#' @export
#'
#' @examples
#'
#' # Generate doublets with three clusters and noise with custom parameters
#' data <- three_doublets_with_noise(sample_size = 420, num_noise_dims = 4,
#' min_noise = -0.05, max_noise = 0.05)
three_doublets_with_noise <- function(sample_size, num_noise_dims, min_noise,
                                      max_noise) {

  # To check that the assigned sample_size is divided by 4.2
  if ((sample_size%%4.2) != 0) {
    warning("The sample size should be a product of 4.2.")
    cluster_size <- floor(sample_size/4.2)

  } else {
    cluster_size <- sample_size/4.2
  }


  df1 <- matrix(c(stats::rnorm(cluster_size, mean = 3, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 1, sd = 0.05)), ncol = 10)

  df2 <- matrix(c(stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 1, sd = 0.05)), ncol = 10)

  df3_new <- (df1 + df2) / 2
  #get a sample of 10
  samp <- sample(NROW(df3_new), cluster_size * 0.40) ## 20% from the original dataset

  #data in the sample
  df3 <- df3_new[samp,]

  df4 <- matrix(c(stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 3, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 1, sd = 0.05)), ncol = 10)

  df5_new <- (df2 + df4) / 2

  #get a sample of 10
  samp1 <- sample(NROW(df5_new), cluster_size * 0.30) ## 20% from the original dataset

  #data in the sample
  df5 <- df5_new[samp1,]

  df6_new <- (df1 + df4) / 2

  #get a sample of 10
  samp2 <- sample(NROW(df6_new), cluster_size * 0.50) ## 20% from the original dataset

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


#' Generate Doublets with Four Clusters and Noise
#'
#' This function generates data with one set of doublets (pairs of clusters) containing
#' four clusters, along with added background noise.
#'
#' @param sample_size The total number of data points to be generated.
#' @param num_noise_dims The number of additional noise dimensions to be generated.
#' @param min_noise The minimum value for the noise added to the data points.
#' @param max_noise The maximum value for the noise added to the data points.
#'
#' @return A matrix containing the generated data, with each row representing a data point.
#' @export
#'
#' @examples
#'
#' # Generate doublets with four clusters and noise with custom parameters
#' data <- one_doublet_four_clusters_with_noise(sample_size = 440, num_noise_dims = 4,
#' min_noise = -0.05, max_noise = 0.05)
one_doublet_four_clusters_with_noise <- function(sample_size, num_noise_dims,
                                                 min_noise, max_noise) {

  # To check that the assigned sample_size is divided by 4.4
  if (((sample_size * 10)%%44) != 0) { #sample_size%%4.4
    stop("The sample size should be a product of 4.4.")

  } else {
    cluster_size <- (sample_size * 10)/44
  }


  df1 <- matrix(c(stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 1, sd = 0.05)), ncol = 7)

  df2 <- matrix(c(stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05)), ncol = 7)

  df3_new <- (df1 + df2) / 2
  #get a sample of 10
  samp <- sample(NROW(df3_new), cluster_size * 0.40) ## 20% from the original dataset

  #data in the sample
  df3 <- df3_new[samp,]

  df4 <- matrix(c(stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05)), ncol = 7)


  df5 <- matrix(c(stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05)), ncol = 7)

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

#' Generate Doublets with Different Variance Clusters and Noise
#'
#' This function generates data with one set of doublets (pairs of clusters) having clusters with different variance, along with added background noise.
#'
#' @param sample_size The total number of data points to be generated.
#' @param num_noise_dims The number of additional noise dimensions to be generated.
#' @param min_noise The minimum value for the noise added to the data points.
#' @param max_noise The maximum value for the noise added to the data points.
#'
#' @return A matrix containing the generated data, with each row representing a data point.
#' @export
#'
#' @examples
#'
#' # Generate doublets with different variance clusters and noise with custom parameters
#' data <- one_doublet_dfifferent_var_clusters_with_noise(sample_size = 260, num_noise_dims = 2,
#' min_noise = -0.05, max_noise = 0.05)
one_doublet_dfifferent_var_clusters_with_noise <- function(sample_size, num_noise_dims,
                                                           min_noise, max_noise) {

  # To check that the assigned sample_size is divided by 2.6
  if (((sample_size * 10)%%26) != 0) {
    stop("The sample size should be a product of 2.6.")

  } else {
    cluster_size <- sample_size/2.6
  }


  df1 <- matrix(c(stats::rnorm(cluster_size, mean = 1, sd = 0.1),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.08),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.08),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.08),
                  stats::rnorm(cluster_size, mean = 1, sd = 0.08),
                  stats::rnorm(cluster_size, mean = 1, sd = 0.02),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.02),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.02)), ncol = 10)

  df2 <- matrix(c(stats::rnorm(cluster_size, mean = 0, sd = 0.02),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.02),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.02),
                  stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 1, sd = 0.02),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.02),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.02),
                  stats::rnorm(cluster_size, mean = 1, sd = 0.02),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.02),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.02)), ncol = 10)

  df3_new <- (df1 + df2) / 2
  #get a sample of 10
  samp <- sample(NROW(df3_new), cluster_size * 0.60) ## 20% from the original dataset

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

#' Generate Doublets with Different Pattern Clusters and Noise
#'
#' This function generates data with one set of doublets (pairs of clusters)
#' having different patterns, along with added background noise.
#'
#' @param sample_size The total number of data points to be generated.
#' @param num_noise_dims The number of additional noise dimensions to be generated.
#' @param min_noise The minimum value for the noise added to the data points.
#' @param max_noise The maximum value for the noise added to the data points.
#'
#' @return A matrix containing the generated data, with each row representing a data point.
#' @export
#'
#' @examples
#'
#' # Generate doublets with different pattern clusters and noise with custom parameters
#' data <- one_doublet_dfifferent_pattern_clusters_with_noise(sample_size = 280,
#' num_noise_dims = 8, min_noise = -0.05, max_noise = 0.05)
one_doublet_dfifferent_pattern_clusters_with_noise <- function(sample_size, num_noise_dims,
                                                               min_noise, max_noise) {

  # To check that the assigned sample_size is divided by 2.8
  if ((sample_size%%2.8) != 0) {
    warning("The sample size should be a product of 2.8.")
    cluster_size <- floor(sample_size/2.8)

  } else {
    cluster_size <- sample_size/2.8
  }


  theta = stats::runif(cluster_size, 0.20, 0.60 * pi)

  df1 <- matrix(c(
    cos(theta) + stats::rnorm(cluster_size, 1, 0.5),
    sin(theta) + stats::rnorm(cluster_size, 1, 0.03),
    cos(theta) + stats::rnorm(cluster_size, 1, 0.03),
    sin(theta) + stats::rnorm(cluster_size, 1, 0.03),
    cos(theta) + stats::rnorm(cluster_size, 1, 0.03),
    sin(theta) + stats::rnorm(cluster_size, 1, 0.03),
    cos(theta) + stats::rnorm(cluster_size, 1, 0.05),
    sin(theta) + stats::rnorm(cluster_size, 1, 0.03),
    cos(theta) + stats::rnorm(cluster_size, 1, 0.3),
    sin(theta) + stats::rnorm(cluster_size, 1, 0.03)), ncol = 10)

  df2 <- matrix(c(stats::rnorm(cluster_size, mean = 1, sd = 0.1),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.08),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.08),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.08),
                  stats::rnorm(cluster_size, mean = 1, sd = 0.08),
                  stats::rnorm(cluster_size, mean = 1, sd = 0.02),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.02),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.02)), ncol = 10)


  df3_new <- (df1 + df2) / 2
  #get a sample of 10
  samp <- sample(NROW(df3_new), cluster_size * 0.80) ## 20% from the original dataset

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

#' Generate Doublets in Parallel with Noise
#'
#' This function generates data with two sets of doublets (pairs of clusters)
#' running in parallel, along with added background noise.
#'
#' @param sample_size The total number of data points to be generated.
#' @param num_noise_dims The number of additional noise dimensions to be generated.
#' @param min_noise The minimum value for the noise added to the data points.
#' @param max_noise The maximum value for the noise added to the data points.
#'
#' @return A matrix containing the generated data, with each row representing a data point.
#' @export
#'
#' @examples
#'
#' # Generate doublets in parallel with noise with custom parameters
#' data <- two_doublets_parallel_with_noise(sample_size = 440, num_noise_dims = 2,
#' min_noise = -0.05, max_noise = 0.05)
two_doublets_parallel_with_noise <- function(sample_size, num_noise_dims, min_noise,
                                             max_noise) {


  # To check that the assigned sample_size is divided by 4.4
  if (((sample_size * 10)%%44) != 0) { #sample_size%%4.4
    stop("The sample size should be a product of 4.4.")

  } else {
    cluster_size <- (sample_size * 10)/44
  }


  df1 <- matrix(c(stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05)), ncol = 10)

  df2 <- matrix(c(stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05)), ncol = 10)

  df3_new <- (df1 + df2) / 2
  #get a sample of 10
  samp <- sample(NROW(df3_new), cluster_size * 0.20) ## 20% from the original dataset

  #data in the sample
  df3 <- df3_new[samp,]

  df4 <- matrix(c(stats::rnorm(cluster_size, mean = -1, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = -1, sd = 0.05),
                  stats::rnorm(cluster_size, mean = -1, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05)), ncol = 10)

  df5 <- matrix(c(stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = -1, sd = 0.05),
                  stats::rnorm(cluster_size, mean = -1, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = -1, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05)), ncol = 10)

  df6_new <- (df4 + df5) / 2
  #get a sample of 10
  samp1 <- sample(NROW(df6_new), cluster_size * 0.20) ## 20% from the original dataset

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

#' Generate Doublets with Background Noise
#'
#' This function generates data with doublets (pairs of clusters) along with
#' added background noise.
#'
#' @param sample_size The total number of data points to be generated.
#' @param num_noise_dims The number of additional noise dimensions to be generated.
#' @param min_noise The minimum value for the noise added to the data points.
#' @param max_noise The maximum value for the noise added to the data points.
#'
#' @return A matrix containing the generated data, with each row representing a data point.
#' @export
#'
#' @examples
#'
#' # Generate doublets with background noise with custom parameters
#' data <- one_doublets_with_bkg_noise(sample_size = 250, num_noise_dims = 4,
#' min_noise = -0.05, max_noise = 0.05)
one_doublets_with_bkg_noise <- function(sample_size, num_noise_dims, min_noise,
                                        max_noise) {

  # To check that the assigned sample_size is divided by 2.5
  if ((sample_size%%2.5) != 0) {
    warning("The sample size should be a product of 2.5.")
    cluster_size <- floor(sample_size/2.5)

  } else {
    cluster_size <- sample_size/2.5
  }


  df1 <- matrix(c(stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 1, sd = 0.05)), ncol = 7)

  df2 <- matrix(c(stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05)), ncol = 7)

  df3_new <- (df1 + df2) / 2
  #get a sample of 10
  samp <- sample(NROW(df3_new), cluster_size * 0.20) ## 20% from the original dataset

  #data in the sample
  df3 <- df3_new[samp,]

  df4_new <- matrix(c(stats::rnorm(cluster_size, mean = 0, sd = 0.2),
                      stats::rnorm(cluster_size, mean = 0, sd = 0.5),
                      stats::rnorm(cluster_size, mean = 0.5, sd = 0.5),
                      stats::rnorm(cluster_size, mean = 0.2, sd = 0.5),
                      stats::rnorm(cluster_size, mean = 0.2, sd = 0.3),
                      stats::rnorm(cluster_size, mean = 0, sd = 0.5),
                      stats::rnorm(cluster_size, mean = 0, sd = 0.3)), ncol = 7)

  #get a sample of 10
  samp1 <- sample(NROW(df4_new), cluster_size * 0.30) ## 20% from the original dataset

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

#' Generate Two Doublets with Background Noise
#'
#' This function generates data with two doublets along with added background noise.
#'
#' @param sample_size The total number of data points to be generated.
#' @param num_noise_dims The number of additional noise dimensions to be generated.
#' @param min_noise The minimum value for the noise added to the data points.
#' @param max_noise The maximum value for the noise added to the data points.
#'
#' @return A matrix containing the generated data, with each row representing a data point.
#' @export
#'
#' @examples
#'
#' # Generate two doublets with background noise with custom parameters
#' data <- two_doublets_with_bkg_noise(sample_size = 200, num_noise_dims = 4,
#' min_noise = -0.05, max_noise = 0.05)
two_doublets_with_bkg_noise <- function(sample_size, num_noise_dims, min_noise,
                                        max_noise) {

  # To check that the assigned sample_size is divided by four
  if ((sample_size%%4) != 0) {
    warning("The sample size should be a product of four.")
    cluster_size <- floor(sample_size/4)

  } else {
    cluster_size <- sample_size/4
  }

  df1 <- matrix(c(stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05)), ncol = 4)

  df2 <- matrix(c(stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05)), ncol = 4)

  df6_new <- (df1 + df2) / 2
  #get a sample of 10
  samp <- sample(NROW(df6_new), cluster_size * 0.20) ## 20% from the original dataset

  #data in the sample
  df6 <- df6_new[samp,]

  df3 <- matrix(c(stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                   stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                   stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                   stats::rnorm(cluster_size, mean = 0, sd = 0.05)), ncol = 4)

  df7_new <- (df1 + df3) / 2
  #get a sample of 10
  samp <- sample(NROW(df7_new), cluster_size * 0.20) ## 20% from the original dataset

  #data in the sample
  df7 <- df7_new[samp,]

  df4 <- matrix(c(stats::rnorm(cluster_size * 0.6, mean = 0, sd = 0.5),
                  stats::rnorm(cluster_size * 0.6, mean = 0, sd = 0.5),
                  stats::rnorm(cluster_size * 0.6, mean = 0, sd = 0.5),
                  stats::rnorm(cluster_size * 0.6, mean = 0, sd = 0.5)), ncol = 4)

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

#' Generate Two Nonlinear Clusters with Noise
#'
#' This function generates data with two nonlinear clusters along with added noise.
#'
#' @param sample_size The total number of data points to be generated.
#' @param num_noise_dims The number of additional noise dimensions to be generated.
#' @param min_noise The minimum value for the noise added to the data points.
#' @param max_noise The maximum value for the noise added to the data points.
#'
#' @return A matrix containing the generated data, with each row representing a data point.
#' @export
#'
#' @examples
#'
#' # Generate two nonlinear clusters with noise with custom parameters
#' data <- two_nonlinear_with_noise(sample_size = 200, num_noise_dims = 4,
#' min_noise = -0.05, max_noise = 0.050)
two_nonlinear_with_noise <- function(sample_size, num_noise_dims, min_noise,
                                     max_noise) {

  # To check that the assigned sample_size is divided by two
  if ((sample_size%%2) != 0) {
    warning("The sample size should be a product of two.")
    cluster_size <- floor(sample_size/2)

  } else {
    cluster_size <- sample_size/2
  }


  x <- stats::runif(cluster_size, -8, 1.5)
  y <- -(exp(x) + stats::runif(cluster_size, 0, 1)) + stats::runif(cluster_size, 0, 0.7)
  z <- -(exp(x) + stats::runif(cluster_size, 0, 1)) + stats::runif(cluster_size, 0, 0.7)
  w <- -(exp(x) + stats::runif(cluster_size, 0, 1)) + stats::runif(cluster_size, 0, 0.7)

  df1 <- matrix(c(x, y, z, w), ncol = 4)

  x <- stats::runif(cluster_size, -8, 1.5)
  y <- 3 - (exp(x) + stats::runif(cluster_size, 0, 1)) + stats::runif(cluster_size, 0, 0.7)
  z <- 3 - (exp(x) + stats::runif(cluster_size, 0, 1)) + stats::runif(cluster_size, 0, 0.7)
  w <- 3 - (exp(x) + stats::runif(cluster_size, 0, 1)) + stats::runif(cluster_size, 0, 0.7)

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

#' Generate Two Curvilinear Clusters with Noise
#'
#' This function generates data with two curvilinear clusters along with added noise.
#'
#' @param sample_size The total number of data points to be generated.
#' @param num_noise_dims The number of additional noise dimensions to be generated.
#' @param min_noise The minimum value for the noise added to the data points.
#' @param max_noise The maximum value for the noise added to the data points.
#'
#' @return A matrix containing the generated data, with each row representing a data point.
#' @export
#'
#' @examples
#'
#' # Generate two curvilinear clusters with noise with custom parameters
#' data <- two_curvilinear_with_noise(sample_size = 200, num_noise_dims = 4,
#' min_noise = -0.05, max_noise = 0.05)
two_curvilinear_with_noise <- function(sample_size, num_noise_dims, min_noise,
                                       max_noise) {

  # To check that the assigned sample_size is divided by two
  if ((sample_size%%2) != 0) {
    warning("The sample size should be a product of two.")
    cluster_size <- floor(sample_size/2)

  } else {
    cluster_size <- sample_size/2
  }

  theta <- stats::runif(cluster_size, 0.20, 0.90 * pi)
  df1 <- matrix(c(
    cos(theta) + stats::rnorm(cluster_size, 1, 0.06),
    sin(theta) + stats::rnorm(cluster_size, 1, 0.06),
    cos(theta) + stats::rnorm(cluster_size, 1, 0.06),
    sin(theta) + stats::rnorm(cluster_size, 1, 0.06)
  ), ncol = 4)

  theta1 <- stats::runif(cluster_size, 0.20, 0.90 * pi)
  df2 <- matrix(c(
    1 + cos(theta1) + stats::rnorm(cluster_size, 1, 0.06),
    1 + sin(theta1) + stats::rnorm(cluster_size, 1, 0.06),
    1 + cos(theta1) + stats::rnorm(cluster_size, 1, 0.06),
    1 + sin(theta1) + stats::rnorm(cluster_size, 1, 0.06)
  ), ncol = 4)

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

#' Generate Two Curvilinear Differentiated Clusters with Noise
#'
#' This function generates data with two curvilinear clusters that are differentiated from each other, along with added noise.
#'
#' @param sample_size The total number of data points to be generated.
#' @param num_noise_dims The number of additional noise dimensions to be generated.
#' @param min_noise The minimum value for the noise added to the data points.
#' @param max_noise The maximum value for the noise added to the data points.
#'
#' @return A matrix containing the generated data, with each row representing a data point.
#' @export
#'
#' @examples
#'
#' # Generate two curvilinear differentiated clusters with noise with custom parameters
#' data <- two_curvilinear_diff_with_noise(sample_size = 200, num_noise_dims = 4,
#' min_noise = -0.05, max_noise = 0.05)
two_curvilinear_diff_with_noise <- function(sample_size, num_noise_dims, min_noise,
                                            max_noise) {

  # To check that the assigned sample_size is divided by two
  if ((sample_size%%2) != 0) {
    warning("The sample size should be a product of two.")
    cluster_size <- floor(sample_size/2)

  } else {
    cluster_size <- sample_size/2
  }

  theta <- stats::runif(cluster_size, 0.40, 0.70 * pi)
  df1 <- matrix(c(
    cos(theta) + stats::rnorm(cluster_size, 1, 0.06),
    sin(theta) + stats::rnorm(cluster_size, 1, 0.06),
    cos(theta) + stats::rnorm(cluster_size, 1, 0.06),
    sin(theta) + stats::rnorm(cluster_size, 1, 0.06)
  ), ncol = 4)

  theta1 <- stats::runif(cluster_size, 0.20, 0.90 * pi)
  df2 <- matrix(c(
    1 + cos(theta1) + stats::rnorm(cluster_size, 1, 0.06),
    1 + sin(theta1) + stats::rnorm(cluster_size, 1, 0.06),
    cos(theta1) + stats::rnorm(cluster_size, 1, 0.06),
    sin(theta1) + stats::rnorm(cluster_size, 1, 0.06)
  ), ncol = 4)

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

#' Generate Three Nonlinear Clusters with Noise
#'
#' This function generates data with three nonlinear clusters, along with added noise.
#'
#' @param sample_size The total number of data points to be generated.
#' @param num_noise_dims The number of additional noise dimensions to be generated.
#' @param min_noise The minimum value for the noise added to the data points.
#' @param max_noise The maximum value for the noise added to the data points.
#'
#' @return A matrix containing the generated data, with each row representing a data point.
#' @export
#'
#' @examples
#'
#' # Generate three nonlinear clusters with noise with custom parameters
#' data <- three_nonlinear_with_noise(sample_size = 300, num_noise_dims = 4,
#' min_noise = -0.05, max_noise = 0.05)
three_nonlinear_with_noise <- function(sample_size, num_noise_dims, min_noise,
                                       max_noise) {

  # To check that the assigned sample_size is divided by three
  if ((sample_size%%3) != 0) {
    warning("The sample size should be a product of three.")
    cluster_size <- floor(sample_size/3)

  } else {
    cluster_size <- sample_size/3
  }

  phi <- stats::runif(cluster_size, max = 2*pi)
  rho <- sqrt(stats::runif(cluster_size))

  theta <- stats::runif(cluster_size, 0,1.80 * pi)
  x <- theta
  y <- sin(theta)

  df1 <- matrix(c(x, y, sqrt(1)*rho*cos(phi) + 4, sqrt(1)*rho*sin(phi) + 4), ncol = 4)
  df2 <- matrix(c(x+1, y+1, sqrt(1)*rho*cos(phi) + 6, sqrt(1)*rho*sin(phi) + 6), ncol = 4)
  df3 <- matrix(c(x-1, y-1, sqrt(1)*rho*cos(phi) + 8, sqrt(1)*rho*sin(phi) + 8), ncol = 4)

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

#' Generate Three Cluster Mirror with Noise
#'
#' This function generates data with three clusters forming a mirror image,
#' along with added noise.
#'
#' @param sample_size The total number of data points to be generated.
#' @param num_noise_dims The number of additional noise dimensions to be generated.
#' @param min_noise The minimum value for the noise added to the data points.
#' @param max_noise The maximum value for the noise added to the data points.
#'
#' @return A matrix containing the generated data, with each row representing a data point.
#' @export
#'
#' @examples
#'
#' # Generate three cluster mirror with noise with custom parameters
#' data <- three_cluster_mirror_with_noise(sample_size = 300, num_noise_dims = 4,
#' min_noise = -0.05, max_noise = 0.05)
three_cluster_mirror_with_noise <- function(sample_size, num_noise_dims, min_noise,
                                            max_noise) {

  # To check that the assigned sample_size is divided by six
  if ((sample_size%%6) != 0) {
    warning("The sample size should be a product of six.")
    cluster_size <- floor(sample_size/6)

  } else {
    cluster_size <- sample_size/6
  }


  df1 <- matrix(c(stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05)), ncol = 4)

  df2 <- matrix(c(stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05)), ncol = 4)

  df3 <- matrix(c(stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 1, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05),
                  stats::rnorm(cluster_size, mean = 0, sd = 0.05)), ncol = 4)

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


#' Generate three clusters of data points with optional noise.
#'
#' This function generates three clusters of data points along with optional noise.
#'
#' @param sample_size Total number of data points to generate, should be a multiple of three.
#' @param num_dims Number of dimensions for each data point.
#' @param num_noise_dims Number of additional noise dimensions to add to the data.
#' @param min_noise Minimum value for the noise added to the data.
#' @param max_noise Maximum value for the noise added to the data.
#'
#' @return A matrix containing the generated data points with or without added noise.
#'
#' @examples
#' three_clusters_data_with_noise(sample_size = 100, num_dims = 7,
#' num_noise_dims = 4, min_noise = -0.05, max_noise = 0.05)
#'
#' @export
three_clusters_data_with_noise <- function(sample_size, num_dims, num_noise_dims,
                                           min_noise, max_noise) {

  # To check that the assigned sample_size is divided by three
  if ((sample_size %% 3) != 0) {
    warning("The sample size should be a product of three.")
    cluster_size <- floor(sample_size/3)

  } else {
    cluster_size <- sample_size/3
  }

  df1 <- matrix(stats::rnorm(cluster_size * num_dims, sd = 1), ncol = num_dims)

  df2 <- matrix(stats::rnorm(cluster_size * num_dims, sd = 1), ncol = num_dims)
  df2[, 1] <- df2[, 1] + 10

  df3 <- matrix(stats::rnorm(cluster_size * num_dims, sd = 1), ncol = num_dims)
  df3[, 1] <- df3[, 1] + 50

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
