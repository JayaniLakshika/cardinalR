#' Generate synthetic data with Gaussian clusters
#'
#' Generate Gaussian Clusters
#'
#' This function generates Gaussian clusters with specified parameters.
#'
#' @param n The total number of data points to be generated.
#' @param num_clust The number of clusters to generate.
#' @param mean_matrix A matrix where each row represents the mean vector for a cluster.
#' @param var_vec A vector specifying the variance for each cluster.
#' @param num_dims The number of effective dimensions for the data points.
#' @param num_noise The number of additional noise dimensions to be generated.
#' @param min_n The minimum value for the noise added to the data points.
#' @param max_n The maximum value for the noise added to the data points.
#'
#' @return A matrix containing the generated Gaussian clusters.
#' @export
#'
#' @examples
#'
#' set.seed(20240412)
#' gau_clust(
#'   n = 300, num_clust = 5,
#'   mean_matrix = rbind(
#'     c(1, 0, 0, 0), c(0, 1, 0, 0), c(0, 0, 1, 0),
#'     c(0, 0, 0, 1), c(0, 0, 0, 0)
#'   ), var_vec = c(0.05, 0.05, 0.05, 0.05, 0.05),
#'   num_dims = 4, num_noise = 2, min_n = -0.05, max_n = 0.05
#' )
gau_clust <- function(n, num_clust, mean_matrix, var_vec, num_dims, num_noise,
                      min_n, max_n) {
  if (n <= 0) {
    stop("Number of points should be a positive number.")
  }

  if (num_clust < 0) {
    stop("Number of clusters should be a positive number.")
  }

  if (num_dims < 0) {
    stop("Number of effective dimensions should be a positive number.")
  }

  if (num_noise < 0) {
    stop("Number of noise dimensions should be a positive number.")
  }

  if (missing(n)) {
    stop("Missing n.")
  }

  if (missing(num_dims)) {
    stop("Missing num_dims.")
  }

  if (missing(num_noise)) {
    stop("Missing num_noise.")
  }

  if (missing(mean_matrix)) {
    stop("Missing mean_matrix.")
  }

  if (missing(var_vec)) {
    stop("Missing var_vec.")
  }

  if (n < num_clust) {
    stop("Number of clusters exceed the number of observations.")
  }

  if ((num_dims == 0) | (num_dims == 1)) {
    stop("There should be at least two dimensions.")
  }

  if (dim(mean_matrix)[1] != length(var_vec)) {
    stop("The length of mean and variance vectors are different.")
  }

  if (dim(mean_matrix)[1] != num_clust) {
    stop("There is not enough mean values for clusters.")
  }

  if (dim(mean_matrix)[2] != num_dims) {
    stop("There is not enough mean values for dimensions.")
  }

  if (length(var_vec) != num_clust) {
    stop("There is not enough varaiance values for clusters.")
  }

  # To check that the assigned n is divided by three
  if ((n %% num_clust) != 0) {
    warning("The sample size should be a product of number of clusters.")
    cluster_size <- floor(n / num_clust)
  } else {
    cluster_size <- n / num_clust
  }

  df <- matrix(nrow = 0, ncol = num_dims)

  for (i in 1:num_clust) {
    # To filter the mean values for specific cluster
    mean_val_for_cluster <- mean_matrix[i, ]

    # To filter the variance values for specific cluster
    variance_val_for_cluster <- var_vec[i]

    # Initialize an empty list to store the vectors with column
    # values
    dim_val_list <- list()

    for (j in 1:num_dims) {
      dim_val_list[[j]] <- stats::rnorm(cluster_size,
        mean = mean_val_for_cluster[j],
        sd = variance_val_for_cluster
      )
    }
    # To generate a tibble for a cluster
    df_cluster <- matrix(unlist(dim_val_list), ncol = length(dim_val_list))

    df <- rbind(df, df_cluster)
  }

  if (num_noise != 0) {
    if (missing(min_n)) {
      stop("Missing min_n.")
    }

    if (missing(max_n)) {
      stop("Missing max_n.")
    }

    noise_mat <- gen_noise_dims(
      n = dim(df)[1], num_noise = num_noise,
      min_n = min_n, max_n = max_n
    )
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
#' @param clust_size_vec A vector specifying the number of points in each cluster.
#' @param num_clust The number of clusters to generate.
#' @param mean_matrix A matrix where each row represents the mean vector for a cluster.
#' @param var_vec A vector specifying the variance for each cluster.
#' @param num_dims The number of effective dimensions for the data points.
#' @param num_noise The number of additional noise dimensions to be generated.
#' @param min_n The minimum value for the noise added to the data points.
#' @param max_n The maximum value for the noise added to the data points.
#'
#' @return A matrix containing the generated Gaussian clusters with different points.
#' @export
#'
#' @examples
#'
#' # Generate Gaussian clusters with custom parameters
#' set.seed(20240412)
#' data <- gau_clust_diff(
#'   clust_size_vec = c(50, 100, 200, 50),
#'   num_clust = 4, mean_matrix =
#'     rbind(
#'       c(1, 0, 0, 0, 0, 0), c(0, 1, 0, 0, 0, 0),
#'       c(0, 0, 1, 0, 0, 0), c(0, 0, 0, 1, 0, 0)
#'     ),
#'   var_vec = c(0.02, 0.05, 0.06, 0.1),
#'   num_dims = 6, num_noise = 4,
#'   min_n = -0.05, max_n = 0.05
#' )
gau_clust_diff <- function(clust_size_vec, num_clust, mean_matrix, var_vec,
                           num_dims, num_noise, min_n, max_n) {
  if (length(clust_size_vec) != num_clust) {
    stop("Length of clust_size_vec should be equal to number of clusters.")
  }

  if (num_clust < 0) {
    stop("Number of clusters should be a positive number.")
  }

  if (num_dims < 0) {
    stop("Number of effective dimensions should be a positive number.")
  }

  if (num_noise < 0) {
    stop("Number of noise dimensions should be a positive number.")
  }

  if (missing(clust_size_vec)) {
    stop("Missing clust_size_vec.")
  }

  if (missing(num_dims)) {
    stop("Missing num_dims.")
  }

  if (missing(num_noise)) {
    stop("Missing num_noise.")
  }

  if (missing(mean_matrix)) {
    stop("Missing mean_matrix.")
  }

  if (missing(var_vec)) {
    stop("Missing var_vec.")
  }

  n <- sum(clust_size_vec)

  if (n < num_clust) {
    stop("Number of clusters exceed the number of observations.")
  }

  if ((num_dims == 0) | (num_dims == 1)) {
    stop("There should be at least two dimensions.")
  }

  if (dim(mean_matrix)[1] != length(var_vec)) {
    stop("The length of mean and variance vectors are different.")
  }

  if (dim(mean_matrix)[1] != num_clust) {
    stop("There is not enough mean values for clusters.")
  }

  if (dim(mean_matrix)[2] != num_dims) {
    stop("There is not enough mean values for dimensions.")
  }

  if (length(var_vec) != num_clust) {
    stop("There is not enough varaiance values for clusters.")
  }

  df <- matrix(nrow = 0, ncol = num_dims)

  for (i in 1:num_clust) {
    # To filter the mean values for specific cluster
    mean_val_for_cluster <- mean_matrix[i, ]

    # To filter the variance values for specific cluster
    variance_val_for_cluster <- var_vec[i]

    num_points_cluster <- clust_size_vec[i]

    # Initialize an empty list to store the vectors with column
    # values
    dim_val_list <- list()

    for (j in 1:num_dims) {
      dim_val_list[[j]] <- stats::rnorm(num_points_cluster,
        mean = mean_val_for_cluster[j],
        sd = variance_val_for_cluster
      )
    }
    # To generate a tibble for a cluster
    df_cluster <- matrix(unlist(dim_val_list), ncol = length(dim_val_list))

    df <- rbind(df, df_cluster)
  }

  if (num_noise != 0) {
    if (missing(min_n)) {
      stop("Missing min_n.")
    }

    if (missing(max_n)) {
      stop("Missing max_n.")
    }

    noise_mat <- gen_noise_dims(
      n = dim(df)[1], num_noise = num_noise,
      min_n = min_n, max_n = max_n
    )
    df <- cbind(df, noise_mat)

    df
  } else {
    df
  }
}


#' Generate Clusters with Different Shapes
#'
#' This function generates clusters with different shapes, including both Gaussian
#' and non-Gaussian clusters.
#'
#' @param n The total number of data points to be generated.
#' @param num_gau_clust The number of Gaussian clusters to generate.
#' @param num_non_gau_clust The number of non-Gaussian clusters to generate.
#' @param clust_sd_gau The standard deviation for the Gaussian clusters.
#' @param clust_sd_non_gau The standard deviation for the non-Gaussian clusters.
#' @param num_dims The number of dimensions for the data points.
#' @param a The scaling factor for the non-Gaussian cluster shape.
#' @param b The translation factor for the non-Gaussian cluster shape.
#'
#' @return A matrix containing the generated clusters with different shapes.
#' @export
#'
#' @examples
#' # Generate clusters with default parameters
#' set.seed(20240412)
#' data <- clust_diff_shapes(
#'   n = 300, num_gau_clust = 4,
#'   num_non_gau_clust = 2, clust_sd_gau = 0.05, clust_sd_non_gau = 0.1,
#'   num_dims = 7, a = 2, b = 4
#' )
clust_diff_shapes <- function(n, num_gau_clust, num_non_gau_clust, clust_sd_gau,
                              clust_sd_non_gau, num_dims, a, b) {
  if (n <= 0) {
    stop("Number of observations should be a positive number.")
  }

  if (num_gau_clust <= 0) {
    stop("Number of Gaussian clusters should be a positive number.")
  }

  if (num_non_gau_clust <= 0) {
    stop("Number of Non-Gaussian clusters should be a positive number.")
  }

  if (num_dims < 0) {
    stop("Number of effective dimensions should be a positive number.")
  }

  if (missing(n)) {
    stop("Missing n.")
  }

  if (missing(num_dims)) {
    stop("Missing num_dims.")
  }

  if (missing(num_gau_clust)) {
    stop("Missing num_gau_clust.")
  }

  if (missing(num_non_gau_clust)) {
    stop("Missing num_non_gau_clust.")
  }

  if (missing(clust_sd_gau)) {
    stop("Missing clust_sd_gau.")
  }

  if (missing(clust_sd_non_gau)) {
    stop("Missing clust_sd_non_gau.")
  }

  if (missing(a)) {
    stop("Missing a.")
  }

  if (missing(b)) {
    stop("Missing b.")
  }

  num_clust <- num_gau_clust + num_non_gau_clust

  # To check that the assigned n is divided by the number of clusters.
  if ((n %% num_clust) != 0) {
    warning("The sample size should be a product of number of clusters.")
    cluster_size <- floor(n / num_clust)
  } else {
    cluster_size <- n / num_clust
  }

  ## Generate Gaussian clusters

  # Create a vector of possible values (0 and 1)
  values <- c(0, 1)

  # Create an expanded grid with 0's and 1's
  mean_val_grid <- as.matrix(expand.grid(rep(list(values), num_dims)))

  # To select combinations for assigned number of clusters

  mean_val_grid_gau <- mean_val_grid[sample(NROW(mean_val_grid),
    size = num_gau_clust, replace = FALSE
  ), ]

  mean_val_grid_non_gau <- mean_val_grid[sample(NROW(mean_val_grid),
    size = num_non_gau_clust, replace = FALSE
  ), ]

  df <- matrix(nrow = 0, ncol = num_dims)

  for (i in 1:num_gau_clust) {
    # To filter the mean values for specific cluster
    mean_val_for_cluster <- mean_val_grid_gau[i, ]

    # Initialize an empty list to store the vectors with column
    # values
    dim_val_list <- list()

    for (j in 1:num_dims) {
      dim_val_list[[j]] <- stats::rnorm(cluster_size,
        mean = mean_val_for_cluster[j],
        sd = clust_sd_gau
      )
    }
    # To generate a tibble for a cluster
    df_gau_cluster <- matrix(unlist(dim_val_list), ncol = length(dim_val_list))

    df <- rbind(df, df_gau_cluster)
  }

  phi <- stats::runif(cluster_size, max = 2 * pi)
  rho <- sqrt(stats::runif(cluster_size))

  for (i in 1:num_non_gau_clust) {
    # To filter the mean values for specific cluster
    presence_of_elipse_cluster <- mean_val_grid_non_gau[i, ]

    # Initialize an empty list to store the vectors with column
    # values
    dim_val_list_n <- list()

    for (j in 1:num_dims) {
      if (presence_of_elipse_cluster[j] == 1) {
        dim_val_list_n[[j]] <- sqrt(a) * rho * cos(phi) + b
        ## Surface of poolar coordinate
      } else {
        dim_val_list_n[[j]] <- stats::rnorm(cluster_size,
          mean = 0,
          sd = clust_sd_non_gau
        )
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
#' This function generates clusters with different shapes, including both Gaussian
#' and non-Gaussian clusters,
#' with different numbers of points in each cluster.
#'
#' @param clust_size_vec A vector specifying the number of points for each cluster.
#' @param num_gau_clust The number of Gaussian clusters to generate.
#' @param num_non_gau_clust The number of non-Gaussian clusters to generate.
#' @param clust_sd_gau The standard deviation for the Gaussian clusters.
#' @param clust_sd_non_gau The standard deviation for the non-Gaussian clusters.
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
#' set.seed(20240412)
#' data <- clust_diff_shapes_pts(
#'   clust_size_vec = c(50, 50, 50, 50, 100, 100),
#'   num_gau_clust = 4,
#'   num_non_gau_clust = 2, clust_sd_gau = 0.05, clust_sd_non_gau = 0.1,
#'   num_dims = 7, a = 2, b = 4
#' )
clust_diff_shapes_pts <- function(clust_size_vec, num_gau_clust,
                                  num_non_gau_clust, clust_sd_gau,
                                  clust_sd_non_gau, num_dims, a, b) {
  if (length(clust_size_vec) != (num_gau_clust + num_non_gau_clust)) {
    stop("Vector of number of observations for each cluster should be equal to
         the number of clusters.")
  }

  if (num_gau_clust <= 0) {
    stop("Number of Gaussian clusters should be a positive number.")
  }

  if (num_non_gau_clust <= 0) {
    stop("Number of Non-Gaussian clusters should be a positive number.")
  }

  if (num_dims < 0) {
    stop("Number of effective dimensions should be a positive number.")
  }

  if (missing(clust_size_vec)) {
    stop("Missing clust_size_vec.")
  }

  if (missing(num_dims)) {
    stop("Missing num_dims.")
  }

  if (missing(num_gau_clust)) {
    stop("Missing num_gau_clust.")
  }

  if (missing(num_non_gau_clust)) {
    stop("Missing num_non_gau_clust.")
  }

  if (missing(clust_sd_gau)) {
    stop("Missing clust_sd_gau.")
  }

  if (missing(clust_sd_non_gau)) {
    stop("Missing clust_sd_non_gau.")
  }

  if (missing(a)) {
    stop("Missing a.")
  }

  if (missing(b)) {
    stop("Missing b.")
  }


  num_clust <- num_gau_clust + num_non_gau_clust

  ## Generate Gaussian clusters

  # Create a vector of possible values (0 and 1)
  values <- c(0, 1)

  # Create an expanded grid with 0's and 1's
  mean_val_grid <- as.matrix(expand.grid(rep(list(values), num_dims)))

  # To select combinations for assigned number of clusters
  mean_val_grid_gau <- mean_val_grid[sample(NROW(mean_val_grid),
    size = num_gau_clust, replace = FALSE
  ), ]

  mean_val_grid_non_gau <- mean_val_grid[sample(NROW(mean_val_grid),
    size = num_non_gau_clust, replace = FALSE
  ), ]

  df <- matrix(nrow = 0, ncol = num_dims)

  for (i in 1:num_gau_clust) {
    # To filter the mean values for specific cluster
    mean_val_for_cluster <- mean_val_grid_gau[i, ]

    # Initialize an empty list to store the vectors with column
    # values
    dim_val_list <- list()

    for (j in 1:num_dims) {
      dim_val_list[[j]] <- stats::rnorm(clust_size_vec[i],
        mean = mean_val_for_cluster[j],
        sd = clust_sd_gau
      )
    }
    # To generate a tibble for a cluster
    df_gau_cluster <- matrix(unlist(dim_val_list), ncol = length(dim_val_list))

    df <- rbind(df, df_gau_cluster)
  }

  for (i in 1:num_non_gau_clust) {
    phi <- stats::runif(clust_size_vec[(num_clust - i)], max = 2 * pi)
    rho <- sqrt(stats::runif(clust_size_vec[(num_clust - i)]))

    # To filter the mean values for specific cluster
    presence_of_elipse_cluster <- mean_val_grid_non_gau[i, ]

    # Initialize an empty list to store the vectors with column
    # values
    dim_val_list_n <- list()

    for (j in 1:num_dims) {
      if (presence_of_elipse_cluster[j] == 1) {
        dim_val_list_n[[j]] <- sqrt(a) * rho * cos(phi) + b
        ## Surface of poolar coordinate
      } else {
        dim_val_list_n[[j]] <- stats::rnorm(clust_size_vec[(num_clust - i)],
          mean = 0,
          sd = clust_sd_non_gau
        )
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
#' @param n The total number of data points to be generated.
#' @param num_noise The number of additional noise dimensions to be generated.
#' @param min_n The minimum value for the noise added to the data points.
#' @param max_n The maximum value for the noise added to the data points.
#'
#' @return A matrix containing the generated data, with each row representing a data point.
#' @export
#'
#' @examples
#'
#' # Generate clusters and curvilinear data with noise with custom parameters
#' set.seed(20240412)
#' data <- gau_curvy_clust_bkg(
#'   n = 260, num_noise = 2, min_n = -0.05,
#'   max_n = 0.05
#' )
gau_curvy_clust_bkg <- function(n, num_noise, min_n, max_n) {
  if (n <= 0) {
    stop("Number of points should be a positive number.")
  }

  if (num_noise < 0) {
    stop("Number of noise dimensions should be a positive number.")
  }

  if (missing(n)) {
    stop("Missing n.")
  }

  if (missing(num_noise)) {
    stop("Missing num_noise.")
  }

  # To check that the assigned n is divided by two
  if ((n %% 2) != 0) {
    stop("The sample size should be a product of two.")
  } else {
    cluster_size <- (n - n * 0.3) / 2
  }

  theta <- stats::runif(cluster_size, 0.20, 0.60 * pi)
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

  x <- stats::rnorm(n * 0.3, 11, 0.5)
  y <- stats::rnorm(n * 0.3, 11, 0.5)
  z <- rep(0, n * 0.3) + stats::rnorm(n * 0.3, 10, 0.05)
  w <- rep(0, n * 0.3) - stats::rnorm(n * 0.3, 10, 0.05)
  df3 <- matrix(c(x, y, z, w), ncol = 4)

  df <- rbind(df1, df2, df3)

  if (num_noise != 0) {
    if (missing(min_n)) {
      stop("Missing min_n.")
    }

    if (missing(max_n)) {
      stop("Missing max_n.")
    }

    noise_mat <- gen_noise_dims(
      n = dim(df)[1], num_noise = num_noise,
      min_n = min_n, max_n = max_n
    )
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
#' @param n The total number of data points to be generated.
#' @param num_noise The number of additional noise dimensions to be generated.
#' @param min_n The minimum value for the noise added to the data points.
#' @param max_n The maximum value for the noise added to the data points.
#'
#' @return A matrix containing the generated data, with each row representing a data point.
#' @export
#'
#' @examples
#'
#' # Generate doublets with noise with custom parameters
#' set.seed(20240412)
#' data <- one_doublet(n = 220, num_noise = 2, min_n = -0.05, max_n = 0.05)
one_doublet <- function(n, num_noise, min_n, max_n) {
  if (n <= 0) {
    stop("Number of points should be a positive number.")
  }

  if (num_noise < 0) {
    stop("Number of noise dimensions should be a positive number.")
  }

  if (missing(n)) {
    stop("Missing n.")
  }

  if (missing(num_noise)) {
    stop("Missing num_noise.")
  }

  # To check that the assigned n is divided by 2.2
  if (((n * 10) %% 22) != 0) { # n%%2.2
    stop("The sample size should be a product of 2.2.")
  } else {
    cluster_size <- (n * 10) / 22
  }


  df1 <- matrix(c(
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 1, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05)
  ), ncol = 4)

  df2 <- matrix(c(
    stats::rnorm(cluster_size, mean = 1, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05)
  ), ncol = 4)

  df3_new <- (df1 + df2) / 2
  # get a sample of 10
  samp <- sample(NROW(df3_new), cluster_size * 0.20) ## 20% from the original dataset

  # data in the sample
  df3 <- df3_new[samp, ]

  df <- rbind(df1, df2, df3)

  if (num_noise != 0) {
    if (missing(min_n)) {
      stop("Missing min_n.")
    }

    if (missing(max_n)) {
      stop("Missing max_n.")
    }

    noise_mat <- gen_noise_dims(
      n = dim(df)[1], num_noise = num_noise,
      min_n = min_n, max_n = max_n
    )
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
#' @param n The total number of data points to be generated.
#' @param num_noise The number of additional noise dimensions to be generated.
#' @param min_n The minimum value for the noise added to the data points.
#' @param max_n The maximum value for the noise added to the data points.
#'
#' @return A matrix containing the generated data, with each row representing a data point.
#' @export
#'
#' @examples
#'
#' # Generate doublets with three clusters and noise with custom parameters
#' set.seed(20240412)
#' data <- three_doublets(
#'   n = 420, num_noise = 2,
#'   min_n = -0.05, max_n = 0.05
#' )
three_doublets <- function(n, num_noise, min_n, max_n) {
  if (n <= 0) {
    stop("Number of points should be a positive number.")
  }

  if (num_noise < 0) {
    stop("Number of noise dimensions should be a positive number.")
  }

  if (missing(n)) {
    stop("Missing n.")
  }

  if (missing(num_noise)) {
    stop("Missing num_noise.")
  }

  # To check that the assigned n is divided by 4.2
  if ((n %% 4.2) != 0) {
    warning("The sample size should be a product of 4.2.")
    cluster_size <- floor(n / 4.2)
  } else {
    cluster_size <- n / 4.2
  }


  df1 <- matrix(c(
    stats::rnorm(cluster_size, mean = 3, sd = 0.05),
    stats::rnorm(cluster_size, mean = 1, sd = 0.05),
    stats::rnorm(cluster_size, mean = 1, sd = 0.05),
    stats::rnorm(cluster_size, mean = 1, sd = 0.05),
    stats::rnorm(cluster_size, mean = 1, sd = 0.05),
    stats::rnorm(cluster_size, mean = 1, sd = 0.05),
    stats::rnorm(cluster_size, mean = 1, sd = 0.05),
    stats::rnorm(cluster_size, mean = 1, sd = 0.05),
    stats::rnorm(cluster_size, mean = 1, sd = 0.05),
    stats::rnorm(cluster_size, mean = 1, sd = 0.05)
  ), ncol = 10)

  df2 <- matrix(c(
    stats::rnorm(cluster_size, mean = 1, sd = 0.05),
    stats::rnorm(cluster_size, mean = 1, sd = 0.05),
    stats::rnorm(cluster_size, mean = 1, sd = 0.05),
    stats::rnorm(cluster_size, mean = 1, sd = 0.05),
    stats::rnorm(cluster_size, mean = 1, sd = 0.05),
    stats::rnorm(cluster_size, mean = 1, sd = 0.05),
    stats::rnorm(cluster_size, mean = 1, sd = 0.05),
    stats::rnorm(cluster_size, mean = 1, sd = 0.05),
    stats::rnorm(cluster_size, mean = 1, sd = 0.05),
    stats::rnorm(cluster_size, mean = 1, sd = 0.05)
  ), ncol = 10)

  df3_new <- (df1 + df2) / 2
  # get a sample of 10
  samp <- sample(NROW(df3_new), cluster_size * 0.40) ## 20% from the original dataset

  # data in the sample
  df3 <- df3_new[samp, ]

  df4 <- matrix(c(
    stats::rnorm(cluster_size, mean = 1, sd = 0.05),
    stats::rnorm(cluster_size, mean = 1, sd = 0.05),
    stats::rnorm(cluster_size, mean = 1, sd = 0.05),
    stats::rnorm(cluster_size, mean = 3, sd = 0.05),
    stats::rnorm(cluster_size, mean = 1, sd = 0.05),
    stats::rnorm(cluster_size, mean = 1, sd = 0.05),
    stats::rnorm(cluster_size, mean = 1, sd = 0.05),
    stats::rnorm(cluster_size, mean = 1, sd = 0.05),
    stats::rnorm(cluster_size, mean = 1, sd = 0.05),
    stats::rnorm(cluster_size, mean = 1, sd = 0.05)
  ), ncol = 10)

  df5_new <- (df2 + df4) / 2

  # get a sample of 10
  samp1 <- sample(NROW(df5_new), cluster_size * 0.30) ## 20% from the original dataset

  # data in the sample
  df5 <- df5_new[samp1, ]

  df6_new <- (df1 + df4) / 2

  # get a sample of 10
  samp2 <- sample(NROW(df6_new), cluster_size * 0.50) ## 20% from the original dataset

  # data in the sample
  df6 <- df6_new[samp2, ]

  df <- rbind(df1, df2, df3, df4, df5, df6)

  if (num_noise != 0) {
    if (missing(min_n)) {
      stop("Missing min_n.")
    }

    if (missing(max_n)) {
      stop("Missing max_n.")
    }

    noise_mat <- gen_noise_dims(
      n = dim(df)[1], num_noise = num_noise,
      min_n = min_n, max_n = max_n
    )
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
#' @param n The total number of data points to be generated.
#' @param num_noise The number of additional noise dimensions to be generated.
#' @param min_n The minimum value for the noise added to the data points.
#' @param max_n The maximum value for the noise added to the data points.
#'
#' @return A matrix containing the generated data, with each row representing a data point.
#' @export
#'
#' @examples
#'
#' # Generate doublets with four clusters and noise with custom parameters
#' set.seed(20240412)
#' data <- one_doublet_four_clusts(
#'   n = 440, num_noise = 2,
#'   min_n = -0.05, max_n = 0.05
#' )
one_doublet_four_clusts <- function(n, num_noise, min_n, max_n) {
  if (n <= 0) {
    stop("Number of points should be a positive number.")
  }

  if (num_noise < 0) {
    stop("Number of noise dimensions should be a positive number.")
  }

  if (missing(n)) {
    stop("Missing n.")
  }

  if (missing(num_noise)) {
    stop("Missing num_noise.")
  }

  # To check that the assigned n is divided by 4.4
  if (((n * 10) %% 44) != 0) { # n%%4.4
    stop("The sample size should be a product of 4.4.")
  } else {
    cluster_size <- (n * 10) / 44
  }


  df1 <- matrix(c(
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 1, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 1, sd = 0.05)
  ), ncol = 7)

  df2 <- matrix(c(
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 1, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05)
  ), ncol = 7)

  df3_new <- (df1 + df2) / 2
  # get a sample of 10
  samp <- sample(NROW(df3_new), cluster_size * 0.40) ## 20% from the original dataset

  # data in the sample
  df3 <- df3_new[samp, ]

  df4 <- matrix(c(
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 1, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05)
  ), ncol = 7)


  df5 <- matrix(c(
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05)
  ), ncol = 7)

  df <- rbind(df1, df2, df3, df4, df5)

  if (num_noise != 0) {
    if (missing(min_n)) {
      stop("Missing min_n.")
    }

    if (missing(max_n)) {
      stop("Missing max_n.")
    }

    noise_mat <- gen_noise_dims(
      n = dim(df)[1], num_noise = num_noise,
      min_n = min_n, max_n = max_n
    )
    df <- cbind(df, noise_mat)

    df
  } else {
    df
  }
}

#' Generate Doublets with Different Variance Clusters and Noise
#'
#' This function generates data with one set of doublets (pairs of clusters) having
#' clusters with different variance, along with added background noise.
#'
#' @param n The total number of data points to be generated.
#' @param num_noise The number of additional noise dimensions to be generated.
#' @param min_n The minimum value for the noise added to the data points.
#' @param max_n The maximum value for the noise added to the data points.
#'
#' @return A matrix containing the generated data, with each row representing a data point.
#' @export
#'
#' @examples
#'
#' # Generate doublets with different variance clusters and noise with custom parameters
#' set.seed(20240412)
#' data <- one_doublet_diff_var_clust(
#'   n = 260, num_noise = 2,
#'   min_n = -0.05, max_n = 0.05
#' )
one_doublet_diff_var_clust <- function(n, num_noise, min_n, max_n) {
  if (n <= 0) {
    stop("Number of points should be a positive number.")
  }

  if (num_noise < 0) {
    stop("Number of noise dimensions should be a positive number.")
  }

  if (missing(n)) {
    stop("Missing n.")
  }

  if (missing(num_noise)) {
    stop("Missing num_noise.")
  }

  # To check that the assigned n is divided by 2.6
  if (((n * 10) %% 26) != 0) {
    stop("The sample size should be a product of 2.6.")
  } else {
    cluster_size <- n / 2.6
  }


  df1 <- matrix(c(
    stats::rnorm(cluster_size, mean = 1, sd = 0.1),
    stats::rnorm(cluster_size, mean = 0, sd = 0.08),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 1, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.08),
    stats::rnorm(cluster_size, mean = 0, sd = 0.08),
    stats::rnorm(cluster_size, mean = 1, sd = 0.08),
    stats::rnorm(cluster_size, mean = 1, sd = 0.02),
    stats::rnorm(cluster_size, mean = 0, sd = 0.02),
    stats::rnorm(cluster_size, mean = 0, sd = 0.02)
  ), ncol = 10)

  df2 <- matrix(c(
    stats::rnorm(cluster_size, mean = 0, sd = 0.02),
    stats::rnorm(cluster_size, mean = 0, sd = 0.02),
    stats::rnorm(cluster_size, mean = 0, sd = 0.02),
    stats::rnorm(cluster_size, mean = 1, sd = 0.05),
    stats::rnorm(cluster_size, mean = 1, sd = 0.02),
    stats::rnorm(cluster_size, mean = 0, sd = 0.02),
    stats::rnorm(cluster_size, mean = 0, sd = 0.02),
    stats::rnorm(cluster_size, mean = 1, sd = 0.02),
    stats::rnorm(cluster_size, mean = 0, sd = 0.02),
    stats::rnorm(cluster_size, mean = 0, sd = 0.02)
  ), ncol = 10)

  df3_new <- (df1 + df2) / 2
  # get a sample of 10
  samp <- sample(NROW(df3_new), cluster_size * 0.60) ## 20% from the original dataset

  # data in the sample
  df3 <- df3_new[samp, ]

  df <- rbind(df1, df2, df3)

  if (num_noise != 0) {
    if (missing(min_n)) {
      stop("Missing min_n.")
    }

    if (missing(max_n)) {
      stop("Missing max_n.")
    }

    noise_mat <- gen_noise_dims(
      n = dim(df)[1], num_noise = num_noise,
      min_n = min_n, max_n = max_n
    )
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
#' @param n The total number of data points to be generated.
#' @param num_noise The number of additional noise dimensions to be generated.
#' @param min_n The minimum value for the noise added to the data points.
#' @param max_n The maximum value for the noise added to the data points.
#'
#' @return A matrix containing the generated data, with each row representing a data point.
#' @export
#'
#' @examples
#'
#' # Generate doublets with different pattern clusters and noise with custom parameters
#' set.seed(20240412)
#' data <- one_doublet_diff_patterns(
#'   n = 280, num_noise = 2, min_n = -0.05,
#'   max_n = 0.05
#' )
one_doublet_diff_patterns <- function(n, num_noise, min_n, max_n) {
  if (n <= 0) {
    stop("Number of points should be a positive number.")
  }

  if (num_noise < 0) {
    stop("Number of noise dimensions should be a positive number.")
  }

  if (missing(n)) {
    stop("Missing n.")
  }

  if (missing(num_noise)) {
    stop("Missing num_noise.")
  }

  # To check that the assigned n is divided by 2.8
  if ((n %% 2.8) != 0) {
    warning("The sample size should be a product of 2.8.")
    cluster_size <- floor(n / 2.8)
  } else {
    cluster_size <- n / 2.8
  }


  theta <- stats::runif(cluster_size, 0.20, 0.60 * pi)

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
    sin(theta) + stats::rnorm(cluster_size, 1, 0.03)
  ), ncol = 10)

  df2 <- matrix(c(
    stats::rnorm(cluster_size, mean = 1, sd = 0.1),
    stats::rnorm(cluster_size, mean = 0, sd = 0.08),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 1, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.08),
    stats::rnorm(cluster_size, mean = 0, sd = 0.08),
    stats::rnorm(cluster_size, mean = 1, sd = 0.08),
    stats::rnorm(cluster_size, mean = 1, sd = 0.02),
    stats::rnorm(cluster_size, mean = 0, sd = 0.02),
    stats::rnorm(cluster_size, mean = 0, sd = 0.02)
  ), ncol = 10)


  df3_new <- (df1 + df2) / 2
  # get a sample of 10
  samp <- sample(NROW(df3_new), cluster_size * 0.80) ## 20% from the original dataset

  # data in the sample
  df3 <- df3_new[samp, ]

  df <- rbind(df1, df2, df3)

  if (num_noise != 0) {
    if (missing(min_n)) {
      stop("Missing min_n.")
    }

    if (missing(max_n)) {
      stop("Missing max_n.")
    }

    noise_mat <- gen_noise_dims(
      n = dim(df)[1], num_noise = num_noise,
      min_n = min_n, max_n = max_n
    )
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
#' @param n The total number of data points to be generated.
#' @param num_noise The number of additional noise dimensions to be generated.
#' @param min_n The minimum value for the noise added to the data points.
#' @param max_n The maximum value for the noise added to the data points.
#'
#' @return A matrix containing the generated data, with each row representing a data point.
#' @export
#'
#' @examples
#'
#' # Generate doublets in parallel with noise with custom parameters
#' set.seed(20240412)
#' data <- two_doublets_parallel(n = 440, num_noise = 2, min_n = -0.05, max_n = 0.05)
two_doublets_parallel <- function(n, num_noise, min_n, max_n) {
  if (n <= 0) {
    stop("Number of points should be a positive number.")
  }

  if (num_noise < 0) {
    stop("Number of noise dimensions should be a positive number.")
  }

  if (missing(n)) {
    stop("Missing n.")
  }

  if (missing(num_noise)) {
    stop("Missing num_noise.")
  }


  # To check that the assigned n is divided by 4.4
  if (((n * 10) %% 44) != 0) { # n%%4.4
    stop("The sample size should be a product of 4.4.")
  } else {
    cluster_size <- (n * 10) / 44
  }


  df1 <- matrix(c(
    stats::rnorm(cluster_size, mean = 1, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 1, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 1, sd = 0.05),
    stats::rnorm(cluster_size, mean = 1, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05)
  ), ncol = 10)

  df2 <- matrix(c(
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 1, sd = 0.05),
    stats::rnorm(cluster_size, mean = 1, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 1, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05)
  ), ncol = 10)

  df3_new <- (df1 + df2) / 2
  # get a sample of 10
  samp <- sample(NROW(df3_new), cluster_size * 0.20) ## 20% from the original dataset

  # data in the sample
  df3 <- df3_new[samp, ]

  df4 <- matrix(c(
    stats::rnorm(cluster_size, mean = -1, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 1, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = -1, sd = 0.05),
    stats::rnorm(cluster_size, mean = -1, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05)
  ), ncol = 10)

  df5 <- matrix(c(
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = -1, sd = 0.05),
    stats::rnorm(cluster_size, mean = -1, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = -1, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05)
  ), ncol = 10)

  df6_new <- (df4 + df5) / 2
  # get a sample of 10
  samp1 <- sample(NROW(df6_new), cluster_size * 0.20) ## 20% from the original dataset

  # data in the sample
  df6 <- df6_new[samp1, ]

  df <- rbind(df1, df2, df3, df4, df5, df6)

  if (num_noise != 0) {
    if (missing(min_n)) {
      stop("Missing min_n.")
    }

    if (missing(max_n)) {
      stop("Missing max_n.")
    }

    noise_mat <- gen_noise_dims(
      n = dim(df)[1], num_noise = num_noise,
      min_n = min_n, max_n = max_n
    )
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
#' @param n The total number of data points to be generated.
#' @param num_noise The number of additional noise dimensions to be generated.
#' @param min_n The minimum value for the noise added to the data points.
#' @param max_n The maximum value for the noise added to the data points.
#'
#' @return A matrix containing the generated data, with each row representing a data point.
#' @export
#'
#' @examples
#'
#' # Generate doublets with background noise with custom parameters
#' set.seed(20240412)
#' data <- one_doublet_bkg(n = 250, num_noise = 2, min_n = -0.05, max_n = 0.05)
one_doublet_bkg <- function(n, num_noise, min_n, max_n) {
  if (n <= 0) {
    stop("Number of points should be a positive number.")
  }

  if (num_noise < 0) {
    stop("Number of noise dimensions should be a positive number.")
  }

  if (missing(n)) {
    stop("Missing n.")
  }

  if (missing(num_noise)) {
    stop("Missing num_noise.")
  }

  # To check that the assigned n is divided by 2.5
  if ((n %% 2.5) != 0) {
    warning("The sample size should be a product of 2.5.")
    cluster_size <- floor(n / 2.5)
  } else {
    cluster_size <- n / 2.5
  }


  df1 <- matrix(c(
    stats::rnorm(cluster_size, mean = 1, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 1, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 1, sd = 0.05)
  ), ncol = 7)

  df2 <- matrix(c(
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 1, sd = 0.05),
    stats::rnorm(cluster_size, mean = 1, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05)
  ), ncol = 7)

  df3_new <- (df1 + df2) / 2
  # get a sample of 10
  samp <- sample(NROW(df3_new), cluster_size * 0.20) ## 20% from the original dataset

  # data in the sample
  df3 <- df3_new[samp, ]

  df4_new <- matrix(c(
    stats::rnorm(cluster_size, mean = 0, sd = 0.2),
    stats::rnorm(cluster_size, mean = 0, sd = 0.5),
    stats::rnorm(cluster_size, mean = 0.5, sd = 0.5),
    stats::rnorm(cluster_size, mean = 0.2, sd = 0.5),
    stats::rnorm(cluster_size, mean = 0.2, sd = 0.3),
    stats::rnorm(cluster_size, mean = 0, sd = 0.5),
    stats::rnorm(cluster_size, mean = 0, sd = 0.3)
  ), ncol = 7)

  # get a sample of 10
  samp1 <- sample(NROW(df4_new), cluster_size * 0.30) ## 20% from the original dataset

  # data in the sample
  df4 <- df4_new[samp1, ]

  df <- rbind(df1, df2, df3, df4)

  if (num_noise != 0) {
    if (missing(min_n)) {
      stop("Missing min_n.")
    }

    if (missing(max_n)) {
      stop("Missing max_n.")
    }

    noise_mat <- gen_noise_dims(
      n = dim(df)[1], num_noise = num_noise,
      min_n = min_n, max_n = max_n
    )
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
#' @param n The total number of data points to be generated.
#' @param num_noise The number of additional noise dimensions to be generated.
#' @param min_n The minimum value for the noise added to the data points.
#' @param max_n The maximum value for the noise added to the data points.
#'
#' @return A matrix containing the generated data, with each row representing a data point.
#' @export
#'
#' @examples
#'
#' # Generate two doublets with background noise with custom parameters
#' set.seed(20240412)
#' data <- two_doublets_bkg(n = 200, num_noise = 2, min_n = -0.05, max_n = 0.05)
two_doublets_bkg <- function(n, num_noise, min_n, max_n) {
  if (n <= 0) {
    stop("Number of points should be a positive number.")
  }

  if (num_noise < 0) {
    stop("Number of noise dimensions should be a positive number.")
  }

  if (missing(n)) {
    stop("Missing n.")
  }

  if (missing(num_noise)) {
    stop("Missing num_noise.")
  }

  # To check that the assigned n is divided by four
  if ((n %% 4) != 0) {
    warning("The sample size should be a product of four.")
    cluster_size <- floor(n / 4)
  } else {
    cluster_size <- n / 4
  }

  df1 <- matrix(c(
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05)
  ), ncol = 4)

  df2 <- matrix(c(
    stats::rnorm(cluster_size, mean = 1, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05)
  ), ncol = 4)

  df6_new <- (df1 + df2) / 2
  # get a sample of 10
  samp <- sample(NROW(df6_new), cluster_size * 0.20) ## 20% from the original dataset

  # data in the sample
  df6 <- df6_new[samp, ]

  df3 <- matrix(c(
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 1, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05)
  ), ncol = 4)

  df7_new <- (df1 + df3) / 2
  # get a sample of 10
  samp <- sample(NROW(df7_new), cluster_size * 0.20) ## 20% from the original dataset

  # data in the sample
  df7 <- df7_new[samp, ]

  df4 <- matrix(c(
    stats::rnorm(cluster_size * 0.6, mean = 0, sd = 0.5),
    stats::rnorm(cluster_size * 0.6, mean = 0, sd = 0.5),
    stats::rnorm(cluster_size * 0.6, mean = 0, sd = 0.5),
    stats::rnorm(cluster_size * 0.6, mean = 0, sd = 0.5)
  ), ncol = 4)

  df <- rbind(df1, df2, df3, df6, df7, df4)

  if (num_noise != 0) {
    if (missing(min_n)) {
      stop("Missing min_n.")
    }

    if (missing(max_n)) {
      stop("Missing max_n.")
    }

    noise_mat <- gen_noise_dims(
      n = dim(df)[1], num_noise = num_noise,
      min_n = min_n, max_n = max_n
    )
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
#' @param n The total number of data points to be generated.
#' @param num_noise The number of additional noise dimensions to be generated.
#' @param min_n The minimum value for the noise added to the data points.
#' @param max_n The maximum value for the noise added to the data points.
#'
#' @return A matrix containing the generated data, with each row representing a data point.
#' @export
#'
#' @examples
#'
#' # Generate two nonlinear clusters with noise with custom parameters
#' set.seed(20240412)
#' data <- two_nonlinear(n = 200, num_noise = 2, min_n = -0.05, max_n = 0.50)
two_nonlinear <- function(n, num_noise, min_n, max_n) {
  if (n <= 0) {
    stop("Number of points should be a positive number.")
  }

  if (num_noise < 0) {
    stop("Number of noise dimensions should be a positive number.")
  }

  if (missing(n)) {
    stop("Missing n.")
  }

  if (missing(num_noise)) {
    stop("Missing num_noise.")
  }

  # To check that the assigned n is divided by two
  if ((n %% 2) != 0) {
    warning("The sample size should be a product of two.")
    cluster_size <- floor(n / 2)
  } else {
    cluster_size <- n / 2
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

  if (num_noise != 0) {
    if (missing(min_n)) {
      stop("Missing min_n.")
    }

    if (missing(max_n)) {
      stop("Missing max_n.")
    }

    noise_mat <- gen_noise_dims(
      n = dim(df)[1], num_noise = num_noise,
      min_n = min_n, max_n = max_n
    )
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
#' @param n The total number of data points to be generated.
#' @param num_noise The number of additional noise dimensions to be generated.
#' @param min_n The minimum value for the noise added to the data points.
#' @param max_n The maximum value for the noise added to the data points.
#'
#' @return A matrix containing the generated data, with each row representing a data point.
#' @export
#'
#' @examples
#'
#' # Generate two curvilinear clusters with noise with custom parameters
#' set.seed(20240412)
#' data <- two_curvy(n = 200, num_noise = 2, min_n = -0.05, max_n = 0.05)
two_curvy <- function(n, num_noise, min_n, max_n) {
  if (n <= 0) {
    stop("Number of points should be a positive number.")
  }

  if (num_noise < 0) {
    stop("Number of noise dimensions should be a positive number.")
  }

  if (missing(n)) {
    stop("Missing n.")
  }

  if (missing(num_noise)) {
    stop("Missing num_noise.")
  }

  # To check that the assigned n is divided by two
  if ((n %% 2) != 0) {
    warning("The sample size should be a product of two.")
    cluster_size <- floor(n / 2)
  } else {
    cluster_size <- n / 2
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

  if (num_noise != 0) {
    if (missing(min_n)) {
      stop("Missing min_n.")
    }

    if (missing(max_n)) {
      stop("Missing max_n.")
    }

    noise_mat <- gen_noise_dims(
      n = dim(df)[1], num_noise = num_noise,
      min_n = min_n, max_n = max_n
    )
    df <- cbind(df, noise_mat)

    df
  } else {
    df
  }
}

#' Generate Two Curvilinear Differentiated Clusters with Noise
#'
#' This function generates data with two curvilinear clusters that are differentiated
#' from each other, along with added noise.
#'
#' @param cluster_size_vec A vector specifying the number of points in each cluster.
#' @param num_noise The number of additional noise dimensions to be generated.
#' @param min_n The minimum value for the noise added to the data points.
#' @param max_n The maximum value for the noise added to the data points.
#'
#' @return A matrix containing the generated data, with each row representing a data point.
#' @export
#'
#' @examples
#'
#' # Generate two curvilinear differentiated clusters with noise with custom parameters
#' set.seed(20240412)
#' data <- two_curvy_diff_pts(
#'   cluster_size_vec = c(50, 100), num_noise = 2,
#'   min_n = -0.05, max_n = 0.05
#' )
two_curvy_diff_pts <- function(cluster_size_vec, num_noise, min_n, max_n) {
  if (length(cluster_size_vec) != 2) {
    stop("There should be two elements.")
  }

  if (num_noise < 0) {
    stop("Number of noise dimensions should be a positive number.")
  }

  if (missing(cluster_size_vec)) {
    stop("Missing cluster_size_vec.")
  }

  if (missing(num_noise)) {
    stop("Missing num_noise.")
  }

  theta <- stats::runif(cluster_size_vec[1], 0.40, 0.70 * pi)
  df1 <- matrix(c(
    cos(theta) + stats::rnorm(cluster_size_vec[1], 1, 0.06),
    sin(theta) + stats::rnorm(cluster_size_vec[1], 1, 0.06),
    cos(theta) + stats::rnorm(cluster_size_vec[1], 1, 0.06),
    sin(theta) + stats::rnorm(cluster_size_vec[1], 1, 0.06)
  ), ncol = 4)

  theta1 <- stats::runif(cluster_size_vec[2], 0.20, 0.90 * pi)
  df2 <- matrix(c(
    1 + cos(theta1) + stats::rnorm(cluster_size_vec[2], 1, 0.06),
    1 + sin(theta1) + stats::rnorm(cluster_size_vec[2], 1, 0.06),
    cos(theta1) + stats::rnorm(cluster_size_vec[2], 1, 0.06),
    sin(theta1) + stats::rnorm(cluster_size_vec[2], 1, 0.06)
  ), ncol = 4)

  df <- rbind(df1, df2)

  if (num_noise != 0) {
    if (missing(min_n)) {
      stop("Missing min_n.")
    }

    if (missing(max_n)) {
      stop("Missing max_n.")
    }

    noise_mat <- gen_noise_dims(
      n = dim(df)[1], num_noise = num_noise,
      min_n = min_n, max_n = max_n
    )
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
#' @param n The total number of data points to be generated.
#' @param num_noise The number of additional noise dimensions to be generated.
#' @param min_n The minimum value for the noise added to the data points.
#' @param max_n The maximum value for the noise added to the data points.
#'
#' @return A matrix containing the generated data, with each row representing a data point.
#' @export
#'
#' @examples
#'
#' # Generate three nonlinear clusters with noise with custom parameters
#' set.seed(20240412)
#' data <- three_nonlinear(n = 300, num_noise = 2, min_n = -0.05, max_n = 0.05)
three_nonlinear <- function(n, num_noise, min_n, max_n) {
  if (n <= 0) {
    stop("Number of points should be a positive number.")
  }

  if (num_noise < 0) {
    stop("Number of noise dimensions should be a positive number.")
  }

  if (missing(n)) {
    stop("Missing n.")
  }

  if (missing(num_noise)) {
    stop("Missing num_noise.")
  }

  # To check that the assigned n is divided by three
  if ((n %% 3) != 0) {
    warning("The sample size should be a product of three.")
    cluster_size <- floor(n / 3)
  } else {
    cluster_size <- n / 3
  }

  phi <- stats::runif(cluster_size, max = 2 * pi)
  rho <- sqrt(stats::runif(cluster_size))

  theta <- stats::runif(cluster_size, 0, 1.80 * pi)
  x <- theta
  y <- sin(theta)

  df1 <- matrix(c(x, y, sqrt(1) * rho * cos(phi) + 4, sqrt(1) * rho * sin(phi) + 4), ncol = 4)
  df2 <- matrix(c(x + 1, y + 1, sqrt(1) * rho * cos(phi) + 6, sqrt(1) * rho * sin(phi) + 6), ncol = 4)
  df3 <- matrix(c(x - 1, y - 1, sqrt(1) * rho * cos(phi) + 8, sqrt(1) * rho * sin(phi) + 8), ncol = 4)

  df <- rbind(df1, df2, df3)

  if (num_noise != 0) {
    if (missing(min_n)) {
      stop("Missing min_n.")
    }

    if (missing(max_n)) {
      stop("Missing max_n.")
    }

    noise_mat <- gen_noise_dims(
      n = dim(df)[1], num_noise = num_noise,
      min_n = min_n, max_n = max_n
    )
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
#' @param n The total number of data points to be generated.
#' @param num_noise The number of additional noise dimensions to be generated.
#' @param min_n The minimum value for the noise added to the data points.
#' @param max_n The maximum value for the noise added to the data points.
#'
#' @return A matrix containing the generated data, with each row representing a data point.
#' @export
#'
#' @examples
#'
#' # Generate three cluster mirror with noise with custom parameters
#' set.seed(20240412)
#' data <- three_clust_mirror(n = 300, num_noise = 2, min_n = -0.05, max_n = 0.05)
three_clust_mirror <- function(n, num_noise, min_n, max_n) {
  if (n <= 0) {
    stop("Number of points should be a positive number.")
  }

  if (num_noise < 0) {
    stop("Number of noise dimensions should be a positive number.")
  }

  if (missing(n)) {
    stop("Missing n.")
  }

  if (missing(num_noise)) {
    stop("Missing num_noise.")
  }

  # To check that the assigned n is divided by six
  if ((n %% 6) != 0) {
    warning("The sample size should be a product of six.")
    cluster_size <- floor(n / 6)
  } else {
    cluster_size <- n / 6
  }


  df1 <- matrix(c(
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05)
  ), ncol = 4)

  df2 <- matrix(c(
    stats::rnorm(cluster_size, mean = 1, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05)
  ), ncol = 4)

  df3 <- matrix(c(
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 1, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05),
    stats::rnorm(cluster_size, mean = 0, sd = 0.05)
  ), ncol = 4)

  df_1 <- rbind(df1, df2, df3)

  df_2 <- df_1 + 2
  df <- rbind(df_1, df_2)

  if (num_noise != 0) {
    if (missing(min_n)) {
      stop("Missing min_n.")
    }

    if (missing(max_n)) {
      stop("Missing max_n.")
    }

    noise_mat <- gen_noise_dims(
      n = dim(df)[1], num_noise = num_noise,
      min_n = min_n, max_n = max_n
    )
    df <- cbind(df, noise_mat)

    df
  } else {
    df
  }
}


#' Generate Cluster and Curvilinear Data with Noise
#'
#' This function generates data with two clusters, one following a curvilinear
#' pattern and the other distributed randomly.
#'
#' @param n The total number of data points to be generated.
#' @param clust_size_vec A vector specifying the number of points for each cluster.
#'                         If not provided, the n is divided equally
#'                         between the two clusters.
#' @param num_noise The number of additional noise dimensions to be generated.
#' @param min_n The minimum value for the noise added to the data points.
#' @param max_n The maximum value for the noise added to the data points.
#'
#' @return A matrix containing the generated data, with each row representing a data point.
#' @export
#'
#' @examples
#'
#' # Generate cluster and curvilinear data with custom parameters
#' set.seed(20240412)
#' data <- gau_curvy_clust(
#'   n = 300, clust_size_vec = c(100, 200), num_noise = 3,
#'   min_n = -0.05, max_n = 0.05
#' )
gau_curvy_clust <- function(n, clust_size_vec, num_noise, min_n, max_n) {
  if (n <= 0) {
    stop("Number of points should be a positive number.")
  }

  if (num_noise < 0) {
    stop("Number of noise dimensions should be a positive number.")
  }

  if (missing(n)) {
    stop("Missing n.")
  }

  if (missing(num_noise)) {
    stop("Missing num_noise.")
  }

  ## If the number of points for each cluster is not defined
  if (missing(clust_size_vec)) {
    # To check that the assigned n is divided by two
    if ((n %% 2) != 0) {
      warning("The sample size should be a product of two.")
      cluster_size <- floor(n / 2)
      clust_size_vec <- append(cluster_size, (n - cluster_size))
    } else {
      cluster_size <- n / 2
      clust_size_vec <- rep(cluster_size, 2)
    }
  }

  theta <- stats::runif(clust_size_vec[1], 0.20, 0.60 * pi)
  x <- cos(theta) + stats::rnorm(clust_size_vec[1], 10, 0.03)
  y <- sin(theta) + stats::rnorm(clust_size_vec[1], 10, 0.03)
  z <- rep(0, clust_size_vec[1]) + stats::rnorm(clust_size_vec[1], 10, 0.03)
  w <- rep(0, clust_size_vec[1]) - stats::rnorm(clust_size_vec[1], 10, 0.03)

  df1 <- matrix(c(x, y, z, w), ncol = 4)

  x <- stats::rnorm(clust_size_vec[2], 10, 0.05)
  y <- stats::rnorm(clust_size_vec[2], 10, 0.05)
  z <- rep(0, clust_size_vec[2]) + stats::rnorm(clust_size_vec[2], 10, 0.05)
  w <- rep(0, clust_size_vec[2]) - stats::rnorm(clust_size_vec[2], 10, 0.05)

  df2 <- matrix(c(x, y, z, w), ncol = 4)

  df <- rbind(df1, df2)

  if (num_noise != 0) {
    if (missing(min_n)) {
      stop("Missing min_n.")
    }

    if (missing(max_n)) {
      stop("Missing max_n.")
    }

    noise_mat <- gen_noise_dims(
      n = dim(df)[1], num_noise = num_noise,
      min_n = min_n, max_n = max_n
    )
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
#' @param n Total number of data points to generate, should be a multiple of three.
#' @param num_dims Number of dimensions for each data point.
#' @param num_noise Number of additional noise dimensions to add to the data.
#' @param min_n Minimum value for the noise added to the data.
#' @param max_n Maximum value for the noise added to the data.
#'
#' @return A matrix containing the generated data points with or without added noise.
#'
#' @examples
#' set.seed(20240412)
#' three_clust_diff_dist(
#'   n = 150, num_dims = 7, num_noise = 4, min_n = -0.05,
#'   max_n = 0.05
#' )
#'
#' @export
three_clust_diff_dist <- function(n, num_dims, num_noise, min_n, max_n) {
  if (n <= 0) {
    stop("Number of points should be a positive number.")
  }

  if (num_noise < 0) {
    stop("Number of noise dimensions should be a positive number.")
  }

  if (missing(n)) {
    stop("Missing n.")
  }

  if (missing(num_noise)) {
    stop("Missing num_noise.")
  }

  # To check that the assigned n is divided by three
  if ((n %% 3) != 0) {
    warning("The sample size should be a product of three.")
    cluster_size <- floor(n / 3)
  } else {
    cluster_size <- n / 3
  }

  df1 <- matrix(stats::rnorm(cluster_size * num_dims, sd = 1), ncol = num_dims)

  df2 <- matrix(stats::rnorm(cluster_size * num_dims, sd = 1), ncol = num_dims)
  df2[, 1] <- df2[, 1] + 10

  df3 <- matrix(stats::rnorm(cluster_size * num_dims, sd = 1), ncol = num_dims)
  df3[, 1] <- df3[, 1] + 50

  df <- rbind(df1, df2, df3)

  if (num_noise != 0) {
    if (missing(min_n)) {
      stop("Missing min_n.")
    }

    if (missing(max_n)) {
      stop("Missing max_n.")
    }

    noise_mat <- gen_noise_dims(
      n = dim(df)[1], num_noise = num_noise,
      min_n = min_n, max_n = max_n
    )
    df <- cbind(df, noise_mat)

    df
  } else {
    df
  }
}
