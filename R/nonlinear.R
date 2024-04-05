#' Generate points on a curvilinear 2D manifold
#'
#' This function generates points on a curvilinear 2D manifold based on a nonlinear equation.
#'
#' @param sample_size The number of points to generate.
#' @param num_noise_dims The number of noise dimensions to add to the generated points.
#' @param min_noise The minimum value for the noise dimensions.
#' @param max_noise The maximum value for the noise dimensions.
#'
#' @return A matrix containing the generated points on the curvilinear 2D manifold.
#'
#' @examples
#' curvilinear_points <- curvilinear_2d(sample_size = 100, num_noise_dims = 2,
#'                                      min_noise = -1, max_noise = 1)
#'
#' @export
curvilinear_2d <- function(sample_size, num_noise_dims, min_noise, max_noise){

  x <- stats::runif(sample_size, 0, 2)
  y <- -(x^3 + stats::runif(sample_size, 0, 3)) + stats::runif(sample_size, 0, 0.5)

  curvilinear_mat <- matrix(c(x, y), ncol = 2)

  if (num_noise_dims != 0) {

    noise_mat <- gen_noise_dims(n = dim(curvilinear_mat)[1], num_noise_dims = num_noise_dims,
                                min_noise = min_noise, max_noise = max_noise)
    curvilinear_mat <- cbind(curvilinear_mat, noise_mat)

    curvilinear_mat

  } else {

    curvilinear_mat

  }

}

#' Generate points on a nonlinear 2D manifold
#'
#' This function generates points on a nonlinear 2D manifold based on a given equation.
#'
#' @param sample_size The number of points to generate.
#' @param num_noise_dims The number of noise dimensions to add to the generated points.
#' @param min_noise The minimum value for the noise dimensions.
#' @param max_noise The maximum value for the noise dimensions.
#'
#' @return A matrix containing the generated points on the nonlinear 2D manifold.
#'
#' @examples
#' nonlinear_points <- nonlinear_2d(sample_size = 100, num_noise_dims = 2,
#'                                  min_noise = -1, max_noise = 1)
#'
#' @export
nonlinear_2d <- function(sample_size, num_noise_dims, min_noise, max_noise) {

  theta = stats::runif(sample_size, 0.2, 0.6 * pi)
  x = cos(theta) + stats::rnorm(sample_size, 10, 0.03)
  y = sin(theta) + stats::rnorm(sample_size, 10, 0.03)

  nonlinear_mat <- matrix(c(x, y), ncol = 2)

  if (num_noise_dims != 0) {

    noise_mat <- gen_noise_dims(n = dim(nonlinear_mat)[1], num_noise_dims = num_noise_dims,
                                min_noise = min_noise, max_noise = max_noise)
    nonlinear_mat <- cbind(nonlinear_mat, noise_mat)

    nonlinear_mat

  } else {

    nonlinear_mat

  }

}

sine_curve_with_noise <- function(sample_size = 100, with_seed = NULL, num_of_noise_dim = 8,
                                  min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  theta = runif(sample_size, 0,1.80 * pi)
  x = theta
  y = sin(theta)

  df <- tibble::tibble(x1 = x, x2 = y)

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), 3:(3 + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)
  df

}

nonlinear_connect_with_noise <- function(sample_size = 400, with_seed = NULL, num_of_noise_dim = 8,
                                         min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by three
  if ((sample_size%%4) != 0) {
    warning("The sample size should be a product of four.")
    cluster_size <- floor(sample_size/4)

  } else {
    cluster_size <- sample_size/4
  }

  theta = runif(cluster_size, 0,0.80 * pi)
  x = cos(theta) + rnorm(cluster_size, 10, 0.03)
  y = sin(theta) + rnorm(cluster_size, 10, 0.03)
  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)


  df1 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x = cos(-theta) + rnorm(cluster_size, 10, 0.03) + rnorm(cluster_size, 0.1, 0)
  y = sin(-theta) + rnorm(cluster_size, 10, 0.03) + rnorm(cluster_size, 0.1, 0)
  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df2 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x = cos(-theta) + rnorm(cluster_size, 10, 0.03) + rnorm(cluster_size, 0.1, 0)
  z = sin(-theta) + rnorm(cluster_size, 10, 0.03) + rnorm(cluster_size, 0.1, 0)
  y <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df3 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x = cos(theta) + rnorm(cluster_size, 10, 0.03) + rnorm(cluster_size, 0.1, 0)
  z = sin(theta) + rnorm(cluster_size, 10, 0.03) + rnorm(cluster_size, 0.1, 0)
  y <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df4 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  df <- bind_rows(df1, df2, df3, df4)
  names(df) <- paste0(rep("x", NCOL(df)), 1:NCOL(df))

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)
  df

}

nonlinear_mirror_with_noise <- function(sample_size = 400, with_seed = NULL, num_of_noise_dim = 8,
                                        min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by three
  if ((sample_size%%2) != 0) {
    warning("The sample size should be a product of four.")
    cluster_size <- floor(sample_size/2)

  } else {
    cluster_size <- sample_size/2
  }

  x <- runif(cluster_size, -8, 1.5)
  y <- -(exp(x) + runif(cluster_size, 0, 1)) + runif(cluster_size, 0, 0.7)

  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df1 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x <- runif(cluster_size, -8, 1.5)
  y <- (exp(x) + runif(cluster_size, 0, 1)) + runif(cluster_size, 0, 0.7)

  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df2 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  df <- bind_rows(df1, df2)
  names(df) <- paste0(rep("x", NCOL(df)), 1:NCOL(df))

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)
  df

}

two_curvy_panckakes_with_noise <- function(sample_size = 300, with_seed = NULL, num_of_noise_dim = 8,
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


  phi <- runif(cluster_size, max = 2*pi)
  rho <- sqrt(runif(cluster_size))

  theta = runif(cluster_size, 0,1.80 * pi)
  x = theta
  y = sin(theta)

  df1 <- tibble::tibble(x1=x, x2=y, x3=sqrt(1)*rho*cos(phi) + 4, x4=sqrt(1)*rho*sin(phi) + 4)
  df2 <- tibble::tibble(x1=x+1, x2=y+1, x3=sqrt(1)*rho*cos(phi) + 6, x4=sqrt(1)*rho*sin(phi) + 6)


  df <- bind_rows(df1, df2)

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)

  df

}

two_curvilinear_data_with_noise <- function(sample_size = 250, with_seed = NULL, num_of_noise_dim = 8,
                                            min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by three
  if (((sample_size - sample_size * 0.2)%%2) != 0) {
    warning("The sample size should be a product of number of clusters.")
    cluster_size <- floor((sample_size - sample_size * 0.2)/2)

  } else {
    cluster_size <- (sample_size - sample_size * 0.2)/2
  }




  x <- runif(cluster_size, -2, -0.5)
  y <- (x^2 + runif(cluster_size, 0, 0.1)) + runif(cluster_size, 0, 0.2)

  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df1 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x <- runif(cluster_size, 0.5, 2)
  y <- (x^2 + runif(cluster_size, 0, 0.1)) + runif(cluster_size, 0, 0.2)

  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df2 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x <- rnorm(sample_size * 0.2, mean = 0, sd = 0.4)
  y <- rnorm(sample_size * 0.2, mean = 1.5, sd = 0.5)

  z <- rep(0, sample_size * 0.2) + rnorm(sample_size * 0.2, 10, 0.03)
  w <- rep(0, sample_size * 0.2) - rnorm(sample_size * 0.2, 10, 0.03)

  df3 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  df <- bind_rows(df1, df2, df3)

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)

  df

}
