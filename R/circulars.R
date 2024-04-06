#' Generate Three Circular Clusters with Noise
#'
#' This function generates three circular clusters in 4D space with added noise dimensions.
#'
#' @param sample_size The total number of samples to generate.
#' @param num_noise_dims The number of additional noise dimensions to add to the data.
#' @param min_noise The minimum value for the noise dimensions.
#' @param max_noise The maximum value for the noise dimensions.
#' @return A matrix containing the three circular clusters with added noise.
#' @export
#'
#' @examples
#' circular_clusters_data <- three_circulars_with_noise(sample_size = 300,
#' num_noise_dims = 4, min_noise = -0.05, max_noise = 0.05)
three_circulars_with_noise <- function(sample_size, num_noise_dims, min_noise,
                                       max_noise) {

  # To check that the assigned sample_size is divided by three
  if ((sample_size%%3) != 0) {
    warning("The sample size should be a product of three.")
    cluster_size <- floor(sample_size/3)

  } else {
    cluster_size <- sample_size/3
  }

  theta <- stats::runif(cluster_size, 0.0,2 * pi)
  x <- cos(theta) + stats::rnorm(cluster_size, 10, 0.03)
  y <- sin(theta) + stats::rnorm(cluster_size, 10, 0.03)

  z <- rep(0, cluster_size) + stats::rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - stats::rnorm(cluster_size, 10, 0.03)

  df1 <- matrix(c(x, y, z, w), ncol = 4)

  x <- 0.5 * cos(theta) + stats::rnorm(cluster_size, 10, 0.03)
  y <- 0.5 * sin(theta) + stats::rnorm(cluster_size, 10, 0.03)

  z <- rep(0, cluster_size) + stats::rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - stats::rnorm(cluster_size, 10, 0.03)

  df2 <- matrix(c(x, y, z, w), ncol = 4)

  x <- stats::rnorm(cluster_size, 10, 0.03)
  y <- stats::rnorm(cluster_size, 10, 0.03)

  z <- rep(0, cluster_size) + stats::rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - stats::rnorm(cluster_size, 10, 0.03)

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

#' Generate Cell Cycle Data with Noise
#'
#' This function generates a cell cycle dataset with added noise dimensions.
#'
#' @param sample_size The total number of samples to generate.
#' @param num_noise_dims The number of additional noise dimensions to add to the data.
#' @param min_noise The minimum value for the noise dimensions.
#' @param max_noise The maximum value for the noise dimensions.
#' @return A matrix containing the cell cycle data with added noise.
#' @export
#'
#' @examples
#' cell_cycle_data <- cell_cycle_with_noise(sample_size = 300,
#' num_noise_dims = 4, min_noise = -0.05, max_noise = 0.05)
cell_cycle_with_noise <- function(sample_size, num_noise_dims, min_noise,
                                  max_noise) {

  # To check that the assigned sample_size is divided by three
  if ((sample_size%%3) != 0) {
    warning("The sample size should be a product of three.")
    cluster_size <- floor(sample_size/3)

  } else {
    cluster_size <- sample_size/3
  }


  r1 <- 2
  r2 <- 1

  theta <- stats::runif(cluster_size, 0, 2 * pi)
  x <- rep(0, cluster_size)
  y <- r1 * cos(theta)
  z <- r2 * sin(theta)

  df1 <- matrix(c(x, y, z), ncol = 3)

  x <- r2 * cos(theta)
  y <- rep(0, cluster_size)
  z <- r1 * sin(theta)

  df2 <- matrix(c(x, y, z), ncol = 3)

  x <- r1 * cos(theta)
  y <- r2 * sin(theta)
  z <- rep(0, cluster_size)

  df3 <- matrix(c(x, y, z), ncol = 3)

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

#' Generate Curvy Cell Cycle Data with Noise
#'
#' This function generates a curvy cell cycle dataset with added noise dimensions.
#'
#' @param sample_size The total number of samples to generate.
#' @param num_noise_dims The number of additional noise dimensions to add to the data.
#' @param min_noise The minimum value for the noise dimensions.
#' @param max_noise The maximum value for the noise dimensions.
#' @return A matrix containing the curvy cell cycle data with added noise.
#' @export
#'
#' @examples
#' curvy_cell_cycle_data <- curvy_cell_cycle_with_noise(sample_size = 300,
#' num_noise_dims = 4, min_noise = -0.05, max_noise = 0.05)
curvy_cell_cycle_with_noise <- function(sample_size, num_noise_dims, min_noise,
                                        max_noise) {

  # To check that the assigned sample_size is divided by three
  if ((sample_size%%3) != 0) {
    warning("The sample size should be a product of three.")
    cluster_size <- floor(sample_size/3)

  } else {
    cluster_size <- sample_size/3
  }


  r = sqrt(3)/3

  theta <- stats::runif(cluster_size, 0, 2 * pi)
  x <- cos(theta)
  y <- r + sin(theta)
  z <- cos(3 * theta)/3

  df1 <- matrix(c(x, y, z), ncol = 3)

  x <- cos(theta) + 0.5
  y <- sin(theta) - r/2
  z <- cos(3 * theta)/3

  df2 <- matrix(c(x, y, z), ncol = 3)

  x <- cos(theta) - 0.5
  y <- sin(theta) - r/2
  z <- cos(3 * theta)/3

  df3 <- matrix(c(x, y, z), ncol = 3)

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
