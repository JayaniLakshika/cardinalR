#' Generate Curvy Tree Data with Noise
#'
#' This function generates a dataset representing a curvy tree structure, with added noise.
#'
#' @param sample_size The total number of samples to generate.
#' @param num_noise_dims The number of additional noise dimensions to add to the data.
#' @param min_noise The minimum value for the noise dimensions.
#' @param max_noise The maximum value for the noise dimensions.
#' @return A matrix containing the curvy tree data with added noise.
#' @export
#'
#' @examples
#' tree_data <- curvy_tree_with_noise(sample_size = 300, num_noise_dims = 8,
#'                                    min_noise = -0.05, max_noise = 0.05)
curvy_tree_with_noise <- function(sample_size, num_noise_dims, min_noise,
                                  max_noise) {

  # To check that the assigned sample_size is divided by three
  if ((sample_size%%3) != 0) {
    warning("The sample size should be a product of three.")
    cluster_size <- floor(sample_size/3)

  } else {
    cluster_size <- sample_size/3
  }


  x <- stats::runif(cluster_size, -2, 2)
  y <- -(x^3 + stats::runif(cluster_size, 0, 6)) + stats::runif(cluster_size, 0, 0.2)
  z <- stats::rnorm(cluster_size, 10, 0.1)
  w <- stats::rnorm(cluster_size, 10, 0.1)

  df1 <- matrix(c(x, y, z, w), ncol = 4)

  x <- stats::runif(cluster_size, 0, 2)
  y <- (x^3 + stats::runif(cluster_size, 0, 6)) + stats::runif(cluster_size, 0, 0.2)
  z <- stats::rnorm(cluster_size, 10, 0.1)
  w <- stats::rnorm(cluster_size, 10, 0.1)

  df2 <- matrix(c(x, y, z, w), ncol = 4)

  x <- stats::runif(cluster_size, -2, 0)
  y <- -(x^3 + stats::runif(cluster_size, 0, 6)) + stats::runif(cluster_size, 0, 0.2) + 10
  z <- stats::rnorm(cluster_size, 10, 0.1)
  w <- stats::rnorm(cluster_size, 10, 0.1)

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

#' Generate Tree-like Data with Noise
#'
#' This function generates a dataset representing a tree-like structure, with added noise.
#'
#' @param sample_size The total number of samples to generate.
#' @param num_noise_dims The number of additional noise dimensions to add to the data.
#' @param min_noise The minimum value for the noise dimensions.
#' @param max_noise The maximum value for the noise dimensions.
#' @return A matrix containing the tree-like data with added noise.
#' @export
#'
#' @examples
#' tree_data <- tree_with_noise(sample_size = 300, num_noise_dims = 8,
#'                              min_noise = -0.05, max_noise = 0.05)
tree_with_noise <- function(sample_size, num_noise_dims, min_noise, max_noise) {

  # To check that the assigned sample_size is divided by five
  if ((sample_size%%5) != 0) {
    warning("The sample size should be a product of five.")
    cluster_size <- floor(sample_size/5)

  } else {
    cluster_size <- sample_size/5
  }


  x <- stats::runif(cluster_size, -3, 3)
  y <- abs(0.5 * x)
  z <- stats::rnorm(cluster_size, 10, 0.03)
  w <- stats::rnorm(cluster_size, 10, 0.03)

  df1 <- matrix(c(x, y, z, w), ncol = 4)

  x <- stats::runif(cluster_size, -0.5, 0.5)
  y <- abs(10*x)
  z <- stats::rnorm(cluster_size, 10, 0.03)
  w <- stats::rnorm(cluster_size, 10, 0.03)

  df2 <- matrix(c(x, y, z, w), ncol = 4)

  x <- stats::runif(cluster_size, -6, 3)
  y <- (-1) * abs(0.5 * x + 5)
  z <- stats::rnorm(cluster_size, 10, 0.03)
  w <- stats::rnorm(cluster_size, 10, 0.03)

  df3 <- matrix(c(x, y, z, w), ncol = 4)

  x <- stats::runif(cluster_size, -0.5, 0.5)
  y <- (-1) * abs(10 * x) - 5
  z <- stats::rnorm(cluster_size, 10, 0.03)
  w <- stats::rnorm(cluster_size, 10, 0.03)

  df4 <- matrix(c(x, y, z, w), ncol = 4)

  x <- stats::runif(cluster_size, -5, 5)
  y <- x
  z <- stats::rnorm(cluster_size, 10, 0.03)
  w <- stats::rnorm(cluster_size, 10, 0.03)

  df5 <- matrix(c(x, y, z, w), ncol = 4)

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

#' Generate Seven-Branching Data with Noise
#'
#' This function generates a dataset representing seven branches with added noise.
#'
#' @param sample_size The total number of samples to generate.
#' @param num_noise_dims The number of additional noise dimensions to add to the data.
#' @param min_noise The minimum value for the noise dimensions.
#' @param max_noise The maximum value for the noise dimensions.
#' @return A matrix containing the seven-branching data with added noise.
#' @export
#'
#' @examples
#' seven_branching_data <- seven_branching_data_with_noise(sample_size = 210,
#' num_noise_dims = 8, min_noise = -0.05, max_noise = 0.05)
seven_branching_data_with_noise <- function(sample_size, num_noise_dims, min_noise,
                                            max_noise) {

  # To check that the assigned sample_size is divided by seven
  if ((sample_size%%7) != 0) {
    warning("The sample size should be a product of seven.")
    cluster_size <- floor(sample_size/7)

  } else {
    cluster_size <- sample_size/7
  }

  x <- stats::runif(cluster_size, -2, 2)
  y <- -(x^3 + stats::runif(cluster_size, 0, 1)) + stats::runif(cluster_size, 0, 0.2)
  z <- rep(0, cluster_size) + stats::rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - stats::rnorm(cluster_size, 10, 0.03)

  df1 <- matrix(c(x, y, z, w), ncol = 4)

  x <- stats::runif(cluster_size, -2, 1.5)
  y <- (x^3 + stats::runif(cluster_size, 0, 1)) + stats::runif(cluster_size, 0, 0.2)
  z <- rep(0, cluster_size) + stats::rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - stats::rnorm(cluster_size, 10, 0.03)

  df2 <- matrix(c(x, y, z, w), ncol = 4)

  x <- stats::runif(cluster_size, -2, 1.5)
  y <- (1 + (x-3)^2 + stats::runif(cluster_size, 0, 1)) + stats::runif(cluster_size, 0, 0.1)
  z <- rep(0, cluster_size) + stats::rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - stats::rnorm(cluster_size, 10, 0.03)

  df3 <- matrix(c(x, y, z, w), ncol = 4)

  x <- stats::runif(cluster_size, -0.5, 3)
  y <- (1 + -(x-3)^2 + stats::runif(cluster_size, 0, 1)) + stats::runif(cluster_size, 0, 0.1)
  z <- rep(0, cluster_size) + stats::rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - stats::rnorm(cluster_size, 10, 0.03)

  df4 <- matrix(c(x, y, z, w), ncol = 4)

  x <- stats::runif(cluster_size, -1, 1)
  y <- (20 + x^3 + stats::runif(cluster_size, 0, 0.1)) + stats::runif(cluster_size, 0, 0.01)
  z <- rep(0, cluster_size) + stats::rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - stats::rnorm(cluster_size, 10, 0.03)

  df5 <- matrix(c(x, y, z, w), ncol = 4)

  x <- stats::runif(cluster_size, -2, 2)
  y <- (x^2 + stats::runif(cluster_size, 0, 0.1)) + stats::runif(cluster_size, 0, 0.01) + 10
  z <- rep(0, cluster_size) + stats::rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - stats::rnorm(cluster_size, 10, 0.03)

  df6 <- matrix(c(x, y, z, w), ncol = 4)

  x <- stats::runif(cluster_size, -2, 2)
  y <- (x^2 + stats::runif(cluster_size, 0, 0.2)) + stats::runif(cluster_size, 0, 0.01) + 15
  z <- rep(0, cluster_size) + stats::rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - stats::rnorm(cluster_size, 10, 0.03)

  df7 <- matrix(c(x, y, z, w), ncol = 4)

  df <- rbind(df1, df2, df3, df4, df5, df6, df7)

  if (num_noise_dims != 0) {

    noise_mat <- gen_noise_dims(n = dim(df)[1], num_noise_dims = num_noise_dims,
                                min_noise = min_noise, max_noise = max_noise)
    df <- cbind(df, noise_mat)

    df

  } else {

    df

  }

}

#' Generate Four-Branching Data with Noise
#'
#' This function generates a dataset representing four branches with added noise.
#'
#' @param sample_size The total number of samples to generate.
#' @param num_noise_dims The number of additional noise dimensions to add to the data.
#' @param min_noise The minimum value for the noise dimensions.
#' @param max_noise The maximum value for the noise dimensions.
#' @return A matrix containing the four-branching data with added noise.
#' @export
#'
#' @examples
#' four_branching_data <- four_branching_data_with_noise(sample_size = 400,
#' num_noise_dims = 8, min_noise = -0.05, max_noise = 0.05)
four_branching_data_with_noise <- function(sample_size, num_noise_dims, min_noise,
                                           max_noise) {

  # To check that the assigned sample_size is divided by four
  if (((sample_size - sample_size * 0.1)%%4) != 0) {
    warning("The sample size should be a product of four.")
    cluster_size <- floor((sample_size - sample_size * 0.1)/4)

  } else {
    cluster_size <- (sample_size - sample_size * 0.1)/4
  }

  x <- stats::runif(cluster_size, -5, 1)
  y <- (exp(x) + stats::runif(cluster_size, 0, 0.1)) + stats::runif(cluster_size, 0, 0.2)
  z <- rep(0, cluster_size) + stats::rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - stats::rnorm(cluster_size, 10, 0.03)

  df1 <- matrix(c(x, y, z, w), ncol = 4)

  x <- stats::runif(cluster_size, -1, 5)
  y <- (exp(-x) + stats::runif(cluster_size, 0, 0.1)) + stats::runif(cluster_size, 0, 0.2)
  z <- rep(0, cluster_size) + stats::rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - stats::rnorm(cluster_size, 10, 0.03)

  df2 <- matrix(c(x, y, z, w), ncol = 4)

  x <- stats::runif(cluster_size, 0, 5)
  y <- (log(x) + stats::runif(cluster_size, 0, 0.1)) + stats::runif(cluster_size, 0, 0.2)
  z <- rep(0, cluster_size) + stats::rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - stats::rnorm(cluster_size, 10, 0.03)

  df3 <- matrix(c(x, y, z, w), ncol = 4)

  x <- stats::runif(cluster_size, -5, 0)
  y <- (log(-x) + stats::runif(cluster_size, 0, 0.1)) + stats::runif(cluster_size, 0, 0.2)
  z <- rep(0, cluster_size) + stats::rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - stats::rnorm(cluster_size, 10, 0.03)

  df4 <- matrix(c(x, y, z, w), ncol = 4)

  x <- stats::runif(sample_size * 0.1, -5, 0)
  y <- stats::runif(sample_size * 0.1, 0, 0.8) + stats::runif(sample_size * 0.1, 0, 0.8)
  z <- rep(0, sample_size * 0.1) + stats::rnorm(sample_size * 0.1, 10, 0.03)
  w <- rep(0, sample_size * 0.1) - stats::rnorm(sample_size * 0.1, 10, 0.03)

  df5 <- matrix(c(x, y, z, w), ncol = 4)

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

#' Generate Eight Branching Data with Noise
#'
#' This function generates a dataset representing eight branching patterns, with added noise.
#'
#' @param sample_size The total number of samples to generate.
#' @param num_noise_dims The number of additional noise dimensions to add to the data.
#' @param min_noise The minimum value for the noise dimensions.
#' @param max_noise The maximum value for the noise dimensions.
#' @return A matrix containing the eight branching data with added noise.
#' @export
#'
#' @examples
#' branching_data <- eight_branching_data_with_noise(sample_size = 400,
#' num_noise_dims = 8, min_noise = -0.05, max_noise = 0.05)
eight_branching_data_with_noise <- function(sample_size, num_noise_dims, min_noise,
                                            max_noise) {

  # To check that the assigned sample_size is divided by eight
  if ((sample_size%%8) != 0) {
    warning("The sample size should be a product of eight.")
    cluster_size <- floor(sample_size/8)

  } else {
    cluster_size <- sample_size/8
  }

  x <- stats::runif(cluster_size, -1, 2)
  y <- (exp(x) + stats::runif(cluster_size, 0, 0.1)) + stats::runif(cluster_size)
  z <- rep(0, cluster_size) + stats::rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - stats::rnorm(cluster_size, 10, 0.03)

  df1 <- matrix(c(x, y, z, w), ncol = 4)

  x <- stats::runif(cluster_size, -1, 1)
  y <- (exp(2*x) + stats::runif(cluster_size, 0, 0.1)) + stats::runif(cluster_size, 0, 0.2)
  z <- rep(0, cluster_size) + stats::rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - stats::rnorm(cluster_size, 10, 0.03)

  df2 <- matrix(c(x, y, z, w), ncol = 4)

  x <- stats::runif(cluster_size, -1, 0.6)
  y <- (exp(3*x) + stats::runif(cluster_size, 0, 0.1)) + stats::runif(cluster_size, 0, 0.2)
  z <- rep(0, cluster_size) + stats::rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - stats::rnorm(cluster_size, 10, 0.03)

  df3 <- matrix(c(x, y, z, w), ncol = 4)

  x <- stats::runif(cluster_size, -1, 3)
  y <- (exp(0.5*x) + stats::runif(cluster_size, 0, 0.1)) + stats::runif(cluster_size, 0, 0.2)
  z <- rep(0, cluster_size) + stats::rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - stats::rnorm(cluster_size, 10, 0.03)

  df4 <- matrix(c(x, y, z, w), ncol = 4)

  x <- stats::runif(cluster_size, -2, 1)
  y <- (exp(-x) + stats::runif(cluster_size, 0, 0.1)) + stats::runif(cluster_size, 0, 0.2)
  z <- rep(0, cluster_size) + stats::rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - stats::rnorm(cluster_size, 10, 0.03)

  df5 <- matrix(c(x, y, z, w), ncol = 4)

  x <- stats::runif(cluster_size, -1, 1)
  y <- (exp(2*-x) + stats::runif(cluster_size, 0, 0.1)) + stats::runif(cluster_size, 0, 0.2)
  z <- rep(0, cluster_size) + stats::rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - stats::rnorm(cluster_size, 10, 0.03)

  df6 <- matrix(c(x, y, z, w), ncol = 4)

  x <- stats::runif(cluster_size, -0.6, 1)
  y <- (exp(3*-x) + stats::runif(cluster_size, 0, 0.1)) + stats::runif(cluster_size, 0, 0.2)
  z <- rep(0, cluster_size) + stats::rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - stats::rnorm(cluster_size, 10, 0.03)

  df7 <- matrix(c(x, y, z, w), ncol = 4)

  x <- stats::runif(cluster_size, -3, 1)
  y <- (exp(0.5*-x) + stats::runif(cluster_size, 0, 0.1)) + stats::runif(cluster_size, 0, 0.2)
  z <- rep(0, cluster_size) + stats::rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - stats::rnorm(cluster_size, 10, 0.03)

  df8 <- matrix(c(x, y, z, w), ncol = 4)

  df <- rbind(df1, df2, df3, df4, df5, df6, df7, df8)

  if (num_noise_dims != 0) {

    noise_mat <- gen_noise_dims(n = dim(df)[1], num_noise_dims = num_noise_dims,
                                min_noise = min_noise, max_noise = max_noise)
    df <- cbind(df, noise_mat)

    df

  } else {

    df

  }

}
