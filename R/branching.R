#' Generate Curvy Tree Data with Noise
#'
#' This function generates a dataset representing a curvy tree structure, with added noise.
#'
#' @param n The total number of samples to generate.
#' @param num_noise The number of additional noise dimensions to add to the data.
#' @param min_n The minimum value for the noise dimensions.
#' @param max_n The maximum value for the noise dimensions.
#' @return A matrix containing the curvy tree data with added noise.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' tree_data <- curvy_tree(n = 300, num_noise = 2, min_n = -0.05, max_n = 0.05)
curvy_tree <- function(n, num_noise, min_n, max_n) {
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

#' Generate Tree-like Data with Noise
#'
#' This function generates a dataset representing a tree-like structure, with added noise.
#'
#' @param n The total number of samples to generate.
#' @param num_noise The number of additional noise dimensions to add to the data.
#' @param min_n The minimum value for the noise dimensions.
#' @param max_n The maximum value for the noise dimensions.
#' @return A matrix containing the tree-like data with added noise.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' tree_data <- tree(n = 300, num_noise = 2, min_n = -0.05, max_n = 0.05)
tree <- function(n, num_noise, min_n, max_n) {
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

  # To check that the assigned n is divided by five
  if ((n %% 5) != 0) {
    warning("The sample size should be a product of five.")
    cluster_size <- floor(n / 5)
  } else {
    cluster_size <- n / 5
  }


  x <- stats::runif(cluster_size, -3, 3)
  y <- abs(0.5 * x)
  z <- stats::rnorm(cluster_size, 10, 0.03)
  w <- stats::rnorm(cluster_size, 10, 0.03)

  df1 <- matrix(c(x, y, z, w), ncol = 4)

  x <- stats::runif(cluster_size, -0.5, 0.5)
  y <- abs(10 * x)
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

#' Generate Seven-Branching Data with Noise
#'
#' This function generates a dataset representing seven branches with added noise.
#'
#' @param n The total number of samples to generate.
#' @param num_noise The number of additional noise dimensions to add to the data.
#' @param min_n The minimum value for the noise dimensions.
#' @param max_n The maximum value for the noise dimensions.
#' @return A matrix containing the seven-branching data with added noise.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' seven_branching_data <- seven_branch(
#'   n = 210, num_noise = 2, min_n = -0.05,
#'   max_n = 0.05
#' )
seven_branch <- function(n, num_noise, min_n, max_n) {
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

  # To check that the assigned n is divided by seven
  if ((n %% 7) != 0) {
    warning("The sample size should be a product of seven.")
    cluster_size <- floor(n / 7)
  } else {
    cluster_size <- n / 7
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
  y <- (1 + (x - 3)^2 + stats::runif(cluster_size, 0, 1)) + stats::runif(cluster_size, 0, 0.1)
  z <- rep(0, cluster_size) + stats::rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - stats::rnorm(cluster_size, 10, 0.03)

  df3 <- matrix(c(x, y, z, w), ncol = 4)

  x <- stats::runif(cluster_size, -0.5, 3)
  y <- (1 + -(x - 3)^2 + stats::runif(cluster_size, 0, 1)) + stats::runif(cluster_size, 0, 0.1)
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

#' Generate Four-Branching Data with Noise
#'
#' This function generates a dataset representing four branches with added noise.
#'
#' @param n The total number of samples to generate.
#' @param num_noise The number of additional noise dimensions to add to the data.
#' @param min_n The minimum value for the noise dimensions.
#' @param max_n The maximum value for the noise dimensions.
#' @return A matrix containing the four-branching data with added noise.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' four_branching_data <- four_branch(
#'   n = 400, num_noise = 2, min_n = -0.05,
#'   max_n = 0.05
#' )
four_branch <- function(n, num_noise, min_n, max_n) {
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
  if (((n - n * 0.1) %% 4) != 0) {
    warning("The sample size should be a product of four.")
    cluster_size <- floor((n - n * 0.1) / 4)
  } else {
    cluster_size <- (n - n * 0.1) / 4
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

  x <- stats::runif(n * 0.1, -5, 0)
  y <- stats::runif(n * 0.1, 0, 0.8) + stats::runif(n * 0.1, 0, 0.8)
  z <- rep(0, n * 0.1) + stats::rnorm(n * 0.1, 10, 0.03)
  w <- rep(0, n * 0.1) - stats::rnorm(n * 0.1, 10, 0.03)

  df5 <- matrix(c(x, y, z, w), ncol = 4)

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

#' Generate Eight Branching Data with Noise
#'
#' This function generates a dataset representing eight branching patterns, with added noise.
#'
#' @param n The total number of samples to generate.
#' @param num_noise The number of additional noise dimensions to add to the data.
#' @param min_n The minimum value for the noise dimensions.
#' @param max_n The maximum value for the noise dimensions.
#' @return A matrix containing the eight branching data with added noise.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' branching_data <- eight_branch(
#'   n = 400, num_noise = 2, min_n = -0.05,
#'   max_n = 0.05
#' )
eight_branch <- function(n, num_noise, min_n, max_n) {
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

  # To check that the assigned n is divided by eight
  if ((n %% 8) != 0) {
    warning("The sample size should be a product of eight.")
    cluster_size <- floor(n / 8)
  } else {
    cluster_size <- n / 8
  }

  x <- stats::runif(cluster_size, -1, 2)
  y <- (exp(x) + stats::runif(cluster_size, 0, 0.1)) + stats::runif(cluster_size)
  z <- rep(0, cluster_size) + stats::rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - stats::rnorm(cluster_size, 10, 0.03)

  df1 <- matrix(c(x, y, z, w), ncol = 4)

  x <- stats::runif(cluster_size, -1, 1)
  y <- (exp(2 * x) + stats::runif(cluster_size, 0, 0.1)) + stats::runif(cluster_size, 0, 0.2)
  z <- rep(0, cluster_size) + stats::rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - stats::rnorm(cluster_size, 10, 0.03)

  df2 <- matrix(c(x, y, z, w), ncol = 4)

  x <- stats::runif(cluster_size, -1, 0.6)
  y <- (exp(3 * x) + stats::runif(cluster_size, 0, 0.1)) + stats::runif(cluster_size, 0, 0.2)
  z <- rep(0, cluster_size) + stats::rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - stats::rnorm(cluster_size, 10, 0.03)

  df3 <- matrix(c(x, y, z, w), ncol = 4)

  x <- stats::runif(cluster_size, -1, 3)
  y <- (exp(0.5 * x) + stats::runif(cluster_size, 0, 0.1)) + stats::runif(cluster_size, 0, 0.2)
  z <- rep(0, cluster_size) + stats::rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - stats::rnorm(cluster_size, 10, 0.03)

  df4 <- matrix(c(x, y, z, w), ncol = 4)

  x <- stats::runif(cluster_size, -2, 1)
  y <- (exp(-x) + stats::runif(cluster_size, 0, 0.1)) + stats::runif(cluster_size, 0, 0.2)
  z <- rep(0, cluster_size) + stats::rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - stats::rnorm(cluster_size, 10, 0.03)

  df5 <- matrix(c(x, y, z, w), ncol = 4)

  x <- stats::runif(cluster_size, -1, 1)
  y <- (exp(2 * -x) + stats::runif(cluster_size, 0, 0.1)) + stats::runif(cluster_size, 0, 0.2)
  z <- rep(0, cluster_size) + stats::rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - stats::rnorm(cluster_size, 10, 0.03)

  df6 <- matrix(c(x, y, z, w), ncol = 4)

  x <- stats::runif(cluster_size, -0.6, 1)
  y <- (exp(3 * -x) + stats::runif(cluster_size, 0, 0.1)) + stats::runif(cluster_size, 0, 0.2)
  z <- rep(0, cluster_size) + stats::rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - stats::rnorm(cluster_size, 10, 0.03)

  df7 <- matrix(c(x, y, z, w), ncol = 4)

  x <- stats::runif(cluster_size, -3, 1)
  y <- (exp(0.5 * -x) + stats::runif(cluster_size, 0, 0.1)) + stats::runif(cluster_size, 0, 0.2)
  z <- rep(0, cluster_size) + stats::rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - stats::rnorm(cluster_size, 10, 0.03)

  df8 <- matrix(c(x, y, z, w), ncol = 4)

  df <- rbind(df1, df2, df3, df4, df5, df6, df7, df8)

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

#' Generate Curvy Branching Cluster Data
#'
#' This function generates curvy branching cluster data with three clusters of different shapes.
#'
#' @param n The total number of data points to be generated.
#' @param clust_vec A vector specifying the number of points for each cluster.
#'                         If not provided, the n is divided equally
#'                         among the clusters.
#' @param num_noise The number of additional noise dimensions to be generated.
#' @param min_n The minimum value for the noise added to the data points.
#' @param max_n The maximum value for the noise added to the data points.
#'
#' @return A matrix containing the generated data, with each row representing a data point.
#' @export
#'
#' @examples
#'
#' # Generate curvy branching cluster data with custom parameters
#' set.seed(20240412)
#' data <- curvy_branch_clust(
#'   n = 300, clust_vec = c(100, 150, 50),
#'   num_noise = 2, min_n = -0.05, max_n = 0.05
#' )
curvy_branch_clust <- function(n, clust_vec, num_noise, min_n, max_n) {
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
  if (missing(clust_vec)) {
    # To check that the assigned n is divided by three
    if ((n %% 3) != 0) {
      warning("The sample size should be a product of three.")
      cluster_size <- floor(n / 3)
      clust_vec <- append(rep(cluster_size, 2), (n - cluster_size * 2))
    } else {
      cluster_size <- n / 3
      clust_vec <- rep(cluster_size, 3)
    }
  }

  theta <- stats::runif(clust_vec[1], 0.20, 0.90 * pi)

  df1 <- matrix(c(
    cos(theta) + stats::rnorm(clust_vec[1], 1, 0.06),
    sin(theta) + stats::rnorm(clust_vec[1], 1, 0.06),
    cos(theta) + stats::rnorm(clust_vec[1], 1, 0.06),
    sin(theta) + stats::rnorm(clust_vec[1], 1, 0.06)
  ), ncol = 4)


  theta1 <- stats::runif(clust_vec[3], 0.20, 0.90 * pi)

  df2 <- matrix(c(
    cos(-theta1) + stats::rnorm(clust_vec[3], 1, 0.06),
    sin(-theta1) + stats::rnorm(clust_vec[3], 1, 0.06),
    cos(-theta1) + stats::rnorm(clust_vec[3], 1, 0.06),
    sin(-theta1) + stats::rnorm(clust_vec[3], 1, 0.06)
  ), ncol = 4)

  df3 <- matrix(
    c(
      stats::rnorm(clust_vec[2], mean = 1, sd = 0.08),
      stats::rnorm(clust_vec[2], mean = 1, sd = 0.08),
      stats::rnorm(clust_vec[2], mean = 1, sd = 0.08),
      stats::rnorm(clust_vec[2], mean = 1, sd = 0.08)
    ),
    ncol = 4
  )

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

#' Generate Curvy Branching Cluster Data with Background Noise
#'
#' This function generates data with four clusters, two of which follow a
#' curvilinear pattern and the other two are distributed randomly.
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
#' # Generate curvy branching cluster data with background noise with custom parameters
#' set.seed(20240412)
#' data <- curvy_branch_clust_bkg(
#'   n = 400, num_noise = 2, min_n = -0.05,
#'   max_n = 0.05
#' )
curvy_branch_clust_bkg <- function(n, num_noise, min_n, max_n) {
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
  if ((n %% 4) != 0) {
    warning("The sample size should be a product of number of clusters.")
    cluster_size <- floor(n / 4)
  } else {
    cluster_size <- n / 4
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
    cos(-theta1) + stats::rnorm(cluster_size, 1, 0.06),
    sin(-theta1) + stats::rnorm(cluster_size, 1, 0.06),
    cos(-theta1) + stats::rnorm(cluster_size, 1, 0.06),
    sin(-theta1) + stats::rnorm(cluster_size, 1, 0.06)
  ), ncol = 4)

  df3 <- matrix(c(
    stats::rnorm(cluster_size, mean = 1, sd = 0.08),
    stats::rnorm(cluster_size, mean = 1, sd = 0.08),
    stats::rnorm(cluster_size, mean = 1, sd = 0.08),
    stats::rnorm(cluster_size, mean = 1, sd = 0.08)
  ), ncol = 4)

  df4 <- matrix(c(
    stats::rnorm(cluster_size, mean = 1, sd = 1),
    stats::rnorm(cluster_size, mean = 1, sd = 1),
    stats::rnorm(cluster_size, mean = 1, sd = 1),
    stats::rnorm(cluster_size, mean = 1, sd = 1)
  ), ncol = 4)

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

#' Generate Curvy Branching Clusters with Noise
#'
#' This function generates data with curvy branching clusters along with added noise.
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
#' # Generate curvy branching clusters with noise with custom parameters
#' set.seed(20240412)
#' data <- curvy_branch(n = 200, num_noise = 2, min_n = -0.05, max_n = 0.05)
curvy_branch <- function(n, num_noise, min_n, max_n) {
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
    cos(-theta1) + stats::rnorm(cluster_size, 1, 0.06),
    sin(-theta1) + stats::rnorm(cluster_size, 1, 0.06),
    cos(-theta1) + stats::rnorm(cluster_size, 1, 0.06),
    sin(-theta1) + stats::rnorm(cluster_size, 1, 0.06)
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
