#' Generate points on a plane in 2D space
#'
#' This function generates points on a plane in 3D space based on the provided coefficients,
#' intercepts, and ranges for the parameters.
#'
#' @param n The number of points to generate.
#' @param coef_x1 The coefficient of the first parameter in the x-dimension equation.
#' @param coef_x2 The coefficient of the second parameter in the x-dimension equation.
#' @param coef_y1 The coefficient of the first parameter in the y-dimension equation.
#' @param coef_y2 The coefficient of the second parameter in the y-dimension equation.
#' @param intercept_x The intercept for the x-dimension equation.
#' @param intercept_y The intercept for the y-dimension equation.
#' @param u_min The minimum value for the first parameter (u) range.
#' @param u_max The maximum value for the first parameter (u) range.
#' @param v_min The minimum value for the second parameter (v) range.
#' @param v_max The maximum value for the second parameter (v) range.
#' @param num_noise The number of noise dimensions to add to the generated points.
#' @param min_n The minimum value for the noise dimensions.
#' @param max_n The maximum value for the noise dimensions.
#'
#' @return A matrix containing the generated points on the plane.
#'
#' @examples
#' set.seed(20240412)
#' plane_points <- plane(
#'   n = 100, coef_x1 = 1, coef_x2 = 1,
#'   coef_y1 = -1, coef_y2 = 1, intercept_x = -10,
#'   intercept_y = 8, u_min = 10, u_max = 30, v_min = 10, v_max = 20,
#'   num_noise = 2, min_n = -0.05, max_n = 0.05
#' )
#'
#' @export
plane <- function(n, coef_x1, coef_x2, coef_y1, coef_y2, intercept_x,
                  intercept_y, u_min, u_max, v_min, v_max, num_noise,
                  min_n, max_n) {
  if (n <= 0) {
    stop("Number of points should be a positive number.")
  }

  if (num_noise < 0) {
    stop("Number of noise dimensions should be a positive number.")
  }

  if (missing(n)) {
    stop("Missing n.")
  }

  if (missing(coef_x1)) {
    stop("Missing coef_x1.")
  }

  if (missing(coef_x2)) {
    stop("Missing coef_x2.")
  }

  if (missing(coef_y1)) {
    stop("Missing coef_y1.")
  }

  if (missing(coef_y2)) {
    stop("Missing coef_y2.")
  }

  if (missing(intercept_x)) {
    stop("Missing coef_y1.")
  }

  if (missing(intercept_y)) {
    stop("Missing coef_y2.")
  }

  if (missing(u_min)) {
    stop("Missing u_min.")
  }

  if (missing(u_max)) {
    stop("Missing u_max.")
  }

  if (missing(v_min)) {
    stop("Missing u_min.")
  }

  if (missing(v_max)) {
    stop("Missing u_max.")
  }

  if (missing(num_noise)) {
    stop("Missing num_noise.")
  }

  u <- stats::runif(n, min = u_min, max = u_max)
  v <- stats::runif(n, min = v_min, max = v_max)
  x <- coef_x1 * u + coef_x2 * v + intercept_x
  y <- coef_y1 * u + coef_y2 * v + intercept_y

  plane_mat <- matrix(c(x, y), ncol = 2)

  if (num_noise != 0) {
    if (missing(min_n)) {
      stop("Missing min_n.")
    }

    if (missing(max_n)) {
      stop("Missing max_n.")
    }

    noise_mat <- gen_noise_dims(
      n = dim(plane_mat)[1], num_noise = num_noise,
      min_n = min_n, max_n = max_n
    )
    plane_mat <- cbind(plane_mat, noise_mat)

    plane_mat
  } else {
    plane_mat
  }
}

#' Generate Long Cluster Data
#'
#' This function generates a dataset consisting of two long clusters with added noise.
#'
#' @param n The total number of samples to generate.
#' @param num_noise The number of additional noise dimensions to add to the data.
#' @param min_n The minimum value for the noise dimensions.
#' @param max_n The maximum value for the noise dimensions.
#' @return A matrix containing the long cluster data with added noise.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' long_cluster <- two_long_clust(
#'   n = 200, num_noise = 2, min_n = -0.05,
#'   max_n = 0.05
#' )
two_long_clust <- function(n, num_noise, min_n, max_n) {
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

  x <- 0:(cluster_size - 1) + 0.03 * cluster_size * stats::rnorm(cluster_size)
  y <- 0:(cluster_size - 1) + 0.03 * cluster_size * stats::rnorm(cluster_size)
  df1 <- matrix(c(x, y), ncol = 2)

  x <- 0:(cluster_size - 1) + 0.03 * cluster_size * stats::rnorm(cluster_size) + cluster_size / 5
  y <- 0:(cluster_size - 1) + 0.03 * cluster_size * stats::rnorm(cluster_size) - cluster_size / 5
  df2 <- matrix(c(x, y), ncol = 2)

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

#' Generate Three Different Linear Data with Noise
#'
#' This function generates a dataset consisting of three different linear
#' patterns with added noise.
#'
#' @param n The total number of samples to generate.
#' @param num_noise The number of additional noise dimensions to add to the data.
#' @param min_n The minimum value for the noise dimensions.
#' @param max_n The maximum value for the noise dimensions.
#' @return A matrix containing the three different linear data with added noise.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' three_diff_linear <- three_diff_linear(
#'   n = 150, num_noise = 2, min_n = -0.05,
#'   max_n = 0.05
#' )
three_diff_linear <- function(n, num_noise, min_n, max_n) {
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

  x <- 0:(cluster_size - 1) + 0.03 * cluster_size * stats::rnorm(cluster_size)
  y <- 0:(cluster_size - 1) + 0.03 * cluster_size * stats::rnorm(cluster_size)
  df_1 <- matrix(c(x, y), ncol = 2)
  df1 <- matrix(c(x - 250, y - 20), ncol = 2)

  x <- 0:(cluster_size - 1) + 0.03 * cluster_size * stats::rnorm(cluster_size) + cluster_size / 5
  y <- 0:(cluster_size - 1) + 0.03 * cluster_size * stats::rnorm(cluster_size) - cluster_size / 5
  df2 <- matrix(c(x, y), ncol = 2)

  df3 <- matrix(c(-df_1[, 2], df_1[, 1]), ncol = 2)

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

#' Generate Four Different Long Clusters with Noise
#'
#' This function generates a dataset consisting of four different long clusters
#' with added noise.
#'
#' @param n The total number of samples to generate.
#' @param num_noise The number of additional noise dimensions to add to the data.
#' @param min_n The minimum value for the noise dimensions.
#' @param max_n The maximum value for the noise dimensions.
#' @return A matrix containing the four different long clusters with added noise.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' four_diff_long_clusters <- four_long_clust(
#'   n = 200, num_noise = 2,
#'   min_n = -0.05, max_n = 0.05
#' )
four_long_clust <- function(n, num_noise, min_n, max_n) {
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

  x <- 0:(cluster_size - 1) + 0.03 * cluster_size * stats::rnorm(cluster_size)
  y <- 0:(cluster_size - 1) + 0.03 * cluster_size * stats::rnorm(cluster_size)
  df_1 <- matrix(c(x, y), ncol = 2)
  df1 <- matrix(c(x - 150, y - 20), ncol = 2)

  x <- 0:(cluster_size - 1) + 0.03 * cluster_size * stats::rnorm(cluster_size) + cluster_size / 5
  y <- 0:(cluster_size - 1) + 0.03 * cluster_size * stats::rnorm(cluster_size) - cluster_size / 5
  df2 <- matrix(c(x, y), ncol = 2)
  df3 <- matrix(c(df_1[, 2] - 70, -df_1[, 1]), ncol = 2)
  df4 <- matrix(c(df3[, 1], df3[, 2] + 150), ncol = 2)

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

#' Generate 2D Plane with Hole and Noise
#'
#' This function generates a dataset representing a 2D plane with a hole in the
#' middle, with added noise.
#'
#' @param n The total number of samples to generate.
#' @param num_noise The number of additional noise dimensions to add to the data.
#' @param min_n The minimum value for the noise dimensions.
#' @param max_n The maximum value for the noise dimensions.
#' @return A list containing the 2D plane data with a hole and the sample size.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' plane_data <- plane_2d_hole(
#'   n = 100, num_noise = 2,
#'   min_n = -0.05, max_n = 0.05
#' )
plane_2d_hole <- function(n, num_noise, min_n, max_n) {
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
    stop("The sample size should be a product of four.")
  } else {
    cluster_size <- n / 4
  }

  u <- stats::runif(cluster_size, min = 10, max = 30)
  v <- stats::runif(cluster_size, min = 10, max = 20)
  x <- u + v - 10
  y <- v - u + 8
  df1 <- matrix(c(x, y), ncol = 2)

  anchor <- c(1, 1)
  indices <- rowSums((sweep(df1, 2, anchor, `-`))) > 20
  df1 <- df1[indices, ]
  rownames(df1) <- NULL

  df2 <- matrix(c(-df1[, 2] + 26, df1[, 1] - 15), ncol = 2)
  df3 <- matrix(c(df1[, 2] + 30, -df1[, 1] + 25), ncol = 2)

  df <- rbind(df1 - 10, df1 + 10, df2, df3)

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
  }

  return(list(df = df, n = NROW(df)))
}

#' Generate Four Long Clusters with Background Noise
#'
#' This function generates data with four long clusters along with background noise.
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
#' # Generate four long clusters with background noise with custom parameters
#' set.seed(20240412)
#' data <- four_long_clust_bkg(
#'   n = 400, num_noise = 4, min_n = -0.05,
#'   max_n = 0.05
#' )
four_long_clust_bkg <- function(n, num_noise, min_n, max_n) {
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

  x <- 0:(cluster_size - 1) + 0.03 * cluster_size * stats::rnorm(cluster_size)
  y <- 0:(cluster_size - 1) + 0.03 * cluster_size * stats::rnorm(cluster_size)
  df_1 <- matrix(c(x, y), ncol = 2)
  df1 <- matrix(c(df_1[, 1] - 20, df_1[, 2] - 20), ncol = 2)

  x <- 0:(cluster_size - 1) + 0.03 * cluster_size * stats::rnorm(cluster_size) + cluster_size / 5
  y <- 0:(cluster_size - 1) + 0.03 * cluster_size * stats::rnorm(cluster_size) - cluster_size / 5
  df_2 <- matrix(c(x, y), ncol = 2)

  df3 <- matrix(c(df_1[, 1] - 10, df_1[, 2] + 10), ncol = 2)
  df4 <- matrix(c(df_1[, 1] + 20, df_1[, 2] + 30), ncol = 2)

  df1 <- rbind(df1, df_2, df3, df4)

  if (num_noise != 0) {
    if (missing(min_n)) {
      stop("Missing min_n.")
    }

    if (missing(max_n)) {
      stop("Missing max_n.")
    }

    noise_mat <- gen_noise_dims(
      n = dim(df1)[1], num_noise = num_noise,
      min_n = min_n, max_n = max_n
    )
    df1 <- cbind(df1, noise_mat)
  }

  df2 <- gen_bkg_noise(n = cluster_size, num_dims = NCOL(df1), mean = 0, sd = 10)

  df <- rbind(df1, df2)

  df
}

#' Generate Three Linear Clusters with Noise
#'
#' This function generates data with three linear clusters, along with added noise.
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
#' # Generate three linear clusters with noise with custom parameters
#' set.seed(20240412)
#' data <- three_long_clust(n = 300, num_noise = 2, min_n = -0.05, max_n = 0.05)
three_long_clust <- function(n, num_noise, min_n, max_n) {
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

  x <- 0:(cluster_size - 1) + 0.03 * cluster_size * stats::rnorm(cluster_size)
  y <- 0:(cluster_size - 1) + 0.03 * cluster_size * stats::rnorm(cluster_size)
  df_1 <- matrix(c(x, y), ncol = 2)
  df1 <- matrix(c(df_1[, 1] - 20, df_1[, 2] - 20), ncol = 2)

  x <- 0:(cluster_size - 1) + 0.03 * cluster_size * stats::rnorm(cluster_size) + cluster_size / 5
  y <- 0:(cluster_size - 1) + 0.03 * cluster_size * stats::rnorm(cluster_size) - cluster_size / 5
  df2 <- matrix(c(x, y), ncol = 2)

  df3 <- matrix(c(df_1[, 1] - 10, df_1[, 2] + 10), ncol = 2)

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

#' Generate Two Linear Differentiated Clusters with Noise
#'
#' This function generates data with two linear clusters that are differentiated
#'  from each other, along with added noise.
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
#' # Generate two linear differentiated clusters with noise with custom parameters
#' set.seed(20240412)
#' data <- two_long_clust_diff(
#'   n = 300, num_noise = 2, min_n = -0.05,
#'   max_n = 0.05
#' )
two_long_clust_diff <- function(n, num_noise, min_n, max_n) {
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

  x <- 0:(cluster_size - 1) + 0.03 * cluster_size * stats::rnorm(cluster_size)
  y <- 0:(cluster_size - 1) + 0.03 * cluster_size * stats::rnorm(cluster_size)
  df_1 <- matrix(c(x, y), ncol = 2)
  df1 <- matrix(c(df_1[, 1] - 20, df_1[, 2] - 20), ncol = 2)

  x <- 0:(cluster_size - 1) + 0.03 * cluster_size * stats::rnorm(cluster_size) + cluster_size / 5
  y <- 0:(cluster_size - 1) + 0.03 * cluster_size * stats::rnorm(cluster_size) - cluster_size / 5
  df2 <- matrix(c(x, y), ncol = 2)
  df3 <- matrix(c(df_1[, 1] + 10, df_1[, 2] + 10), ncol = 2)

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
