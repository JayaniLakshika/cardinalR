#' Generate Triangular 3D Datasets with Noise
#'
#' This function generates triangular 3D datasets with added noise dimensions.
#'
#' @param n The total number of samples to generate.
#' @param num_noise The number of additional noise dimensions to add to the data.
#' @param min_n The minimum value for the noise dimensions.
#' @param max_n The maximum value for the noise dimensions.
#' @return A matrix containing the triangular 3D datasets with added noise.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' triangular_3d_data <- tri_3d(
#'   n = 100, num_noise = 2,
#'   min_n = -0.05, max_n = 0.05
#' )
tri_3d <- function(n, num_noise, min_n, max_n) {
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

  trace_point <- stats::runif(3)
  corner_points <- matrix(c(c(0, 1, 0.5, 0.5), c(0, 0, 1, 0.5), c(0, 0, 0, 1)),
    ncol = 3
  )

  df <- matrix(c(rep(0, n), rep(0, n), rep(0, n)), ncol = 3)
  for (i in 1:n) {
    trace_point <- (corner_points[sample(4, 1), ] + trace_point) / 2
    df[i, ] <- trace_point
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


#' Generate Triangular Plane with Background Noise
#'
#' This function generates a triangular plane dataset with background noise dimensions.
#'
#' @param n The total number of samples to generate.
#' @param num_noise The number of additional noise dimensions to add to the data.
#' @param min_n The minimum value for the noise dimensions.
#' @param max_n The maximum value for the noise dimensions.
#' @return A matrix containing the triangular plane dataset with background noise.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' triangular_plane_data <- tri_plane_bkg(
#'   n = 216,
#'   num_noise = 2, min_n = -0.05, max_n = 0.05
#' )
tri_plane_bkg <- function(n, num_noise, min_n, max_n) {
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


  trace_point <- stats::runif(2)
  corner_points <- matrix(c(c(0, 1, 0.5), c(0, 0, 1)), ncol = 2)
  df1 <- matrix(c(rep(0, n), rep(0, n)), ncol = 2)

  for (i in 1:cluster_size) {
    trace_point <- (corner_points[sample(3, 1), ] + trace_point) / 2
    df1[i, ] <- trace_point
  }

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

  df2 <- gen_bkg_noise(
    n = cluster_size, num_dims = NCOL(df1), mean = 0.025,
    sd = 0.5
  )

  df <- rbind(df1, df2, -df1)

  df
}
