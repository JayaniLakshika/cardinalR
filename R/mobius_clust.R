#' Generate a Single Row for a 5-D Mobius Strip
#'
#' This function generates a single row of data representing a point on a 5-dimensional Mobius strip.
#'
#' @return A vector containing the coordinates of the point on the Mobius strip.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' mobius_row <- mobius_5d_row()
mobius_5d_row <- function() {
  ## Generates Angles
  a <- stats::runif(1, min = 0, max = 2 * pi)
  a <- c(a, a / 2)

  ## Generates Small Radius
  radius <- c(1, stats::runif(1, min = -.4, max = .4))

  ## Generates Row of Data
  mobius <- c(
    (cos(a[2]) * radius[2] + radius[1]) * cos(a[1]),
    (cos(a[2]) * radius[2] + radius[1]) * sin(a[1]),
    sin(a[2]) * radius[2]
  )

  k <- stats::runif(1, min = 0, max = pi)
  ## Rot over x axis
  rot_1 <- matrix(
    c(
      0,
      cos(k),
      -sin(k),
      1, 0, 0, 0,
      sin(k),
      cos(k)
    ),
    ncol = 3, byrow = TRUE
  )
  ## Rot over z axis
  rot_2 <- matrix(
    c(
      cos(2 * k),
      -sin(2 * k),
      0,
      sin(2 * k),
      cos(2 * k),
      0, 0, 0, 1
    ),
    ncol = 3, byrow = TRUE
  )
  ## Trans perpendicular to z axis
  trans <- matrix(c(4 * cos(2 * k), 4 * sin(2 * k), 0), ncol = 1)

  mobius <- rot_2 %*% rot_1 %*% mobius + trans

  mobius
}

#' Generate a 5-D Mobius Strip
#'
#' This function generates a dataset representing a 5-dimensional Mobius strip.
#'
#' @param n The number of points to generate for the Mobius strip.
#' @param num_noise The number of additional noise dimensions to add to the data.
#' @param min_n The minimum value for the noise dimensions.
#' @param max_n The maximum value for the noise dimensions.
#' @return A matrix containing the generated Mobius strip.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' mobius_data <- mobius_5d(n = 100, num_noise = 2, min_n = -0.05, max_n = 0.05)
mobius_5d <- function(n, num_noise, min_n, max_n) {
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

  df_list <- lapply(1:n, function(i) mobius_5d_row())
  df <- purrr::reduce(df_list, rbind)

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

#' Generate Mobius Cluster with Noise
#'
#' This function generates a dataset consisting of a mobius cluster with added noise.
#'
#' @param n The total number of samples to generate.
#' @param num_noise The number of additional noise dimensions to add to the data.
#' @param min_n The minimum value for the noise dimensions.
#' @param max_n The maximum value for the noise dimensions.
#' @return A matrix containing the mobius cluster with added noise.
#' @export
#'
#' @examples
#' mobius_cluster <- mobius_clust(
#'   n = 200, num_noise = 2, min_n = -0.05,
#'   max_n = 0.05
#' )
mobius_clust <- function(n, num_noise, min_n, max_n) {
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

  df1 <- mobius_5d(n = n * 0.80, num_noise = 0)

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

  ## To add background noise
  df2 <- gen_bkg_noise(n = n * 0.20, num_dims = NCOL(df1), mean = 0, sd = 0.3)
  df <- rbind(df1, df2)
  df
}
