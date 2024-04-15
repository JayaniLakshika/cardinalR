#' Generate points on a curvilinear 2D manifold
#'
#' This function generates points on a curvilinear 2D manifold based on a nonlinear equation.
#'
#' @param n The number of points to generate.
#' @param num_noise The number of noise dimensions to add to the generated points.
#' @param min_n The minimum value for the noise dimensions.
#' @param max_n The maximum value for the noise dimensions.
#'
#' @return A matrix containing the generated points on the curvilinear 2D manifold.
#'
#' @examples
#' set.seed(20240412)
#' curvilinear_points <- curv_2d(
#'   n = 100, num_noise = 2, min_n = -0.01,
#'   max_n = 0.01
#' )
#'
#' @export
curv_2d <- function(n, num_noise, min_n, max_n) {
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

  x <- stats::runif(n, 0, 2)
  y <- -(x^3 + stats::runif(n, 0, 3)) + stats::runif(n, 0, 0.5)

  curvilinear_mat <- matrix(c(x, y), ncol = 2)

  if (num_noise != 0) {
    if (missing(min_n)) {
      stop("Missing min_n.")
    }

    if (missing(max_n)) {
      stop("Missing max_n.")
    }

    noise_mat <- gen_noise_dims(
      n = dim(curvilinear_mat)[1], num_noise = num_noise,
      min_n = min_n, max_n = max_n
    )
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
#' @param n The number of points to generate.
#' @param num_noise The number of noise dimensions to add to the generated points.
#' @param min_n The minimum value for the noise dimensions.
#' @param max_n The maximum value for the noise dimensions.
#'
#' @return A matrix containing the generated points on the nonlinear 2D manifold.
#'
#' @examples
#' set.seed(20240412)
#' nonlinear_points <- nonlinear_2d(
#'   n = 100, num_noise = 2, min_n = -0.01,
#'   max_n = 0.01
#' )
#'
#' @export
nonlinear_2d <- function(n, num_noise, min_n, max_n) {
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

  theta <- stats::runif(n, 0.2, 0.6 * pi)
  x <- cos(theta) + stats::rnorm(n, 10, 0.03)
  y <- sin(theta) + stats::rnorm(n, 10, 0.03)

  nonlinear_mat <- matrix(c(x, y), ncol = 2)

  if (num_noise != 0) {
    if (missing(min_n)) {
      stop("Missing min_n.")
    }

    if (missing(max_n)) {
      stop("Missing max_n.")
    }

    noise_mat <- gen_noise_dims(
      n = dim(nonlinear_mat)[1], num_noise = num_noise,
      min_n = min_n, max_n = max_n
    )
    nonlinear_mat <- cbind(nonlinear_mat, noise_mat)

    nonlinear_mat
  } else {
    nonlinear_mat
  }
}

#' Generate Sine Curve Data with Noise
#'
#' This function generates a dataset representing a sine curve with added noise.
#'
#' @param n The number of samples to generate.
#' @param num_noise The number of additional noise dimensions to add to the data.
#' @param min_n The minimum value for the noise dimensions.
#' @param max_n The maximum value for the noise dimensions.
#' @return A matrix containing the sine curve data with noise.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' sine_curve <- sine_curve(n = 100, num_noise = 2, min_n = -0.05, max_n = 0.05)
sine_curve <- function(n, num_noise, min_n, max_n) {
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

  theta <- stats::runif(n, 0, 1.80 * pi)
  x <- theta
  y <- sin(theta)
  df <- matrix(c(x, y), ncol = 2)

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

#' Generate Nonlinear Connected Data with Noise
#'
#' This function generates a dataset representing nonlinear connected clusters with added noise.
#'
#' @param n The total number of samples to generate.
#' @param num_noise The number of additional noise dimensions to add to the data.
#' @param min_n The minimum value for the noise dimensions.
#' @param max_n The maximum value for the noise dimensions.
#' @return A matrix containing the nonlinear connected data with noise.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' nonlinear_connect <- nonlinear_connect(
#'   n = 400, num_noise = 2, min_n = -0.05,
#'   max_n = 0.05
#' )
nonlinear_connect <- function(n, num_noise, min_n, max_n) {
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
    warning("The sample size should be a product of four.")
    cluster_size <- floor(n / 4)
  } else {
    cluster_size <- n / 4
  }

  theta <- stats::runif(cluster_size, 0, 0.80 * pi)
  x <- cos(theta) + stats::rnorm(cluster_size, 10, 0.03)
  y <- sin(theta) + stats::rnorm(cluster_size, 10, 0.03)
  z <- rep(0, cluster_size) + stats::rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - stats::rnorm(cluster_size, 10, 0.03)

  df1 <- matrix(c(x, y, z, w), ncol = 4)

  x <- cos(-theta) + stats::rnorm(cluster_size, 10, 0.03) + stats::rnorm(cluster_size, 0.1, 0)
  y <- sin(-theta) + stats::rnorm(cluster_size, 10, 0.03) + stats::rnorm(cluster_size, 0.1, 0)
  z <- rep(0, cluster_size) + stats::rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - stats::rnorm(cluster_size, 10, 0.03)

  df2 <- matrix(c(x, y, z, w), ncol = 4)

  x <- cos(-theta) + stats::rnorm(cluster_size, 10, 0.03) + stats::rnorm(cluster_size, 0.1, 0)
  z <- sin(-theta) + stats::rnorm(cluster_size, 10, 0.03) + stats::rnorm(cluster_size, 0.1, 0)
  y <- rep(0, cluster_size) + stats::rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - stats::rnorm(cluster_size, 10, 0.03)

  df3 <- matrix(c(x, y, z, w), ncol = 4)

  x <- cos(theta) + stats::rnorm(cluster_size, 10, 0.03) + stats::rnorm(cluster_size, 0.1, 0)
  z <- sin(theta) + stats::rnorm(cluster_size, 10, 0.03) + stats::rnorm(cluster_size, 0.1, 0)
  y <- rep(0, cluster_size) + stats::rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - stats::rnorm(cluster_size, 10, 0.03)

  df4 <- matrix(c(x, y, z, w), ncol = 4)

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


#' Generate Nonlinear Mirror Data with Noise
#'
#' This function generates a dataset representing two mirror-image clusters with added noise.
#'
#' @param n The total number of samples to generate.
#' @param num_noise The number of additional noise dimensions to add to the data.
#' @param min_n The minimum value for the noise dimensions.
#' @param max_n The maximum value for the noise dimensions.
#' @return A matrix containing the nonlinear mirror data with noise.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' nonlinear_mirror <- nonlinear_mirror(
#'   n = 400, num_noise = 2, min_n = -0.05,
#'   max_n = 0.05
#' )
nonlinear_mirror <- function(n, num_noise, min_n, max_n) {
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
  z <- rep(0, cluster_size) + stats::rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - stats::rnorm(cluster_size, 10, 0.03)

  df1 <- matrix(c(x, y, z, w), ncol = 4)

  x <- stats::runif(cluster_size, -8, 1.5)
  y <- (exp(x) + stats::runif(cluster_size, 0, 1)) + stats::runif(cluster_size, 0, 0.7)
  z <- rep(0, cluster_size) + stats::rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - stats::rnorm(cluster_size, 10, 0.03)

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

#' Generate Two Curvy Pancakes with Noise
#'
#' This function generates a dataset representing two curvy pancake-shaped clusters with added noise.
#'
#' @param n The total number of samples to generate.
#' @param num_noise The number of additional noise dimensions to add to the data.
#' @param min_n The minimum value for the noise dimensions.
#' @param max_n The maximum value for the noise dimensions.
#' @return A matrix containing the two curvy pancakes data with noise.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' two_curvy_panckakes <- two_curvy_panckakes(
#'   n = 300, num_noise = 2,
#'   min_n = -0.05, max_n = 0.05
#' )
two_curvy_panckakes <- function(n, num_noise, min_n, max_n) {
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

  phi <- stats::runif(cluster_size, max = 2 * pi)
  rho <- sqrt(stats::runif(cluster_size))

  theta <- stats::runif(cluster_size, 0, 1.80 * pi)
  x <- theta
  y <- sin(theta)
  df1 <- matrix(c(x, y, sqrt(1) * rho * cos(phi) + 4, sqrt(1) * rho * sin(phi) + 4), ncol = 4)
  df2 <- matrix(c(x + 1, y + 1, sqrt(1) * rho * cos(phi) + 6, sqrt(1) * rho * sin(phi) + 6), ncol = 4)

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

#' Generate Two Curvilinear Data with Noise
#'
#' This function generates a dataset representing two curvilinear clusters with added noise.
#'
#' @param n The total number of samples to generate.
#' @param num_noise The number of additional noise dimensions to add to the data.
#' @param min_n The minimum value for the noise dimensions.
#' @param max_n The maximum value for the noise dimensions.
#' @return A matrix containing the two curvilinear data with noise.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' two_curvilinear <- two_curvilinear(
#'   n = 250, num_noise = 2, min_n = -0.05,
#'   max_n = 0.05
#' )
two_curvilinear <- function(n, num_noise, min_n, max_n) {
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
  if (((n - n * 0.2) %% 2) != 0) {
    warning("The sample size should be a product of two.")
    cluster_size <- floor((n - n * 0.2) / 2)
  } else {
    cluster_size <- (n - n * 0.2) / 2
  }

  x <- stats::runif(cluster_size, -2, -0.5)
  y <- (x^2 + stats::runif(cluster_size, 0, 0.1)) + stats::runif(cluster_size, 0, 0.2)
  z <- rep(0, cluster_size) + stats::rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - stats::rnorm(cluster_size, 10, 0.03)

  df1 <- matrix(c(x, y, z, w), ncol = 4)

  x <- stats::runif(cluster_size, 0.5, 2)
  y <- (x^2 + stats::runif(cluster_size, 0, 0.1)) + stats::runif(cluster_size, 0, 0.2)
  z <- rep(0, cluster_size) + stats::rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - stats::rnorm(cluster_size, 10, 0.03)

  df2 <- matrix(c(x, y, z, w), ncol = 4)

  x <- stats::rnorm(n * 0.2, mean = 0, sd = 0.4)
  y <- stats::rnorm(n * 0.2, mean = 1.5, sd = 0.5)
  z <- rep(0, n * 0.2) + stats::rnorm(n * 0.2, 10, 0.03)
  w <- rep(0, n * 0.2) - stats::rnorm(n * 0.2, 10, 0.03)

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

#' Generate Swiss Roll Data
#'
#' This function generates data points in the shape of a Swiss roll.
#'
#' @param n The total number of data points to be generated.
#' @param num_noise The number of additional noise dimensions to be generated.
#' @param min_n The minimum value for the noise added to the data points.
#' @param max_n The maximum value for the noise added to the data points.
#'
#' @return A matrix containing the generated Swiss roll data points.
#' @export
#'
#' @examples
#'
#' # Generate Swiss roll data with noise with custom parameters
#' set.seed(20240412)
#' data <- swiss_roll(n = 200, num_noise = 2, min_n = -0.05, max_n = 0.05)
swiss_roll <- function(n, num_noise, min_n, max_n) {
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

  phi <- stats::runif(n, min = 1.5 * pi, max = 4.5 * pi)
  x <- phi * cos(phi)
  y <- phi * sin(phi)
  z <- stats::runif(n, max = 10)
  df <- matrix(c(x, y, z), ncol = 3)

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
