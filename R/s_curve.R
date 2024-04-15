#' Generate S-curve Data
#'
#' This function generates S-curve data, which is a commonly used dataset for
#' testing and visualizing dimensionality reduction algorithms.
#'
#' @param n The number of samples to generate.
#' @param num_noise The number of additional noise dimensions to add to the data.
#' @param min_n The minimum value for the noise dimensions.
#' @param max_n The maximum value for the noise dimensions.
#' @return A matrix containing the generated S-curve data.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' s_curve_data <- scurve(
#'   n = 100, num_noise = 2,
#'   min_n = -0.05, max_n = 0.05
#' )
scurve <- function(n, num_noise, min_n, max_n) {
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

  a <- 3 * pi * stats::runif(n = n, min = -0.5, max = 0.5)
  x <- sin(a)
  y <- 2.0 * stats::runif(n = n)
  z <- sign(a) * (cos(a) - 1)

  scurve_mat <- matrix(c(x, y, z), ncol = 3)

  if (num_noise != 0) {
    if (missing(min_n)) {
      stop("Missing min_n.")
    }

    if (missing(max_n)) {
      stop("Missing max_n.")
    }

    noise_mat <- gen_noise_dims(
      n = dim(scurve_mat)[1], num_noise = num_noise,
      min_n = min_n, max_n = max_n
    )
    scurve_mat <- cbind(scurve_mat, noise_mat)

    scurve_mat
  } else {
    scurve_mat
  }
}

#' Generate S-curve Data with a Hole
#'
#' This function generates S-curve data with a hole by filtering out samples that
#' are not close to a specified anchor point.
#'
#' @param n The number of samples to generate.
#' @param num_noise The number of additional noise dimensions to add to the data.
#' @param min_n The minimum value for the noise dimensions.
#' @param max_n The maximum value for the noise dimensions.
#' @return A matrix containing the generated S-curve data with a hole.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' s_curve_hole_data <- scurve_hole(
#'   n = 100, num_noise = 2,
#'   min_n = -0.05, max_n = 0.05
#' )
scurve_hole <- function(n, num_noise, min_n, max_n) {
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

  scurve <- scurve(n = n, num_noise = 0)

  anchor <- c(0, 1, 0)
  indices <- rowSums((sweep(scurve, 2, anchor, `-`))^2) > 0.3
  scurve <- scurve[indices, ]
  rownames(scurve) <- NULL

  if (num_noise != 0) {
    if (missing(min_n)) {
      stop("Missing min_n.")
    }

    if (missing(max_n)) {
      stop("Missing max_n.")
    }

    noise_mat <- gen_noise_dims(
      n = dim(scurve)[1], num_noise = num_noise,
      min_n = min_n, max_n = max_n
    )
    scurve <- cbind(scurve, noise_mat)

    scurve
  } else {
    scurve
  }
}

#' Generate Two S-curve Datasets with Noise
#'
#' This function generates two S-curve datasets with added noise dimensions.
#'
#' @param n The total number of samples to generate (should be divisible by 2).
#' @param num_noise The number of additional noise dimensions to add to the data.
#' @param min_n The minimum value for the noise dimensions.
#' @param max_n The maximum value for the noise dimensions.
#' @return A matrix containing the combined S-curve datasets with added noise.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' two_s_curve_data <- two_scurves(
#'   n = 200, num_noise = 2,
#'   min_n = -0.05, max_n = 0.05
#' )
two_scurves <- function(n, num_noise, min_n, max_n) {
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


  df1 <- scurve(n = n, num_noise = 0)
  df2 <- matrix(c(-df1[, 1] + 5, df1[, 2] + 1, df1[, 3] + 1), ncol = 3)

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


#' Generate Mirror S-curve Datasets with Noise
#'
#' This function generates mirror S-curve datasets with added noise dimensions.
#'
#' @param n The total number of samples to generate (should be divisible by 2).
#' @param num_noise The number of additional noise dimensions to add to the data.
#' @param min_n The minimum value for the noise dimensions.
#' @param max_n The maximum value for the noise dimensions.
#' @return A matrix containing the combined mirror S-curve datasets with added noise.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' mirror_s_curve_data <- mirror_scurves(
#'   n = 200, num_noise = 2,
#'   min_n = -0.05, max_n = 0.05
#' )
mirror_scurves <- function(n, num_noise, min_n, max_n) {
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

  df1 <- scurve(n = n, num_noise = 0)
  df2 <- matrix(c(-df1[, 1] + 2, df1[, 2], df1[, 3]), ncol = 3)

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


#' Generate Two S-Curve Data with Noise
#'
#' This function generates two S-curve data with noise.
#'
#' @param n The total number of data points to be generated.
#' @param num_noise The number of additional noise dimensions to be generated.
#' @param min_n The minimum value for the noise added to the data points.
#' @param max_n The maximum value for the noise added to the data points.
#'
#' @return A matrix containing the two S-curve datasets with added noise.
#' @export
#'
#' @examples
#'
#' # Generate two S-curve data with noise with custom parameters
#' set.seed(20240412)
#' data <- two_scurve_hole(
#'   n = 200, num_noise = 2,
#'   min_n = -0.05, max_n = 0.05
#' )
two_scurve_hole <- function(n, num_noise, min_n, max_n) {
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

  ## S curve with a hole
  df1 <- scurve(n = cluster_size, num_noise = 0)
  df2 <- df1 + 1

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
  }


  return(df)
}
