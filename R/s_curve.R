#' Generate S-curve Data
#'
#' This function generates S-curve data, which is a commonly used dataset for
#' testing and visualizing dimensionality reduction algorithms.
#'
#' @param sample_size The number of samples to generate.
#' @param num_noise_dims The number of additional noise dimensions to add to the data.
#' @param min_noise The minimum value for the noise dimensions.
#' @param max_noise The maximum value for the noise dimensions.
#' @return A matrix containing the generated S-curve data.
#' @export
#'
#' @examples
#' s_curve_data <- s_curve(sample_size = 100, num_noise_dims = 3,
#' min_noise = -0.5, max_noise = 0.5)
s_curve <- function(sample_size, num_noise_dims, min_noise, max_noise) {
  a <- 3 * pi * stats::runif(n = sample_size, min = -0.5, max = 0.5)
  x <- sin(a)
  y <- 2.0 * stats::runif(n = sample_size)
  z <- sign(a) * (cos(a) - 1)

  scurve_mat <- matrix(c(x, y, z), ncol = 3)

  if (num_noise_dims != 0) {

    noise_mat <- gen_noise_dims(n = dim(scurve_mat)[1], num_noise_dims = num_noise_dims,
                                min_noise = min_noise, max_noise = max_noise)
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
#' @param sample_size The number of samples to generate.
#' @param num_noise_dims The number of additional noise dimensions to add to the data.
#' @param min_noise The minimum value for the noise dimensions.
#' @param max_noise The maximum value for the noise dimensions.
#' @return A matrix containing the generated S-curve data with a hole.
#' @export
#'
#' @examples
#' s_curve_hole_data <- s_curve_hole(sample_size = 100, num_noise_dims = 3,
#' min_noise = -0.5, max_noise = 0.5)
s_curve_hole <- function(sample_size, num_noise_dims, min_noise, max_noise) {
  scurve <- s_curve(sample_size = sample_size, num_noise_dims = 0)

  anchor <- c(0, 1, 0)
  indices <- rowSums((sweep(scurve, 2, anchor, `-`))^2) > 0.3
  scurve <- scurve[indices, ]
  rownames(scurve) <- NULL

  if (num_noise_dims != 0) {

    noise_mat <- gen_noise_dims(n = dim(scurve)[1], num_noise_dims = num_noise_dims,
                                min_noise = min_noise, max_noise = max_noise)
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
#' @param sample_size The total number of samples to generate (should be divisible by 2).
#' @param num_noise_dims The number of additional noise dimensions to add to the data.
#' @param min_noise The minimum value for the noise dimensions.
#' @param max_noise The maximum value for the noise dimensions.
#' @return A matrix containing the combined S-curve datasets with added noise.
#' @export
#'
#' @examples
#' two_s_curve_data <- two_s_curves_with_noise(sample_size = 200, num_noise_dims = 8,
#' min_noise = -0.5, max_noise = 0.5)
two_s_curves_with_noise <- function(sample_size, num_noise_dims, min_noise,
                                    max_noise) {

  # To check that the assigned sample_size is divided by two
  if ((sample_size%%2) != 0) {
    warning("The sample size should be a product of two.")
    cluster_size <- floor(sample_size/2)

  } else {
    cluster_size <- sample_size/2
  }


  df1 <- s_curve(sample_size = sample_size, num_noise_dims = 0)
  df2 <- matrix(c(-df1[,1] + 5, df1[,2] + 1, df1[,3] + 1), ncol = 3)

  df <- rbind(df1, df2)

  if (num_noise_dims != 0) {

    noise_mat <- gen_noise_dims(n = dim(df)[1], num_noise_dims = num_noise_dims,
                                min_noise = min_noise, max_noise = max_noise)
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
#' @param sample_size The total number of samples to generate (should be divisible by 2).
#' @param num_noise_dims The number of additional noise dimensions to add to the data.
#' @param min_noise The minimum value for the noise dimensions.
#' @param max_noise The maximum value for the noise dimensions.
#' @return A matrix containing the combined mirror S-curve datasets with added noise.
#' @export
#'
#' @examples
#' mirror_s_curve_data <- mirror_s_curves_with_noise(sample_size = 200, num_noise_dims = 8,
#' min_noise = -0.5, max_noise = 0.5)
mirror_s_curves_with_noise <- function(sample_size, num_noise_dims, min_noise,
                                       max_noise) {

  # To check that the assigned sample_size is divided by two
  if ((sample_size%%2) != 0) {
    warning("The sample size should be a product of two.")
    cluster_size <- floor(sample_size/2)

  } else {
    cluster_size <- sample_size/2
  }

  df1 <- s_curve(sample_size = sample_size, num_noise_dims = 0)
  df2 <- matrix(c(-df1[,1] + 2, df1[,2], df1[,3]), ncol = 3)

  df <- rbind(df1, df2)

  if (num_noise_dims != 0) {

    noise_mat <- gen_noise_dims(n = dim(df)[1], num_noise_dims = num_noise_dims,
                                min_noise = min_noise, max_noise = max_noise)
    df <- cbind(df, noise_mat)

    df

  } else {

    df

  }

}
