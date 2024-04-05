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
