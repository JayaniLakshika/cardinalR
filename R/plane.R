#' Generate points on a plane in 2D space
#'
#' This function generates points on a plane in 3D space based on the provided coefficients,
#' intercepts, and ranges for the parameters.
#'
#' @param sample_size The number of points to generate.
#' @param coefficient_x_1 The coefficient of the first parameter in the x-dimension equation.
#' @param coefficient_x_2 The coefficient of the second parameter in the x-dimension equation.
#' @param coefficient_y_1 The coefficient of the first parameter in the y-dimension equation.
#' @param coefficient_y_2 The coefficient of the second parameter in the y-dimension equation.
#' @param intercept_x The intercept for the x-dimension equation.
#' @param intercept_y The intercept for the y-dimension equation.
#' @param u_min The minimum value for the first parameter (u) range.
#' @param u_max The maximum value for the first parameter (u) range.
#' @param v_min The minimum value for the second parameter (v) range.
#' @param v_max The maximum value for the second parameter (v) range.
#' @param num_noise_dims The number of noise dimensions to add to the generated points.
#' @param min_noise The minimum value for the noise dimensions.
#' @param max_noise The maximum value for the noise dimensions.
#'
#' @return A matrix containing the generated points on the plane.
#'
#' @examples
#' plane_points <- plane(sample_size = 100, coefficient_x_1 = 1, coefficient_x_2 = 1,
#'                      coefficient_y_1 = -1, coefficient_y_2 = 1, intercept_x = -10,
#'                      intercept_y = 8, u_min = 10, u_max = 30, v_min = 10, v_max = 20,
#'                      num_noise_dims = 2, min_noise = 0, max_noise = 1)
#'
#' @export
plane <- function(sample_size, coefficient_x_1, coefficient_x_2, coefficient_y_1,
                  coefficient_y_2, intercept_x, intercept_y, u_min, u_max, v_min,
                  v_max, num_noise_dims, min_noise, max_noise) {

  u <- stats::runif(sample_size, min = u_min, max = u_max)
  v <- stats::runif(sample_size, min = v_min, max = v_max)
  x <- coefficient_x_1 * u + coefficient_x_2 * v + intercept_x
  y <- coefficient_y_1 * u + coefficient_y_2 * v + intercept_y

  plane_mat <- matrix(c(x, y), ncol = 2)

  if (num_noise_dims != 0) {

    noise_mat <- gen_noise_dims(n = dim(plane_mat)[1], num_noise_dims = num_noise_dims,
                                min_noise = min_noise, max_noise = max_noise)
    plane_mat <- cbind(plane_mat, noise_mat)

    plane_mat

  } else {

    plane_mat

  }

}
