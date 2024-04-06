#' Generate Coordinates for a Sphere
#'
#' This function generates the coordinates for a sphere in three-dimensional space.
#'
#' @param radius The radius of the sphere.
#' @param resolution The number of points used to approximate the surface of the sphere.
#' @param num_noise_dims The number of additional noise dimensions to add to the coordinates.
#' @param min_noise The minimum value for the random noise added to the coordinates.
#' @param max_noise The maximum value for the random noise added to the coordinates.
#'
#' @return A matrix containing the Cartesian coordinates of the points on the sphere.
#'
#' @examples
#' # Generate coordinates for a sphere with radius 1 and resolution 100
#' sphere(radius = 1, resolution = 20, num_noise_dims = 3, min_noise = -0.05, max_noise = 0.05)
#'
#' @export
sphere <- function(radius, resolution, num_noise_dims, min_noise, max_noise) {

  # Generate the coordinates for the sphere
  theta <- seq(0, 2*pi, length.out = resolution)
  phi <- seq(0, pi, length.out = resolution)
  coords <- expand.grid(theta = theta, phi = phi)

  # Convert spherical coordinates to Cartesian coordinates
  x <- radius * sin(coords$phi) * cos(coords$theta)
  y <- radius * sin(coords$phi) * sin(coords$theta)
  z <- radius * cos(coords$phi)

  sphere_mat <- matrix(c(x, y, z), ncol = 3)

  if (num_noise_dims != 0) {

    noise_mat <- gen_noise_dims(n = dim(sphere_mat)[1], num_noise_dims = num_noise_dims,
                                min_noise = min_noise, max_noise = max_noise)
    sphere_mat <- cbind(sphere_mat, noise_mat)

    sphere_mat

  } else {

    sphere_mat

  }

}

#' Generate data representing small spheres within a larger encompassing sphere with added noise.
#'
#' This function generates data points representing small spheres within a larger encompassing sphere
#' and adds noise to the data if specified.
#'
#' @param sample_size Total number of data points to generate, should be a multiple of 13.
#' @param num_noise_dims Number of additional noise dimensions to add to the data.
#' @param min_noise Minimum value for the noise added to the data.
#' @param max_noise Maximum value for the noise added to the data.
#'
#' @return A matrix containing the generated data points with or without added noise.
#'
#' @examples
#' small_big_sphere_with_noise(sample_size = 390, num_noise_dims = 4,
#' min_noise = -0.05, max_noise = 0.05)
#'
#' @export
small_big_sphere_with_noise <- function(sample_size, num_noise_dims, min_noise,
                                        max_noise) {

  # To check that the assigned sample_size is divided by thirteen
  if ((sample_size%%13) != 0) {
    warning("The sample size should be a product of thirteen.")
    small_sphere_sample_size <- floor(sample_size/13)

  } else {
    small_sphere_sample_size <- sample_size/13
  }

  m <- matrix(stats::rnorm(n = small_sphere_sample_size * 4), nrow = small_sphere_sample_size, ncol = 4)
  d_dim_sphere <- 3 * m / sqrt(rowSums(m * m))

  small_spheres <-
    replicate(3,
              sweep(
                d_dim_sphere, 2,
                stats::rnorm(n = 4, sd = 10 / sqrt(3)), `+`
              ),
              simplify = FALSE
    )

  # The larger encompassing sphere
  n_big_samples <- 10 * small_sphere_sample_size
  m <- matrix(stats::rnorm(n = n_big_samples * 4), nrow = n_big_samples, ncol = 4)
  big_sphere <- 3 * 5 * m / sqrt(rowSums(m * m))

  df <- rbind(do.call(rbind, small_spheres), big_sphere)

  if (num_noise_dims != 0) {

    noise_mat <- gen_noise_dims(n = dim(df)[1], num_noise_dims = num_noise_dims,
                                min_noise = min_noise, max_noise = max_noise)
    df <- cbind(df, noise_mat)

    df

  } else {

    df

  }

}
