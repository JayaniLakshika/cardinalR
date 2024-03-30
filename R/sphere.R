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

    # Initialize an empty list to store the vectors
    noise_dim_val_list <- list()

    for (j in 1:num_noise_dims) {
      if ((j%%2) == 0) {
        noise_dim_val_list[[j]] <- stats::runif(dim(sphere_mat)[1],
                                                min = min_noise, max = max_noise)
      } else {
        noise_dim_val_list[[j]] <- (-1) * stats::runif(dim(sphere_mat)[1],
                                                       min = min_noise, max = max_noise)
      }


    }

    noise_mat <- matrix(unlist(noise_dim_val_list), ncol = num_noise_dims)
    sphere_mat <- cbind(sphere_mat, noise_mat)

    sphere_mat

  } else {

    sphere_mat

  }

}
