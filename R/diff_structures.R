# Function to gen a eliplical cluster in 4D space with an offset
gen_elliptical_cluster_4d <- function(n, axes_lengths, offset) {
  if (n <= 0) {
    stop("Number of points should be a positive number.")
  }

  # gen points uniformly distributed on the surface of a 4D unit sphere
  sphere_points <- matrix(rnorm(n * 4), nrow = n, ncol = 4)
  sphere_points <- t(apply(sphere_points, 1, function(row) row / sqrt(sum(row^2))))

  colnames(sphere_points) <- paste0("x", 1:4)

  # Scale the points to fit the ellipse defined by axes_lengths
  elliptical_points <- sweep(sphere_points, 2, axes_lengths, "*")

  # Add offset to shift the ellipse position
  elliptical_df <- sweep(elliptical_points, 2, offset, "+") |>
    as_tibble()

  elliptical_df
}

# Load necessary library for multivariate normal distribution
library(MASS)

# Function to gen a Gaussian cluster in 4D
gen_gaussian_cluster_4d <- function(n, mean_vec = c(0, 0, 0, 0), cov_mat = diag(4) * 0.1, offset) {
  if (n <= 0) {
    stop("Number of points should be a positive integer.")
  }

  # gen points from a 4D multivariate normal distribution
  gaussian_cluster <- mvrnorm(n = n, mu = mean_vec, Sigma = cov_mat)
  colnames(gaussian_cluster) <- paste0("x", 1:4)

  gaussian_cluster <- sweep(gaussian_cluster, 2, offset, "+") |>
    as_tibble()

  return(gaussian_cluster)
}

# Function to gen points on a 4D hemisphere
gen_hemisphere_4d <- function(n, radius = 1, offset) {
  # Step 1: Random angles for spherical coordinates
  theta1 <- runif(n, 0, pi)        # Angle for (x1, x2) plane (azimuth)
  theta2 <- runif(n, 0, pi)        # Angle for (x2, x3) plane (elevation)
  theta3 <- runif(n, 0, pi / 2)    # Angle for (x3, x4), restricted for hemisphere

  # Step 2: Convert spherical coordinates to Cartesian coordinates in 4D
  x1 <- radius * sin(theta1) * cos(theta2)  # x1 coordinate
  x2 <- radius * sin(theta1) * sin(theta2)  # x2 coordinate
  x3 <- radius * cos(theta1) * cos(theta3)  # x3 coordinate
  x4 <- radius * cos(theta1) * sin(theta3)  # x4 coordinate (restricted to hemisphere)

  # Combine the coordinates into a 4D dataset
  hemisphere_4d <- cbind(x1, x2, x3, x4)

  # Apply the offset to shift the cluster
  hemisphere_4d <- sweep(hemisphere_4d, 2, offset, "+") |>
    as_tibble()

  return(hemisphere_4d)
}

# Function to gen a small cube in 4D space
gen_cube_4d <- function(n, side_length = 1, center_point = c(0, 0, 0, 0)) {
  if (n <= 0) {
    stop("Number of points should be a positive number.")
  }

  # Generate random points in 4D space between -side_length/2 and side_length/2
  points <- matrix(runif(n * 4, -side_length / 2, side_length / 2), nrow = n, ncol = 4)

  # Shift the points to be centered around the specified center point
  cube_centered <- sweep(points, 2, center_point, "+")
  colnames(cube_centered) <- paste0("x", 1:4)

  cube_centered <- cube_centered |>
    as_tibble()

  return(cube_centered)
}
