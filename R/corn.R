# Function to gen a corn-shaped cluster in 4D space with an offset
gen_blunted_corn_cluster_4d <- function(n, height, base_radius, tip_radius, tip_point) {
  if (n <= 0) {
    stop("Number of points should be a positive number.")
  }

  # gen points with a higher density near the tip
  # Use an exponential distribution to cluster more points closer to the tip
  height_values <- rexp(n, rate = 1 / (height / 2))  # Exponentially distributed heights
  height_values <- pmin(height_values, height)  # Cap heights to the maximum height

  # Radius decreases linearly from the base to the tip
  radii <- tip_radius + (base_radius - tip_radius) * (height_values / height)

  # gen points uniformly distributed in the cross-section (circle) at each height level
  theta <- runif(n, 0, 2 * pi)
  phi <- runif(n, 0, 2 * pi)

  x1 <- radii * cos(theta) * sin(phi)
  x2 <- radii * sin(theta) * sin(phi)
  x3 <- radii * cos(theta)
  x4 <- height_values

  # Shift the cluster towards the tip point
  corn_cluster <- cbind(x1, x2, x3, x4)
  corn_cluster <- sweep(corn_cluster, 2, tip_point, "+") |>
    as_tibble()

  corn_cluster
}

gen_blunted_corn_cluster_flip1_4d <- function(n, height, base_radius, tip_radius, tip_point) {
  if (n <= 0) {
    stop("Number of points should be a positive number.")
  }

  # gen points with a higher density near the tip
  # Use an exponential distribution to cluster more points closer to the tip
  height_values <- rexp(n, rate = 1 / (height / 2))  # Exponentially distributed heights
  height_values <- pmin(height_values, height)  # Cap heights to the maximum height

  # Radius decreases linearly from the base to the tip
  radii <- tip_radius + (base_radius - tip_radius) * (height_values / height)

  # gen points uniformly distributed in the cross-section (circle) at each height level
  theta <- runif(n, 0, 2 * pi)
  phi <- runif(n, 0, 2 * pi)

  x1 <- radii * sin(theta) * sin(phi)
  x2 <- height_values
  x3 <- radii * cos(theta) * sin(phi)
  x4 <- radii * cos(theta)

  # Shift the cluster towards the tip point
  corn_cluster <- cbind(x1, x2, x3, x4)
  corn_cluster <- sweep(corn_cluster, 2, tip_point, "+") |>
    as_tibble()

  corn_cluster
}

gen_blunted_corn_cluster_flip2_4d <- function(n, height, base_radius, tip_radius, tip_point) {
  if (n <= 0) {
    stop("Number of points should be a positive number.")
  }

  # gen points with a higher density near the tip
  # Use an exponential distribution to cluster more points closer to the tip
  height_values <- rexp(n, rate = 1 / (height / 2))  # Exponentially distributed heights
  height_values <- pmin(height_values, height)  # Cap heights to the maximum height

  # Radius decreases linearly from the base to the tip
  radii <- tip_radius + (base_radius - tip_radius) * (height_values / height)

  # gen points uniformly distributed in the cross-section (circle) at each height level
  theta <- runif(n, 0, 2 * pi)
  phi <- runif(n, 0, 2 * pi)

  x2 <- radii * sin(theta) * sin(phi)
  x3 <- height_values
  x4 <- radii * cos(theta) * sin(phi)
  x1 <- radii * cos(theta)

  # Shift the cluster towards the tip point
  corn_cluster <- cbind(x1, x2, x3, x4)
  corn_cluster <- sweep(corn_cluster, 2, tip_point, "+") |>
    as_tibble()

  corn_cluster
}

# Function to gen a corn-shaped cluster in 4D with a rectangular base
gen_corn_cluster_rectangular_base_4d <- function(n, height = 5, base_width_x = 3, base_width_y = 2, tip_radius = 0.5, tip_point = c(0, 0, 0, 0)) {
  if (n <= 0) {
    stop("Number of points should be a positive number.")
  }

  # gen points with a higher density near the tip
  height_values <- rexp(n, rate = 1 / (height / 2))  # Exponentially distributed heights
  height_values <- pmin(height_values, height)  # Cap heights to the maximum height

  # Base dimensions decrease linearly as height increases
  x_radii <- tip_radius + (base_width_x - tip_radius) * (height_values / height)
  y_radii <- tip_radius + (base_width_y - tip_radius) * (height_values / height)

  # gen points uniformly within the rectangular cross-section at each height level
  x1 <- runif(n, -x_radii, x_radii)
  x2 <- runif(n, -y_radii, y_radii)
  x3 <- runif(n, -x_radii, x_radii)  # For the third dimension, using the same range as x
  x4 <- height_values  # Fourth dimension is the height

  # Shift the cluster towards the tip point (e.g., offset)
  corn_cluster <- cbind(x1, x2, x3, x4)
  corn_cluster <- sweep(corn_cluster, 2, tip_point, "+") |>
    as_tibble()

  return(corn_cluster)
}

# Function to gen a corn-shaped cluster in 4D with a triangular base
gen_corn_cluster_triangular_base_4d <- function(n, height = 5, base_width = 3, tip_radius = 0.5, tip_point = c(0, 0, 0, 0)) {
  if (n <= 0) {
    stop("Number of points should be a positive number.")
  }

  # gen points with a higher density near the tip
  height_values <- rexp(n, rate = 1 / (height / 2))  # Exponentially distributed heights
  height_values <- pmin(height_values, height)  # Cap heights to the maximum height

  # Base size decreases linearly as height increases
  radii <- tip_radius + (base_width - tip_radius) * (height_values / height)

  # gen points within a triangular cross-section at each height level
  # Using barycentric coordinates to gen points inside a triangle
  u <- runif(n)
  v <- runif(n)
  is_outside <- (u + v) > 1
  u[is_outside] <- 1 - u[is_outside]
  v[is_outside] <- 1 - v[is_outside]

  # Scale the triangular points by the radius (for tapering)
  x1 <- radii * (1 - u - v)  # First triangle coordinate
  x2 <- radii * u            # Second triangle coordinate
  x3 <- radii * v            # Third triangle coordinate
  x4 <- height_values        # Fourth dimension (height)

  # Shift the cluster towards the tip point (e.g., offset)
  corn_cluster <- cbind(x1, x2, x3, x4)
  corn_cluster <- sweep(corn_cluster, 2, tip_point, "+") |>
    as_tibble()

  return(corn_cluster)
}

# Function to gen a filled hexagonal pyramid in 4D space
gen_filled_hexagonal_pyramid_4d <- function(n, height = 5, base_radius = 3, tip_point = c(0, 0, 0, 0)) {
  if (n <= 0) {
    stop("Number of points should be a positive number.")
  }

  # gen height values with more points near the base
  height_values <- runif(n, 0, height)  # Uniformly distributed heights

  # The base radius decreases linearly as the height increases
  radii <- (base_radius * (height - height_values)) / height

  # gen points within a hexagonal base in the x, y plane
  angles <- runif(n, 0, 2 * pi)
  hexagon_angles <- seq(0, 2 * pi, length.out = 7)[1:6]  # 6 angles for hexagon

  # Randomly assign each point to a part of the hexagon
  selected_angles <- sample(hexagon_angles, n, replace = TRUE)

  # gen points inside the hexagon (filling the base)
  radial_factors <- sqrt(runif(n, 0, 1))  # Ensures uniform distribution inside the hexagon
  x1 <- radii * cos(selected_angles) * radial_factors
  x2 <- radii * sin(selected_angles) * radial_factors

  # For the third dimension (z), gen points inside the volume, tapering toward the tip
  x3 <- runif(n, -0.1, 0.1) * (height - height_values) / height  # Tapering with small variance

  # The fourth dimension (w) is the height
  x4 <- height_values

  # Combine the dimensions into a 4D structure
  filled_pyramid_cluster <- cbind(x1, x2, x3, x4)

  # Shift the cluster towards the tip point (optional offset)
  filled_pyramid_cluster <- sweep(filled_pyramid_cluster, 2, tip_point, "+") |>
    as_tibble()

  return(filled_pyramid_cluster)
}
