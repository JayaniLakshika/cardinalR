# Function to gen a curvilinear cluster in 4D space with an offset
gen_curv_4d <- function(n, offset) {
  if (n <= 0) {
    stop("Number of points should be a positive number.")
  }

  # gen the core curvilinear pattern in 2D
  x1 <- stats::runif(n, 0, 2)
  #x2 <- -(x1^3 + stats::runif(n, 0, 3)) + stats::runif(n, 0, 0.5)
  x2 <- -(x1^2) + stats::runif(n, 0, 0.5)


  # Define additional dimensions for 4D
  x3 <- -sin(x1 * pi) + runif(n, -0.5, 0.5)  # A sine-based curve
  x4 <- cos(x1 * pi) + runif(n, -0.5, 0.5)   # A cosine-based curve

  curvilinear_mat <- cbind(x1, x2, x3, x4)

  if (!missing(offset)) {
    # Apply the offset to shift the cluster
    curvilinear_df <- sweep(curvilinear_mat, 2, offset, "+") |>
      as_tibble()
  } else{

    curvilinear_df <- curvilinear_mat |>
      as_tibble()

  }
  curvilinear_df
}

gen_curv_flip1_4d <- function(n, offset) {
  if (n <= 0) {
    stop("Number of points should be a positive number.")
  }

  # gen the core curvilinear pattern in 2D
  x3 <- stats::runif(n, 0, 2)
  x1 <- -sin(x3 * pi) + runif(n, -0.5, 0.5)  # A sine-based curve
  x2 <- cos(x3 * pi) + runif(n, -0.5, 0.5)   # A cosine-based curve
  x4 <- -(x3^3 + stats::runif(n, 0, 3)) + stats::runif(n, 0, 0.5)

  curvilinear_mat <- cbind(x1, x2, x3, x4)

  # Apply the offset to shift the cluster
  curvilinear_df <- sweep(curvilinear_mat, 2, offset, "+") |>
    as_tibble()

  curvilinear_df
}

gen_curv_flip2_4d <- function(n, offset) {
  if (n <= 0) {
    stop("Number of points should be a positive number.")
  }

  # gen the core curvilinear pattern in 2D
  x2 <- stats::runif(n, 0, 2)
  x3 <- -sin(x2 * pi) + runif(n, -0.5, 0.5)  # A sine-based curve
  x4 <- cos(x2 * pi) + runif(n, -0.5, 0.5)   # A cosine-based curve
  x1 <- -(x2^3 + stats::runif(n, 0, 3)) + stats::runif(n, 0, 0.5)

  curvilinear_mat <- cbind(x1, x2, x3, x4)

  # Apply the offset to shift the cluster
  curvilinear_df <- sweep(curvilinear_mat, 2, offset, "+") |>
    as_tibble()

  curvilinear_df
}

# Function to gen a noise-free 4D crescent
gen_crescent_4d <- function(n, offset) {
  # Step 1: gen angles for a semi-circle
  #theta <- seq(pi / 6, 8 * pi / 6, length.out = n)  # evenly spaced angles for crescent
  theta <- seq(pi / 6, 12 * pi / 6, length.out = n)  # evenly spaced angles for crescent

  # Step 2: gen points in 2D crescent shape
  x1 <- cos(theta)  # x-coordinate on the crescent (cosine function)
  x2 <- sin(theta)  # y-coordinate on the crescent (sine function)

  # Step 3: Add additional 2 dimensions for 4D
  # x3 could represent an increasing function of theta
  x3 <- theta + rnorm(n, 0, 0.5)  # Simply map theta to the third dimension

  # x4 could be a linear transformation of theta
  x4 <- 2 * theta + rnorm(n, 0, 0.5)  # Linear function for the fourth dimension

  # Combine into a 4D dataset
  crescent_4d <- tibble(x1 = x1, x2 = x2, x3 = x3, x4 = x4)

  # Apply the offset to shift the cluster
  crescent_4d <- sweep(crescent_4d, 2, offset, "+") |>
    as_tibble()

  return(crescent_4d)
}

# Function to gen an S-curve in 4D
gen_s_curve_4d <- function(n, offset) {
  # Step 1: gen the theta angle to represent the S-shape
  #theta <- seq(-3 * pi / 2, 3 * pi / 2, length.out = n)  # evenly spaced angles
  theta <- seq(-2 * pi / 2, 2 * pi / 2, length.out = n)  # evenly spaced angles


  # Step 2: gen the 3D S-curve
  x1 <- sin(theta)  # x1 based on the sine of theta (S-shape in x)
  x2 <- runif(n, 0, 1)  # x2 is some uniform thickness along the curve
  x3 <- sign(theta) * (cos(theta) - 1)  # x3 for the S-curve depth (cosine curve)

  # Step 3: Add the 4th dimension
  # x4 can be a linear function of theta or something else
  x4 <- theta  # Simply map theta to the fourth dimension

  # Combine into a 4D dataset
  s_curve_4d <- cbind(x1, x2, x3, x4)

  # Apply the offset to shift the cluster
  s_curve_4d <- sweep(s_curve_4d, 2, offset, "+") |>
    as_tibble()


  return(s_curve_4d)
}

# Function to gen a curvy cylinder in 4D
gen_curvy_cylinder_4d <- function(n, radius = 1, height = 10, curve_strength = 1, offset) {

  # Step 1: gen cylindrical coordinates in 2D (x1, x2)
  theta <- runif(n, 0, 3 * pi)  # Random angle for the circular base
  x1 <- radius * cos(theta)            # x1 coordinate (circular)
  x2 <- radius * sin(theta)            # x2 coordinate (circular)

  # Step 2: gen the curvy components in 3rd and 4th dimensions
  x3 <- runif(n, 0, height)     # Height along the cylinder
  x4 <- curve_strength * sin(x3)       # Curvy pattern in the 4th dimension

  # Combine the coordinates into a 4D dataset
  curvy_cylinder_4d <- cbind(x1, x2, x3, x4)

  # Apply the offset to shift the cluster
  curvy_cylinder_4d <- sweep(curvy_cylinder_4d, 2, offset, "+") |>
    as_tibble()

  return(curvy_cylinder_4d)
}

# Function to gen a curvilinear cluster in 4D space with an offset
gen_curv2_4d <- function(n, offset) {
  if (n <= 0) {
    stop("Number of points should be a positive number.")
  }

  # gen the core curvilinear pattern in 2D
  x1 <- stats::runif(n, 0, 2)
  x2 <- -(x1^2 + stats::runif(n, 0, 1)) + stats::runif(n, 0, 0.5)

  # Define additional dimensions for 4D
  x3 <- -sin(x1 * pi) + runif(n, -0.5, 0.5)  # A sine-based curve
  x4 <- cos(x1 * pi) + runif(n, -0.5, 0.5)   # A cosine-based curve

  curvilinear_mat <- cbind(x1, x2, x3, x4)

  # Apply the offset to shift the cluster
  curvilinear_mat <- sweep(curvilinear_mat, 2, offset, "+") |>
    as_tibble()

  curvilinear_mat
}

# Function to gen a non-linear rectangular hyperbola in 4D
gen_nonlinear_hyperbola_4d <- function(n, C = 1, nonlinear_factor = 0.5, offset) {

  # gen random points for x1 and x3 in a range avoiding zero
  x1 <- runif(n, 0.1, 2)  # Avoid zero to prevent division by zero
  x3 <- runif(n, 0.1, 2)

  # Define additional dimensions for 4D
  x2 <- -sin(x1 * pi) + runif(n, -0.1, 0.1)  # A sine-based curve
  x4 <- cos(x1 * pi) + runif(n, -0.1, 0.1)   # A cosine-based curve

  # Combine the coordinates into a 4D dataset
  nonlinear_hyperbola_4d <- cbind(x1, x2, x3, x4)

  # Apply the offset to shift the cluster
  nonlinear_hyperbola_4d <- sweep(nonlinear_hyperbola_4d, 2, offset, "+") |>
    as_tibble()

  return(nonlinear_hyperbola_4d)
}

# Function to gen a non-linear rectangular hyperbola in 4D
gen_nonlinear_hyperbola2_4d <- function(n, C = 1, nonlinear_factor = 0.5, offset) {

  # gen random points for x1 and x3 in a range avoiding zero
  x1 <- runif(n, 0.1, 2)  # Avoid zero to prevent division by zero
  x3 <- runif(n, 0.1, 0.8)

  # # Apply non-linear distortions for the second dimension
  x2 <- (C / x1) + nonlinear_factor * sin(x1)  # Hyperbola + sine curve distortion
  #
  # # Apply non-linear distortions for the fourth dimension
  # x4 <- (C / x3) + nonlinear_factor * cos(x3)  # Hyperbola + cosine curve distortion

  # Define additional dimensions for 4D
  #x2 <- -sin(x1 * pi) + runif(n, -0.1, 0.1)  # A sine-based curve
  x4 <- cos(x1 * pi) + runif(n, -0.1, 0.1)   # A cosine-based curve

  # Combine the coordinates into a 4D dataset
  nonlinear_hyperbola_4d <- cbind(x1, x2, x3, x4)

  # Apply the offset to shift the cluster
  nonlinear_hyperbola_4d <- sweep(nonlinear_hyperbola_4d, 2, offset, "+") |>
    as_tibble()

  return(nonlinear_hyperbola_4d)
}

# Function to gen a conic spiral in 4D
gen_conic_spiral_4d <- function(n, spiral_turns = 1, cone_height = 2, cone_radius = 1, offset) {

  # gen theta values to represent the angle of the spiral in the xy-plane
  theta <- seq(0, 2 * pi * spiral_turns, length.out = n)

  # Spiral in the first two dimensions (x1, x2) - Archimedean spiral
  r <- cone_radius * theta  # Radius increases linearly with theta
  x1 <- r * cos(theta)
  x2 <- r * sin(theta)

  # Conical shape in the third dimension (x3) - linear increase with height
  x3 <- cone_height * theta / max(theta) + runif(n, -0.1, 0.6) # Scaling height to range from 0 to cone_height

  # Spiral in the fourth dimension (x4) - a helical shape based on the cone
  x4 <- cone_radius * sin(2 * theta) + runif(n, -0.1, 0.6) # Helical movement

  # Combine the coordinates into a 4D dataset
  conic_spiral_4d <- cbind(x1, x2, x3, x4)

  # Apply the offset to shift the cluster
  conic_spiral_4d <- sweep(conic_spiral_4d, 2, offset, "+") |>
    as_tibble()

  return(conic_spiral_4d)
}

# Function to gen a Helical Hyper-spiral in 4D
gen_helical_hyper_spiral_4d <- function(n, a = 0.05, b = 0.1, k = 1, spiral_radius = 0.5, scale_factor = 0.1, offset) {
  if (n <= 0) {
    stop("Number of points should be a positive integer.")
  }

  # gen angles for the spiral (theta)
  theta <- seq(0, 5/4 * pi, length.out = n)

  # Helical spiral coordinates
  x1 <- spiral_radius * cos(theta)  # x1 is a circular pattern
  x2 <- spiral_radius * sin(theta)  # x2 is a circular pattern
  x3 <- a * theta + runif(n, -0.5, 0.5) # x3 moves linearly with theta (like a helix)
  x4 <- b * sin(k * theta)          # x4 oscillates with sin(k * theta)


  # Combine the coordinates into a 4D dataset
  helical_hyper_spiral_4d <- cbind(x1, x2, x3, x4)

  # Apply the offset to shift the cluster
  helical_hyper_spiral_4d <- sweep(helical_hyper_spiral_4d, 2, offset, "+") |>
    as_tibble()
}

# Function to gen a 4D Spherical Spiral
gen_spherical_spiral_4d <- function(n, radius = 1, spiral_turns = 1, offset) {
  if (n <= 0) {
    stop("Number of points should be a positive integer.")
  }

  # gen angles (theta, phi) for the spherical coordinates
  theta <- seq(0, spiral_turns * 2 * pi, length.out = n)  # Controls the number of spiral turns
  phi <- seq(0, pi, length.out = n)                       # Controls movement along the latitude

  # Spherical to Cartesian coordinates for 4D
  x1 <- radius * sin(phi) * cos(theta)
  x2 <- radius * sin(phi) * sin(theta)
  x3 <- radius * cos(phi) + runif(n, -0.5, 0.5)
  x4 <- theta / max(theta) * radius  # Spiral along the 4th dimension

  # Combine the coordinates into a 4D dataset
  spherical_spiral_4d <- cbind(x1, x2, x3, x4)

  # Apply the offset to shift the cluster
  spherical_spiral_4d <- sweep(spherical_spiral_4d, 2, offset, "+") |>
    as_tibble()
}


