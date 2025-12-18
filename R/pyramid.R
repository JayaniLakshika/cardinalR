#' Generate Rectangular-based Pyramid
#'
#' This function generates a dataset representing a rectangular based pyramid.
#'
#' @param n An integer value (default: 500) representing the sample size.
#' @param p An integer value (default: 4) representing the number of dimensions.
#' @param h A numeric value (default: 5) representing the height of the pyramid.
#' @param l_vec A numeric vector (default: c(3, 2)) representing the base lengths
#' along the and y of the pyramid.
#' @param rt A numeric value (default: 0.5) representing the tip radius of the pyramid.
#'
#' @return A tibble containing the rectangular-based pyramid.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' pyrrect <- gen_pyrrect(n = 500, p = 4, h = 5, l_vec = c(3, 2), rt = 0.5)
gen_pyrrect <- function(n = 500, p = 4, h = 5, l_vec = c(3, 2), rt = 0.5) {

  if (p < 2) {
    cli::cli_abort("p should be greater than 2.")
  }

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  if (h <= 0) {
    cli::cli_abort("h should be positive.")
  }

  if (any(l_vec <= 0)) {
    cli::cli_abort("Values in the base length vector should be positive.")
  }

  if (rt < 0) {
    cli::cli_abort("rt should be positive.")
  }

  if (any(rt >= l_vec)) {
    cli::cli_abort("The rt should be smaller than the any base length values of the pyramid.")
  }

  base_width_x <- l_vec[1]
  base_width_y <- l_vec[2]

  # gen points with a higher density near the tip
  height_values <- stats::rexp(n, rate = 1 / (h / 2))  # Exponentially distributed heights
  height_values <- pmin(height_values, h)  # Cap heights to the maximum h

  # Base dimensions decrease linearly as h increases
  x_radii <- rt + (base_width_x - rt) * (height_values / h)
  y_radii <- rt + (base_width_y - rt) * (height_values / h)

  coords <- matrix(0, nrow = n, ncol = p)
  coords[, 1] <- stats::runif(n, -x_radii, x_radii)
  coords[, 2] <- stats::runif(n, -y_radii, y_radii)
  coords[, 3] <- stats::runif(n, -x_radii, x_radii)  # For the third dimension, using the same range as x

  # For the fourth dimension and beyond, taper toward the tip
  if (p > 3) {
    for (i in 4:p) {
      coords[, i - 1] <- stats::runif(n, -0.1, 0.1) * (h - height_values) / h # Tapering
    }
  }

  # The last dimension is the h
  coords[, p] <- height_values

  # Create the tibble
  df <- tibble::as_tibble(coords, .name_repair = "minimal")
  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Data generation completed successfully!!!")
  return(df)
}

#' Generate Triangular Based Pyramid
#'
#' This function generates a dataset representing a triangular based pyramid.
#'
#' @param n An integer value (default: 500) representing the sample size.
#' @param p An integer value (default: 4) representing the number of dimensions.
#' @param h A numeric value (default: 5) representing the height of the pyramid.
#' @param l A numeric value (default: 3) representing the base length of the pyramid.
#' @param rt A numeric value (default: 0.5) representing the tip radius of the pyramid.
#'
#' @return A data containing the triangular based pyramid.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' pyrtri <- gen_pyrtri(n = 500, p = 4, h = 5, l = 3, rt = 0.5)
gen_pyrtri <- function(n = 500, p = 4, h = 5, l = 3, rt = 0.5) {

  if (p < 2) {
    cli::cli_abort("p should be greater than 2.")
  }

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  if (h <= 0) {
    cli::cli_abort("h should be positive.")
  }

  if (l <= 0) {
    cli::cli_abort("The base length should be positive.")
  }

  if (rt < 0) {
    cli::cli_abort("rt should be positive.")
  }

  if (rt >= l) {
    cli::cli_abort("The tip radius should be smaller than the base length of the pyramid.")
  }

  # gen points with a higher density near the tip
  height_values <- stats::rexp(n, rate = 1 / (h / 2))  # Exponentially distributed heights
  height_values <- pmin(height_values, h)  # Cap heights to the maximum h

  # Base size decreases linearly as h increases
  radii <- rt + (l - rt) * (height_values / h)

  # gen points within a triangular cross-section at each h level
  # Using barycentric coordinates to gen points inside a triangle
  u <- stats::runif(n)
  v <- stats::runif(n)
  is_outside <- (u + v) > 1
  u[is_outside] <- 1 - u[is_outside]
  v[is_outside] <- 1 - v[is_outside]

  coords <- matrix(0, nrow = n, ncol = p)
  coords[, 1] <- radii * (1 - u - v) # First triangle coordinate (mapped to x1)
  coords[, 2] <- radii * u         # Second triangle coordinate (mapped to x2)
  coords[, 3] <- radii * v         # Third triangle coordinate (mapped to x3)

  # For the fourth dimension and beyond, taper toward the tip
  if (p > 3) {
    for (i in 4:p) {
      coords[, i - 1] <- stats::runif(n, -0.1, 0.1) * (h - height_values) / h # Tapering
    }
  }

  # The last dimension is the h
  coords[, p] <- height_values

  # Create the tibble
  df <- tibble::as_tibble(coords, .name_repair = "minimal")
  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Data generation completed successfully!!!")
  return(df)
}

#' Generate Star Based Pyramid
#'
#' This function generates a dataset representing a star based pyramid.
#'
#' @param n An integer value (default: 500) representing the sample size.
#' @param p An integer value (default: 4) representing the number of dimensions.
#' @param h A numeric value (default: 5) representing the height of the pyramid.
#' @param rb A numeric value (default: 3) representing the base radius of the pyramid.
#'
#' @return A data containing the star based pyramid.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' pyrstar <- gen_pyrstar(n = 500, p = 4, h = 5, rb = 3)
gen_pyrstar <- function(n = 500, p = 4, h = 5, rb = 3) {

  if (p < 2) {
    cli::cli_abort("p should be greater than 2.")
  }

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  if (h <= 0) {
    cli::cli_abort("h should be positive.")
  }

  if (rb <= 0) {
    cli::cli_abort("rb should be positive.")
  }

  # Gen h values with more points near the base
  height_values <- stats::runif(n, 0, h) # Uniformly distributed heights

  # The base radius decreases linearly as the h increases
  radii <- (rb * (h - height_values)) / h

  # Gen points within a hexagonal base in the first two dimensions
  hexagon_angles <- seq(0, 2 * pi, length.out = 7)[1:6] # 6 angles for hexagon

  # Randomly assign each point to a part of the hexagon
  selected_angles <- sample(hexagon_angles, n, replace = TRUE)

  # Gen points inside the hexagon (filling the base)
  radial_factors <- sqrt(stats::runif(n, 0, 1)) # Ensures uniform distribution inside the hexagon
  coords <- matrix(0, nrow = n, ncol = p)
  coords[, 1] <- radii * cos(selected_angles) * radial_factors
  coords[, 2] <- radii * sin(selected_angles) * radial_factors

  # For the third dimension and beyond, taper toward the tip
  if (p >= 3) {
    for (i in 3:p) {
      coords[, i] <- stats::runif(n, -0.1, 0.1) * (h - height_values) / h # Tapering
    }
  }

  # The last dimension is the h
  coords[, p] <- height_values

  # Create the tibble
  df <- tibble::as_tibble(coords, .name_repair = "minimal")
  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Data generation completed successfully!!!")
  return(df)
}

#' Generate p-D Triangular Pyramid With Triangular Pyramid shaped holes
#'
#' This function generates p-D triangular pyramid with triangular pyramid shaped holes.
#'
#' @param n An integer value (default: 500) representing the sample size.
#' @param p An integer value (default: 4) representing the number of dimensions.
#' @return A data containing a triangular pyramid with triangular pyramid shaped holes.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' pyrfrac <- gen_pyrfrac(n = 500, p = 3)
gen_pyrfrac <- function(n = 500, p = 4) {

  if (p < 2) {
    cli::cli_abort("p should be greater than 2.")
  }

  if (length(n) != 1) {
    cli::cli_abort("n should be a single integer specifying the number of points.")
  }

  if (n < 0) {
    cli::cli_abort("n should be positive.")
  }

  trace_point <- stats::runif(p)
  corner_points <- geozoo::simplex(p=p)$points

  data_list <- lapply(1:p, function(i) rep(0, n))
  names(data_list) <- paste0("x", 1:p)

  df <- tibble::tibble(!!!data_list)

  for (i in 1:n) {
    trace_point <- (corner_points[sample((p + 1), 1), ] + trace_point) / 2
    df[i, ] <- as.list(trace_point)
  }

  cli::cli_alert_success("Data generation completed successfully!!!")
  return(df)
}
