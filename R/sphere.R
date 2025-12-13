#' Generate Circle in p-d
#'
#' This function generates a dataset representing a structure with a circle.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @return A data containing a circle.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' circle <- gen_circle(n = 500, p = 4)
gen_circle <- function(n = 500, p = 4){

  if (p < 3) {
    cli::cli_abort("p should be greater than 3.")
  }

  if (length(n) != 1) {
    cli::cli_abort("n should be a single integer specifying the number of points.")
  }

  if (n < 0) {
    cli::cli_abort("n should be positive.")
  }

  theta <- stats::runif(n, 0.0, 2 * pi)
  coords <- matrix(0, nrow = n, ncol = p)
  coords[, 1] <- cos(theta)
  coords[, 2] <- sin(theta)

  # Introduce scaling factors for subsequent dimensions
  scaling_factors <- sqrt(cumprod(c(1, rep(0.5, p - 2)))) # Example: decreasing scale

  if (p > 2) {
    # Apply remaining dimensions with sinusoidal patterns
    for (i in 3:p) {
      # Introduce a phase shift for each dimension to make them distinct
      phase_shift <- (i - 2) * (pi / (2 * p))
      coords[, i] <- scaling_factors[i-1] * sin(theta + phase_shift)
    }
  }

  df <- suppressMessages(tibble::as_tibble(coords, .name_repair = "unique"))
  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Data generation completed successfully!!!")
  return(df)
}

#' Generate Curvy Cell Cycle in p-d
#'
#' This function generates a dataset representing a structure with a curvy cell cycle.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @return A data containing a curvy cell cycle.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' curvycycle <- gen_curvycycle(n = 500, p = 4)
gen_curvycycle <- function(n = 500, p = 4){

  if (p < 3) {
    cli::cli_abort("p should be greater than 3.")
  }

  if (length(n) != 1) {
    cli::cli_abort("n should be a single integer specifying the number of points.")
  }

  if (n < 0) {
    cli::cli_abort("n should be positive.")
  }

  theta <- stats::runif(n, 0.0, 2 * pi)
  coords <- matrix(0, nrow = n, ncol = p)
  coords[, 1] <- cos(theta)
  coords[, 2] <- sqrt(3) / 3 + sin(theta)
  coords[, 3] <- 1/3 * cos(3 * theta)

  # Introduce scaling factors for subsequent dimensions
  scaling_factors <- sqrt(cumprod(c(1, rep(0.5, p - 3)))) # Example: decreasing scale

  if (p > 3) {
    # Apply remaining dimensions with sinusoidal patterns
    for (i in 4:p) {
      # Introduce a phase shift for each dimension to make them distinct
      phase_shift <- (i - 2) * (pi / (2 * p))
      coords[, i] <- scaling_factors[i-2] * sin(theta + phase_shift)
    }
  }

  df <- suppressMessages(tibble::as_tibble(coords, .name_repair = "unique"))
  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Data generation completed successfully!!!")
  return(df)
}

#' Generate Uniform Sphere
#'
#' This function generates a dataset representing a structure with a uniform sphere.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @param r A numeric vector (default: 1) representing the radius of the sphere.
#' @return A data containing a uniform sphere.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' unifsphere <- gen_unifsphere(n = 500)
gen_unifsphere <- function(n = 500, r = 1){

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  if (r <= 0) {
    cli::cli_abort("r should be positive.")
  }

  u <- stats::runif(n, -1, 1)                 # cos(phi)
  theta <- stats::runif(n, 0, 2 * pi)        # azimuth

  x1 <- r * sqrt(1 - u^2) * cos(theta)
  x2 <- r * sqrt(1 - u^2) * sin(theta)
  x3 <- r * u

  df <- matrix(c(x1, x2, x3), ncol = 3)

  df <- tibble::as_tibble(df, .name_repair = "minimal")
  names(df) <- paste0("x", 1:3)

  cli::cli_alert_success("Data generation completed successfully!!!")
  return(df)

}

#' Generate Hollow Sphere
#'
#' This function generates a dataset representing a structure with a sphere with points on the surface.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @return A data containing a hollow sphere.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' hollowsphere <- gen_hollowsphere(n = 500)
gen_hollowsphere <- function(n = 500, p = 4){

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }
  if (p <= 0) {
    cli::cli_abort("p should be positive.")
  }

  df <- geozoo::sphere.hollow(n = n, p = p)$points |>
    tibble::as_tibble(.name_repair = "minimal")

  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Data generation completed successfully!!!")
  return(df)

}

#' Generate Grided Sphere
#'
#' This function generates a dataset representing a structure with a grided sphere.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @return A data containing a grided sphere.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' gridedsphere <- gen_gridedsphere(n = 500, p = 4)
gen_gridedsphere <- function(n = 500, p = 4){

  if (p < 3) {
    cli::cli_abort("p should be greater than 3.")
  }

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  n_vec <- gen_nproduct(n = n, p = (p-1))

  # define angle ranges
  angle_ranges <- lapply(seq_len(p - 1), function(j) {
    if (j == (p - 1)) {
      seq(0, 2 * pi, length.out = n_vec[j])
    } else {
      seq(0, pi, length.out = n_vec[j])
    }
  })

  # full grid
  coords <- expand.grid(angle_ranges)

  # spherical -> Cartesian
  df <- matrix(NA, nrow = nrow(coords), ncol = p)

  for (i in seq_len(p)) {
    val <- rep(1, nrow(coords))
    if (i > 1) {
      for (j in seq_len(i - 1)) {
        val <- val * sin(coords[[j]])
      }
    }
    if (i < p) {
      val <- val * cos(coords[[i]])
    }
    df[, i] <- val
  }

  colnames(df) <- paste0("x", seq_len(p))

  cli::cli_alert_success("Data generation completed successfully!!!")
  return(df)

}

#' Generate Small Spheres Within a Big Sphere
#'
#' This function generates a dataset representing a structure with a small and big spheres.
#'
#' @param n_vec A numeric vector (default: c(1000, 100)) representing the sample sizes of the big and small spheres respectively.
#' @param k_small A numeric value (default: 3) representing the number of small spheres.
#' @param r_vec A numeric vector (default: c(15, 3)) representing the radius of the big and small spheres respectively.
#' @param sep A numeric value (default: 10 / sqrt(3) representing how far the small spheres are placed from each other.
#' @return A data containing small spheres within a big sphere.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' clusteredspheres <- gen_clusteredspheres(n_vec = c(1000, 100), k_small = 3,
#' r_vec = c(15, 3), sep = 10 / sqrt(3))
gen_clusteredspheres <- function(n_vec = c(1000, 100), k_small = 3, r_vec = c(15, 3),
                                 sep = 10 / sqrt(3)) {

  if (length(n_vec) != 2) {
    cli::cli_abort("n should contain exactly two values.")
  }

  if (any(n_vec < 0)) {
    cli::cli_abort("Values in n should be positive.")
  }

  if (any(r_vec < 0)) {
    cli::cli_abort("Values in r should be positive.")
  }

  n_big <- n_vec[1]
  n_small <- n_vec[2]

  r_big <- r_vec[1]
  r_small <- r_vec[2]

  d_dim_sphere <- gen_unifsphere(n_small, r_small)
  small_spheres <- lapply(seq_len(k_small), function(i) {
    center <- stats::rnorm(3, sd = sep)
    sweep(d_dim_sphere, 2, center, "+")
  })

  big_sphere <- gen_unifsphere(n_big, r_big)

  small_labeled <- lapply(seq_along(small_spheres), function(i) {
    cbind(small_spheres[[i]], cluster = paste0("small_", i))
  })

  big_labeled <- cbind(big_sphere, cluster = "big")

  df <- dplyr::bind_rows(c(small_labeled, list(big_labeled))) |>
    tibble::as_tibble()
  names(df) <- append(paste0("x", 1:3), "cluster")

  ## Swap rows
  df <- randomize_rows(df)

  cli::cli_alert_success("Data generation completed successfully!!!")
  return(df)
}


#' Generate Hemisphere
#'
#' This function generates a dataset representing a structure with a hemisphere.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @return A data containing a hemisphere.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' hemisphere <- gen_hemisphere(n = 500)
gen_hemisphere <- function(n = 500) {

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  # Step 1: Random angles for spherical coordinates
  theta1 <- stats::runif(n, 0, pi)        # Angle for (x1, x2) plane (azimuth)
  theta2 <- stats::runif(n, 0, pi)        # Angle for (x2, x3) plane (elevation)
  theta3 <- stats::runif(n, 0, pi / 2)    # Angle for (x3, x4), restricted for hemisphere

  # Step 2: Convert spherical coordinates to Cartesian coordinates in 4D
  x1 <- sin(theta1) * cos(theta2)  # x1 coordinate
  x2 <- sin(theta1) * sin(theta2)  # x2 coordinate
  x3 <- cos(theta1) * cos(theta3)  # x3 coordinate
  x4 <- cos(theta1) * sin(theta3)  # x4 coordinate (restricted to hemisphere)

  df <- matrix(c(x1, x2, x3, x4), ncol = 4)

  cli::cli_alert_success("Data generation completed successfully!!!")
  return(df)

}
