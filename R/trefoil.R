#' Generate 4-D Trefoil Knot Coordinates
#'
#' This function generates coordinates for a 4-D trefoil knot.
#' The number of points is determined by the length of the theta and phi sequences.
#'
#' @param n An integer value (default: 500) representing the sample size.
#' @param steps A numeric value (default: 5) representing the number of steps for the theta parameter.
#' @return A tibble containing 4-D trefoil knot.
#' @export
#' @examples
#' set.seed(20240412)
#' trefoil4d <- gen_trefoil4d(n = 500, steps = 5)
gen_trefoil4d <- function(n = 500, steps = 5) {

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  if (steps <= 0) {
    cli::cli_abort("steps should be positive.")
  }

  # Ensure n is at least steps for a meaningful grid
  if (n < steps) {
    cli:: cli_alert_warning("`n` is less than `steps`. Adjusting `n` to `steps` for minimum resolution.")
    n <- steps
  }

  # Calculate the number of steps for phi based on the desired total points
  n_phi_steps <- ceiling(n / steps)

  # Define theta and phi sequences internally
  theta <- seq(0, by = 0.175, length.out = steps) # Keeps original theta density/range
  phi   <- seq(0, 4 * pi, length.out = n_phi_steps)

  # Create a tibble of all combinations of theta and phi
  df <- tidyr::expand_grid(theta = theta, phi = phi) |>
    # Calculate the 4D coordinates
    dplyr::mutate(
      x1 = cos(theta) * cos(phi),
      x2 = cos(theta) * sin(phi),
      x3 = sin(theta) * cos(1.5 * phi),
      x4 = sin(theta) * sin(1.5 * phi)
    ) |>
    # Reorder columns for clarity, putting inputs first
    dplyr::select(x1, x2, x3, x4)

  cli::cli_alert_success("Data generation completed successfully!!!")

  return(df)
}


#' Generate 3-D Trefoil Knot Coordinates (Stereographic Projection)
#'
#' This function generates coordinates for a 3-D trefoil knot
#' by applying a stereographic projection from 4-D space.
#'
#' @param n An integer value (default: 500) representing the sample size.
#' @param steps A numeric value (default: 5) representing the number of steps for the theta parameter.
#' @return A data containing 3-D trefoil knot.
#' @export
#' @examples
#' set.seed(20240412)
#' trefoil3d <- gen_trefoil3d(n = 500, steps = 5)
gen_trefoil3d <- function(n = 500, steps = 5) {

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  if (steps <= 0) {
    cli::cli_abort("steps should be positive.")
  }

  # First, get the 4D coordinates using the tibble-returning function
  df <- gen_trefoil4d(n = n, steps = steps) |>
    # Filter out points where w is 1 to avoid division by zero
    dplyr::filter(x4 != 1) |>
    # Apply the stereographic projection
    dplyr::mutate(
      x1 = x1 / (1 - x4),
      x2 = x2 / (1 - x4),
      x3 = x3 / (1 - x4)
    ) |>
    # Select the 3D coordinates plus original parameters if desired
    dplyr::select(x1, x2, x3)

  cli::cli_alert_success("Data generation completed successfully!!!")

  return(df)
}

utils::globalVariables(c("x1", "x2", "x3", "x4"))
