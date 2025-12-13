#' Generate Blunted Cone
#'
#' This function generates a dataset representing a cone with the option of a sharp or blunted apex.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @param h A numeric value (default: 5) representing the height of the cone.
#' @param ratio A numeric value (default: 0.5) representing the radius tip to radius base ratio of the cone. Should be less than 1.
#'
#' @return A data containing the cone with the option of a sharp or blunted apex.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' cone <- gen_cone(n = 500, p = 4, h = 5, ratio = 0.5)
gen_cone <- function(n = 500, p = 4, h = 5, ratio = 0.5) {

  if (p < 3) {
    cli::cli_abort("p should be greater than 3.")
  }

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  if (h <= 0) {
    cli::cli_abort("h should be positive.")
  }

  if (ratio <= 0 || ratio >= 0.8) {
    cli::cli_abort("ratio should be between 0.1 and 0.8 (tip/base).")
  }
  #ratio = rt/rb

  # Gen points with a higher density near the tip (along the last dimension - 'h')
  height_values <- stats::rexp(n, rate = 1 / (h / 2)) # Exponentially distributed heights
  height_values <- pmin(height_values, h)       # Cap heights to the maximum h

  # Generalized "radius" decreases linearly from the base to the tip
  radii <- ratio + (1 - ratio) * (height_values / h)

  # Generate generalized "angles" for the (p-1)-dimensional hypersphere
  angles <- matrix(stats::runif(n * (p - 2), 0, 2 * pi), nrow = n)
  phi <- stats::runif(n, 0, pi) # One angle with range 0 to pi

  coords <- matrix(0, nrow = n, ncol = p)
  coords[, p] <- height_values # The last dimension is our 'h'

  coords[, 1] <- radii * cos(angles[, 1]) * sin(phi)
  coords[, 2] <- radii * sin(angles[, 1]) * sin(phi)
  coords[, 3] <- radii * cos(phi)

  if(p > 3) {

    for (i in 4:p-1) {
      product_of_sines <- 1
      for (j in 1:(i - 2)) {
        product_of_sines <- product_of_sines * sin(angles[, j])
      }
      coords[, i - 1] <- radii * product_of_sines * cos(ifelse(i == p, phi, angles[, i - 2]))
      if (i < p) {
        coords[, i] <- radii * product_of_sines * sin(angles[, i - 2])
      }
    }
    coords[, p] <- height_values
  }

  # Create the tibble
  df <- tibble::as_tibble(coords, .name_repair = "minimal")
  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Data generation completed successfully!!!")
  return(df)
}
