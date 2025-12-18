#' Generate Quadratic
#'
#' This function generates a dataset representing a structure with a quadratic pattern.
#'
#' @param n A integer value (default: 500) representing the sample size.
#' @param range A numeric vector (default: c(-1, 1)) representing the range along x1 axis.
#' @return A tibble containing a quadratic structure.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' quadratic <- gen_quadratic(n = 500)
gen_quadratic <- function(n = 500, range = c(-1, 1)) {

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  df <- matrix(0, nrow = n, ncol = 2)
  # gen the core curvilinear pattern in 2D
  df[, 1] <- stats::runif(n, range[1], range[2])
  poly_basis <- stats::poly(df[, 1], degree = 2, raw = TRUE)
  df[, 2] <- poly_basis[, 1] - poly_basis[, 2] + stats::runif(n, 0, 0.5)

  df <- tibble::as_tibble(df, .name_repair = "minimal")
  names(df) <- paste0("x", 1:2)

  cli::cli_alert_success("Data generation completed successfully!!!")
  return(df)

}


#' Generate Cubic
#'
#' This function generates a dataset representing a structure with a cubic pattern.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @param range A numeric vector (default: c(-1, 2)) representing the range along x1 axis.
#' @return A data containing a cubic structure.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' cubic <- gen_cubic(n = 500)
gen_cubic <- function(n = 500, range = c(-1, 2)) {

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  df <- matrix(0, nrow = n, ncol = 2)
  # gen the core curvilinear pattern in 2D
  df[, 1] <- stats::runif(n, range[1], range[2])
  poly_basis <- stats::poly(df[, 1], degree = 3, raw = TRUE)
  df[, 2] <- poly_basis[, 1] + poly_basis[, 2] - poly_basis[, 3] + stats::runif(n, 0, 0.5)

  df <- tibble::as_tibble(df, .name_repair = "minimal")
  names(df) <- paste0("x", 1:2)

  cli::cli_alert_success("Data generation completed successfully!!!")
  return(df)

}
