#' Generate Multivariate Gaussian Cloud
#'
#' This function generates a dataset representing a structure with a Gaussian.
#'
#' @param n An integer value (default: 500) representing the sample size.
#' @param p An integer value (default: 4) representing the number of dimensions.
#' @param m A numeric vector (default: c(0, 0, 0, 0)) representing the mean along each dimensions.
#' @param s A numeric matrix (default:  diag(4) * 0.01) representing the variance of along each dimension.
#' @return A tibble containing a multivariate Gaussian cloud dataset.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' gaussian <- gen_gaussian(n = 500, p = 4, m = rep(0, 4), s = diag(4))
gen_gaussian <- function(n = 500, p = 4, m = rep(0, p), s = diag(p) * 0.01) {

  if (p < 2) {
    cli::cli_abort("p should be greater than 2.")
  }

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  if (!is.vector(m)) {
    cli::cli_abort("m should be a numeric vector.")
  }

  if (!is.matrix(s)) {
    cli::cli_abort("s should be a matrix.")
  }

  if (NROW(s) != NCOL(s)) {
    cli::cli_abort("s should be a square matrix.")
  }

  if ((NROW(s) != p) | (NCOL(s) != p)) {
    cli::cli_abort("Number of rows and columns in s should be {.val {p}}.")
  }

  df <- mvtnorm::rmvnorm(n, mean = m, sigma = s)
  # Create the tibble
  df <- tibble::as_tibble(df, .name_repair = "minimal")
  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Data generation completed successfully!!!")
  return(df)
}
