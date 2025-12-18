#' Generate Long Linear Data
#'
#' This function generates a dataset consisting of long linear data.
#'
#' @param n An integer value (default: 500) representing the sample size.
#' @param p An integer value (default: 4) representing the number of dimensions.
#' @return A tibble containing the long linear data.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' longlinear <- gen_longlinear(n = 500, p = 4)
gen_longlinear <- function(n = 500, p = 4) {

  if (p < 2) {
    cli::cli_abort("p should be greater than or equal to 2.")
  }

  if (n <= 0){
    cli::cli_abort("n should be positive.")
  }

  df <- tibble::tibble()

  data_list <- lapply(1:p, function(i) rep(0, n))
  names(data_list) <- paste0("x", 1:p)

  df <- tibble::tibble(!!!data_list)

  scale_fac_vec <- stats::runif(p, -10, 10)
  shift_fac_vec <- stats::runif(p, -300, 300)

  for (i in 1:p) {

    df[, i] <- scale_fac_vec[i] * (0:(n - 1) + 0.03 * n * stats::rnorm(n) + shift_fac_vec[i])

  }

  cli::cli_alert_success("Data generation completed successfully!!!")
  return(df)
}
