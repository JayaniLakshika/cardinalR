#' Generate a 3-D Mobius
#'
#' This function generates a dataset representing a structure with a mobius.
#'
#' @param n An integer value (default: 500) representing the sample size.
#' @return A tibble containing a mobius structure.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' mobius <- gen_mobius(n = 500)
gen_mobius <- function(n = 500) {

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  df <- geozoo::mobius(n = n, p = 3)$points |>
    tibble::as_tibble(.name_repair = "minimal")

  names(df) <- paste0("x", 1:3)

  cli::cli_alert_success("Data generation completed successfully!!!")
  return(df)

}
