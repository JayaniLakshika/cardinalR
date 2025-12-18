#' Generate Cube with grid points
#'
#' This function generates a grid dataset with specified grid points along each axes.
#'
#' @param n An integer vector (default: 500) representing the sample size.
#' @param p An integer value (default: 4) representing the number of dimensions.
#' @return A tibble containing the cube with grid points.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' gridcube <- gen_gridcube(n = 500, p = 4)
gen_gridcube <- function(n = 500, p = 4) {

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  df <- geozoo::cube.solid.grid(p = p)$points |>
    tibble::as_tibble(.name_repair = "minimal")

  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Data generation completed successfully!!!")
  return(df)

}

#' Generate Cube with uniform points
#'
#' This function generates a grid dataset with specified uniform points along each axes.
#'
#' @param n A numeric vector (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @return A data containing the cube with uniform points.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' unifcube <- gen_unifcube(n = 500, p = 4)
gen_unifcube <- function(n = 500, p = 4) {

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  df <- geozoo::cube.solid.random(n = n, p = p)$points |>
    tibble::as_tibble(.name_repair = "minimal")

  df <- df[-(1:(2^p)),] ## To remove vertices

  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Data generation completed successfully!!!")
  return(df)


}

#' Generate Cube with Hole
#'
#' This function generates a dataset representing a cube with a hole.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @param r_hole A numeric value (default: 0.5) representing the radius of the hole.
#'
#' @return A data containing the cube data with a hole.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' cubehole <- gen_unifcubehole(n = 1000, p = 4)
gen_unifcubehole <- function(n = 5000, p = 4, r_hole = 0.5) {
  if (n <= 0) cli::cli_abort("n should be positive.")
  if (p <= 0) cli::cli_abort("p should be positive.")

  # generate cube from geozoo
  df <- gen_unifcube(n = n, p = p)

  # apply hole generation
  df <- gen_hole(df, r = r_hole)

  cli::cli_alert_success("Data generation completed successfully!!!")
  return(df)
}
