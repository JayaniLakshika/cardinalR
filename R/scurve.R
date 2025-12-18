#' Generate S-curve Data
#'
#' This function generates S-curve data.
#'
#' @param n An integer value (default: 500) representing the sample size.
#' @return A data containing the generated S-curve data.
#' @references
#' Buitinck, L., Louppe, G., Blondel, M., Pedregosa, F., Mueller, A.,
#' Grisel, O., ... & Varoquaux, G. (2013).
#' API design for machine learning software: experiences from the scikit-learn
#' project.
#' \emph{arXiv preprint} \emph{arXiv:1309.0238}.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' scurve <- gen_scurve(n = 500)
gen_scurve <- function(n = 500) {

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  a <- 3 * pi * stats::runif(n = n, min = -0.5, max = 0.5)
  x1 <- sin(a)
  x2 <- 2.0 * stats::runif(n = n, min = 0, max = 1)
  x3 <- sign(a) * (cos(a) - 1)

  df <- matrix(0, nrow = n, ncol = 3)
  df[, 1] <- x1
  df[, 2] <- x2
  df[, 3] <- x3

  # Create the tibble
  df <- tibble::as_tibble(df, .name_repair = "minimal")
  names(df) <- paste0("x", 1:3)

  cli::cli_alert_success("Data generation completed successfully!!!")
  return(df)
}

#' Generate S-curve Data with a Hole
#'
#' This function generates S-curve data with a hole by filtering out samples that
#' are not close to a specified anchor point.
#'
#' @param n A numeric value (default: 500) representing the sample size.
#' @param r_hole A numeric value (default: 0.5) representing the radius of the hole.
#' @return A data containing the generated S-curve data with a hole.
#' @references
#' Wang, Y., Huang, H., Rudin, C., & Shaposhnik, Y. (2021).
#' Understanding how dimension reduction tools work: an empirical approach to
#' deciphering t-SNE, UMAP, TriMAP, and PaCMAP for data visualization.
#' \emph{J Mach. Learn. Res}, \emph{22}, 1-73.
#' @seealso the \href{https://github.com/YingfanWang/PaCMAP}{PaCMAP homepage}.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' scurvehole <- gen_scurvehole(n = 1000)
gen_scurvehole <- function(n = 500, r_hole = 0.5) {
  if (n <= 0) cli::cli_abort("n should be positive.")

  df <- gen_scurve(n = n)
  df <- gen_hole(df, anchor = c(0, 1, 0), r = r_hole)

  cli::cli_alert_success("Data generation completed successfully!!!")
  return(df)
}

