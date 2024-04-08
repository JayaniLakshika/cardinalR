#' Generate Random Noise Dimensions
#'
#' This function generates random noise dimensions to be added to the coordinates of a sphere.
#'
#' @param n The number of observations for which to generate noise dimensions.
#' @param num_noise_dims The number of noise dimensions to generate.
#' @param min_noise The minimum value for the random noise.
#' @param max_noise The maximum value for the random noise.
#'
#' @return A matrix containing the generated random noise dimensions.
#'
#' @examples
#' # Generate random noise dimensions with 3 dimensions, minimum value -1, and maximum value 1
#' gen_noise_dims(n = 50, num_noise_dims = 3, min_noise = -1, max_noise = 1)
#'
#' @export
gen_noise_dims <- function(n, num_noise_dims, min_noise, max_noise) {

  # Initialize an empty list to store the vectors
  noise_dim_val_list <- list()

  for (j in 1:num_noise_dims) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[j]] <- stats::runif(n, min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[j]] <- (-1) * stats::runif(n, min = min_noise, max = max_noise)
    }


  }

  noise_mat <- matrix(unlist(noise_dim_val_list), ncol = num_noise_dims)

  return(noise_mat)


}

#' Generate Background Noise Data
#'
#' This function generates background noise data with specified parameters such as
#' the number of samples, number of dimensions, mean, and standard deviation.
#'
#' @param n Number of samples to generate.
#' @param num_dims Number of dimensions (columns) of the data.
#' @param mean Mean of the normal distribution used to generate noise (default is 0).
#' @param sd Standard deviation of the normal distribution used to generate noise (default is 1).
#'
#' @return A matrix containing the generated background noise data, with
#' \code{n} rows and \code{num_dims} columns.
#'
#' @examples
#'
#' # Generate background noise with custom mean and standard deviation
#' gen_bkg_noise(n = 100, num_dims = 10, mean = 5, sd = 2)
#'
#' @export
gen_bkg_noise <- function(n, num_dims, mean, sd) {

  # Initialize an empty list to store the vectors
  noise_bkg_val_list <- list()

  for (j in 1:num_dims) {
    noise_bkg_val_list[[j]] <- stats::rnorm(n, mean = mean, sd = sd)
  }

  bkg_mat <- matrix(unlist(noise_bkg_val_list), ncol = length(noise_bkg_val_list))

  return(bkg_mat)


}

utils::globalVariables(c("n"))
