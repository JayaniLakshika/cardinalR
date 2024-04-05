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

utils::globalVariables(c("n"))
