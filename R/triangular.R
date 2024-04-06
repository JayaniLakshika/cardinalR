#' Generate Triangular 3D Datasets with Noise
#'
#' This function generates triangular 3D datasets with added noise dimensions.
#'
#' @param sample_size The total number of samples to generate.
#' @param num_noise_dims The number of additional noise dimensions to add to the data.
#' @param min_noise The minimum value for the noise dimensions.
#' @param max_noise The maximum value for the noise dimensions.
#' @return A matrix containing the triangular 3D datasets with added noise.
#' @export
#'
#' @examples
#' triangular_3d_data <- traingular_3d_with_noise(sample_size = 150, num_noise_dims = 4,
#' min_noise = -0.05, max_noise = 0.05)
traingular_3d_with_noise <- function(sample_size, num_noise_dims,
                                     min_noise, max_noise) {

  trace_point <- stats::runif(3)
  corner_points <- matrix(c(c(0, 1, 0.5, 0.5), c(0, 0, 1, 0.5), c(0, 0, 0, 1)), ncol =3)

  df <- matrix(c(rep(0,sample_size), rep(0,sample_size), rep(0,sample_size)), ncol =3)
  for(i in 1:sample_size){
    trace_point <- (corner_points[sample(4,1),]+trace_point)/2
    df[i,] <- trace_point
  }

  if (num_noise_dims != 0) {

    noise_mat <- gen_noise_dims(n = dim(df)[1], num_noise_dims = num_noise_dims,
                                min_noise = min_noise, max_noise = max_noise)
    df <- cbind(df, noise_mat)

    df

  } else {

    df

  }

}


#' Generate Triangular Plane with Background Noise
#'
#' This function generates a triangular plane dataset with background noise dimensions.
#'
#' @param sample_size The total number of samples to generate.
#' @param num_noise_dims The number of additional noise dimensions to add to the data.
#' @param min_noise The minimum value for the noise dimensions.
#' @param max_noise The maximum value for the noise dimensions.
#' @return A matrix containing the triangular plane dataset with background noise.
#' @export
#'
#' @examples
#' triangular_plane_data <- triangular_plane_with_bkg_noise(sample_size = 675,
#' num_noise_dims = 3, min_noise = -0.05, max_noise = 0.05)
triangular_plane_with_bkg_noise <- function(sample_size, num_noise_dims,
                                            min_noise, max_noise) {

  # To check that the assigned sample_size is divided by three
  if ((sample_size%%3) != 0) {
    warning("The sample size should be a product of three.")
    cluster_size <- floor(sample_size/3)

  } else {
    cluster_size <- sample_size/3
  }


  trace_point <- stats::runif(2)
  corner_points <- matrix(c(c(0, 1, 0.5), c(0, 0, 1)))
  df1 <- matrix(c(rep(0,sample_size), rep(0,sample_size)), ncol =2)

  for(i in 1:cluster_size){
    trace_point <- (corner_points[sample(3,1),]+trace_point)/2
    df1[i,] <- trace_point
  }

  if (num_noise_dims != 0) {

    noise_mat <- gen_noise_dims(n = dim(df1)[1], num_noise_dims = num_noise_dims,
                                min_noise = min_noise, max_noise = max_noise)
    df1 <- cbind(df1, noise_mat)

  }

  ## Generate the bkg noise
  noise_bkg_val_list <- list()

  for (j in 1:NCOL(df1)) {
    noise_bkg_val_list[[j]] <- stats::rnorm(cluster_size, mean = 0.025, sd = 0.5)
  }

  df2 <- matrix(unlist(noise_bkg_val_list), ncol = length(noise_bkg_val_list))
  df <- rbind(df1, df2, -df1)

  df

}
