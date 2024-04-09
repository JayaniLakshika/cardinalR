#' Generate Grid Data with Noise
#'
#' This function generates a grid dataset with specified grid points along the x and y axes, and optionally adds noise dimensions.
#'
#' @param nx The number of grid points along the x axis.
#' @param ny The number of grid points along the y axis.
#' @param num_noise The number of additional noise dimensions to add to the data.
#' @param min_n The minimum value for the noise dimensions.
#' @param max_n The maximum value for the noise dimensions.
#' @return A matrix containing the grid data with added noise.
#' @export
#'
#' @examples
#' grid_data <- grid_data(nx = 10, ny = 10, num_noise = 2,
#' min_n = -0.05, max_n = 0.05)
grid_data <- function(nx, ny, num_noise, min_n, max_n) {
  df <- expand.grid(1:nx, 1:ny)
  df_mat <- matrix(c(df$Var1, df$Var2), ncol = 2)

  if (num_noise != 0) {

    noise_mat <- gen_noise_dims(n = dim(df_mat)[1], num_noise = num_noise,
                                min_n = min_n, max_n = max_n)
    df_mat <- cbind(df_mat, noise_mat)

    df_mat

  } else {

    df_mat

  }
}

#' Generate Three Grids with Noise
#'
#' This function generates three grid datasets with noise dimensions.
#'
#' @param n_value The number of grid points along the x and y axes for each grid.
#' @param num_noise The number of additional noise dimensions to add to the data.
#' @param min_n The minimum value for the noise dimensions.
#' @param max_n The maximum value for the noise dimensions.
#' @return A list containing three grid datasets with added noise and the sample
#'  size of each dataset.
#' @export
#'
#' @examples
#' three_grids <- three_grid_with_noise(n_value = 19, num_noise = 4,
#' min_n = -0.05, max_n = 0.05)
three_grid_with_noise <- function(n_value, num_noise, min_n, max_n) {

  df1 <- grid_data(nx = n_value, ny = n_value, num_noise = 0)
  df1 <- cbind(df1, stats::runif(NROW(df1), -0.01, 0.01), stats::runif(NROW(df1), -0.01, 0.01))

  df2 <- grid_data(nx = n_value, ny = n_value, num_noise = 0)
  df2 <- cbind(df2, stats::runif(NROW(df2), -0.01, 0.01), stats::runif(NROW(df2), -0.01, 0.01))
  df2 <- df2[, c(1, 3, 2, 4)]

  df3 <- grid_data(nx = n_value, ny = n_value, num_noise = 0)
  df3 <- cbind(df3, stats::runif(NROW(df3), -0.01, 0.01), stats::runif(NROW(df3), -0.01, 0.01))
  df3 <- df3[, c(1, 3, 4, 2)]

  df <- rbind(df1, df2, df3)

  if (num_noise != 0) {

    noise_mat <- gen_noise_dims(n = dim(df)[1], num_noise = num_noise,
                                min_n = min_n, max_n = max_n)
    df <- cbind(df, noise_mat)

  }

  return(list(df = df, n = NROW(df)))

}

#' Generate One Grid with Different Values and Background Noise
#'
#' This function generates a grid dataset with different values and background noise.
#'
#' @param n The total number of samples, including the background noise.
#' @param num_noise The number of additional noise dimensions to add to the data.
#' @param min_n The minimum value for the noise dimensions.
#' @param max_n The maximum value for the noise dimensions.
#' @return A list containing the grid dataset with different values and background noise.
#' @export
#'
#' @examples
#' one_grid_diff_with_bkg_noise <- one_grid_diff_with_bkg_noise(n = 260,
#' num_noise = 5, min_n = -0.05, max_n = 0.05)
one_grid_diff_with_bkg_noise <- function(n = 260, num_noise = 5,
                                         min_n = -0.05, max_n = 0.05) {


  if (((n - (n * 6/26)) %% 2) != 0) {

    stop("The sample size should be a product of two.")

  } else {

    if (((sqrt((n - (n * 6/26)) / 2)) %% 1) != 0) {

      stop("The square root should exists.")

    } else {

      n_value <- sqrt((n - (n * 0.6/2.6)) / 2)

    }

  }

  df1 <- grid_data(nx = n_value, ny = n_value, num_noise = 0)
  df2 <- df1 + 3
  df1 <- rbind(df1, df2)


  if (num_noise != 0) {

    noise_mat <- gen_noise_dims(n = dim(df1)[1], num_noise = num_noise,
                                min_n = min_n, max_n = max_n)
    df1 <- cbind(df1, noise_mat)

  }

  df2 <- gen_bkg_noise(n = n * 0.6/2.6, num_dims = NCOL(df1), mean = 3, sd = 5)
  df <- rbind(df1, df2)
  df

}

#' Generate Two Grids with Background Noise
#'
#' This function generates two grid datasets with background noise.
#'
#' @param n_value The number of grid points along each axis for the grids.
#' @param num_noise The number of additional noise dimensions to add to the data.
#' @param min_n The minimum value for the noise dimensions.
#' @param max_n The maximum value for the noise dimensions.
#' @return A list containing the two grid datasets with background noise and the sample size.
#' @export
#'
#' @examples
#' two_grid_with_bkg_noise <- two_grid_with_bkg_noise(n_value = 10, num_noise = 4,
#'                                                   min_n = -0.05, max_n = 0.05)
two_grid_with_bkg_noise <- function(n_value, num_noise, min_n, max_n) {

  df1 <- grid_data(nx = n_value, ny = n_value, num_noise = 0)
  df3 <- df1 + 5
  df1 <- rbind(df1, df3)

  n <- NROW(df1) + NROW(df1) * 0.6/2

  if (num_noise != 0) {

    noise_mat <- gen_noise_dims(n = dim(df1)[1], num_noise = num_noise,
                                min_n = min_n, max_n = max_n)
    df1 <- cbind(df1, noise_mat)

  }

  df2 <- gen_bkg_noise(n = n * 0.6/2.6, num_dims = NCOL(df1), mean = 3, sd = 5)
  df <- rbind(df1, df2)

  return(list(df = df, n = n))

}

#' Generate One Grid with Different Offset
#'
#' This function generates a single grid dataset with a different offset.
#'
#' @param n The total number of samples in the dataset.
#' @param num_noise The number of additional noise dimensions to add to the data.
#' @param min_n The minimum value for the noise dimensions.
#' @param max_n The maximum value for the noise dimensions.
#' @return A list containing the grid dataset with different offsets and the sample size.
#' @export
#'
#' @examples
#' one_grid_diff <- one_grid_diff(n = 200, num_noise = 2,
#' min_n = -0.05, max_n = 0.05)
one_grid_diff <- function(n, num_noise, min_n, max_n) {

  if ((n %% 2) != 0) {

    stop("The sample size should be a product of two.")

  } else {

    if (((sqrt(n/2)) %% 1) != 0) {

      stop("The square root should exists.")

    } else {

      n_value <- sqrt(n/2)

    }

  }

  df1 <- grid_data(nx = n_value, ny = n_value, num_noise = 0)
  df2 <- df1 + 3
  df <- rbind(df1, df2)

  if (num_noise != 0) {

    noise_mat <- gen_noise_dims(n = dim(df)[1], num_noise = num_noise,
                                min_n = min_n, max_n = max_n)
    df <- cbind(df, noise_mat)

  }

  return(list(df = df, n = NROW(df)))

}

#' Generate Three Grid Data with Noise
#'
#' This function generates three grid data with noise.
#'
#' @param n_value The number of grid points along each dimension.
#' @param num_noise The number of additional noise dimensions to be generated.
#' @param min_n The minimum value for the noise added to the data points.
#' @param max_n The maximum value for the noise added to the data points.
#'
#' @return A list containing the generated data matrix (`df`) and the total sample size.
#' @export
#'
#' @examples
#'
#' # Generate grid data with noise with custom parameters
#' data <- three_grid_with_noise(n_value = 19, num_noise = 4,
#'  min_n = -0.05, max_n = 0.05)
three_grid_with_noise <- function(n_value = 19, num_noise = 4,
                                  min_n = -0.05, max_n = 0.05) {

  df1 <- grid_data(nx = n_value, ny = n_value, num_noise = 0)
  df1 <- cbind(df1, stats::runif(NROW(df1), -0.01, 0.01),
               stats::runif(NROW(df1), -0.01, 0.01))

  df2 <- grid_data(nx = n_value, ny = n_value, num_noise = 0)
  df2 <- cbind(df2, stats::runif(NROW(df2), -0.01, 0.01),
               stats::runif(NROW(df2), -0.01, 0.01))
  df2 <- df2[, c(1, 3, 2, 4)]

  df3 <- grid_data(nx = n_value, ny = n_value, num_noise = 0)
  df3 <- cbind(df3, stats::runif(NROW(df3), -0.01, 0.01),
               stats::runif(NROW(df3), -0.01, 0.01))
  df3 <- df3[, c(1, 3, 4, 2)]

  df <- rbind(df1, df2, df3)

  if (num_noise != 0) {

    noise_mat <- gen_noise_dims(n = dim(df)[1], num_noise = num_noise,
                                min_n = min_n, max_n = max_n)
    df <- cbind(df, noise_mat)

    df

  } else {

    df

  }

  return(list(df = df, n = NROW(df)))

}
