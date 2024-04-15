#' Generate Grid Data with Noise
#'
#' This function generates a grid dataset with specified grid points along the x
#' and y axes, and optionally adds noise dimensions.
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
#' set.seed(20240412)
#' one_grid <- one_grid(nx = 10, ny = 10, num_noise = 2, min_n = -0.05, max_n = 0.05)
one_grid <- function(nx, ny, num_noise, min_n, max_n) {
  if (nx <= 0) {
    stop("The number of grid points along the x axis should be a positive number.")
  }

  if (ny <= 0) {
    stop("The number of grid points along the y axis should be a positive number.")
  }

  if (num_noise < 0) {
    stop("Number of noise dimensions should be a positive number.")
  }

  if (missing(nx)) {
    stop("Missing nx.")
  }

  if (missing(ny)) {
    stop("Missing nx.")
  }

  if (missing(num_noise)) {
    stop("Missing num_noise.")
  }

  df <- expand.grid(1:nx, 1:ny)
  df_mat <- matrix(c(df$Var1, df$Var2), ncol = 2)

  if (num_noise != 0) {
    if (missing(min_n)) {
      stop("Missing min_n.")
    }

    if (missing(max_n)) {
      stop("Missing max_n.")
    }

    noise_mat <- gen_noise_dims(
      n = dim(df_mat)[1], num_noise = num_noise,
      min_n = min_n, max_n = max_n
    )
    df_mat <- cbind(df_mat, noise_mat)

    df_mat
  } else {
    df_mat
  }
}

#' Generate Two Grids with Noise
#'
#' This function generates two grid datasets with noise dimensions.
#'
#' @param n_value The number of grid points along the x and y axes for each grid.
#' @param num_noise The number of additional noise dimensions to add to the data.
#' @param min_n The minimum value for the noise dimensions.
#' @param max_n The maximum value for the noise dimensions.
#' @return A list containing two grid datasets with added noise and the sample
#'  size of each dataset.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' two_grids <- two_grid(n_value = 19, num_noise = 2, min_n = -0.05, max_n = 0.05)
two_grid <- function(n_value, num_noise, min_n, max_n) {
  if (n_value <= 0) {
    stop("The number of grid points along the x and y axes should be a positive number.")
  }

  if (num_noise < 0) {
    stop("Number of noise dimensions should be a positive number.")
  }

  if (missing(n_value)) {
    stop("Missing n_value.")
  }

  if (missing(num_noise)) {
    stop("Missing num_noise.")
  }

  df1 <- one_grid(nx = n_value, ny = n_value, num_noise = 0)
  df1 <- cbind(df1, stats::runif(NROW(df1), -0.01, 0.01), stats::runif(NROW(df1), -0.01, 0.01))

  df2 <- one_grid(nx = n_value, ny = n_value, num_noise = 0)
  df2 <- cbind(df2, stats::runif(NROW(df2), -0.01, 0.01), stats::runif(NROW(df2), -0.01, 0.01))
  df2 <- df2[, c(1, 3, 2, 4)]

  df <- rbind(df1, df2)

  if (num_noise != 0) {
    if (missing(min_n)) {
      stop("Missing min_n.")
    }

    if (missing(max_n)) {
      stop("Missing max_n.")
    }

    noise_mat <- gen_noise_dims(
      n = dim(df)[1], num_noise = num_noise,
      min_n = min_n, max_n = max_n
    )
    df <- cbind(df, noise_mat)
  }

  return(list(df = df, n = NROW(df)))
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
#' set.seed(20240412)
#' three_grids <- three_grid(
#'   n_value = 19, num_noise = 2, min_n = -0.05,
#'   max_n = 0.05
#' )
three_grid <- function(n_value, num_noise, min_n, max_n) {
  if (n_value <= 0) {
    stop("The number of grid points along the x and y axes should be a positive number.")
  }

  if (num_noise < 0) {
    stop("Number of noise dimensions should be a positive number.")
  }

  if (missing(n_value)) {
    stop("Missing n_value.")
  }

  if (missing(num_noise)) {
    stop("Missing num_noise.")
  }

  df1 <- one_grid(nx = n_value, ny = n_value, num_noise = 0)
  df1 <- cbind(df1, stats::runif(NROW(df1), -0.01, 0.01), stats::runif(NROW(df1), -0.01, 0.01))

  df2 <- one_grid(nx = n_value, ny = n_value, num_noise = 0)
  df2 <- cbind(df2, stats::runif(NROW(df2), -0.01, 0.01), stats::runif(NROW(df2), -0.01, 0.01))
  df2 <- df2[, c(1, 3, 2, 4)]

  df3 <- one_grid(nx = n_value, ny = n_value, num_noise = 0)
  df3 <- cbind(df3, stats::runif(NROW(df3), -0.01, 0.01), stats::runif(NROW(df3), -0.01, 0.01))
  df3 <- df3[, c(1, 3, 4, 2)]

  df <- rbind(df1, df2, df3)

  if (num_noise != 0) {
    if (missing(min_n)) {
      stop("Missing min_n.")
    }

    if (missing(max_n)) {
      stop("Missing max_n.")
    }

    noise_mat <- gen_noise_dims(
      n = dim(df)[1], num_noise = num_noise,
      min_n = min_n, max_n = max_n
    )
    df <- cbind(df, noise_mat)
  }

  return(list(df = df, n = NROW(df)))
}

#' Generate One Grid with Different Values and Background Noise
#'
#' This function generates a grid dataset with different values and background noise.
#'
#' @param n_value The number of grid points along each axis for the grids.
#' @param num_noise The number of additional noise dimensions to add to the data.
#' @param min_n The minimum value for the noise dimensions.
#' @param max_n The maximum value for the noise dimensions.
#' @return A list containing the one grid datasets with background noise and the sample size.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' one_grid_bkg <- one_grid_bkg(
#'   n_value = 10, num_noise = 2, min_n = -0.05,
#'   max_n = 0.05
#' )
one_grid_bkg <- function(n_value, num_noise, min_n, max_n) {
  if (n_value <= 0) {
    stop("The number of grid points along the x and y axes should be a positive number.")
  }

  if (num_noise < 0) {
    stop("Number of noise dimensions should be a positive number.")
  }

  if (missing(n_value)) {
    stop("Missing n_value.")
  }

  if (missing(num_noise)) {
    stop("Missing num_noise.")
  }

  df1 <- one_grid(nx = n_value, ny = n_value, num_noise = 0)

  if (num_noise != 0) {
    if (missing(min_n)) {
      stop("Missing min_n.")
    }

    if (missing(max_n)) {
      stop("Missing max_n.")
    }

    noise_mat <- gen_noise_dims(
      n = dim(df1)[1], num_noise = num_noise,
      min_n = min_n, max_n = max_n
    )
    df1 <- cbind(df1, noise_mat)
  }

  df2 <- gen_bkg_noise(n = NROW(df1) * 0.5, num_dims = NCOL(df1), mean = 2, sd = 3)
  df <- rbind(df1, df2)

  return(list(df = df, n = NROW(df)))
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
#' set.seed(20240412)
#' two_grid_comb_bkg <- two_grid_comb_bkg(
#'   n_value = 10, num_noise = 2,
#'   min_n = -0.05, max_n = 0.05
#' )
two_grid_comb_bkg <- function(n_value, num_noise, min_n, max_n) {
  if (n_value <= 0) {
    stop("The number of grid points along the x and y axes should be a positive number.")
  }

  if (num_noise < 0) {
    stop("Number of noise dimensions should be a positive number.")
  }

  if (missing(n_value)) {
    stop("Missing n_value.")
  }

  if (missing(num_noise)) {
    stop("Missing num_noise.")
  }

  df1 <- one_grid(nx = n_value, ny = n_value, num_noise = 0)
  df3 <- df1 + 5
  df1 <- rbind(df1, df3)

  n <- NROW(df1) + NROW(df1) * 0.6 / 2

  if (num_noise != 0) {
    if (missing(min_n)) {
      stop("Missing min_n.")
    }

    if (missing(max_n)) {
      stop("Missing max_n.")
    }

    noise_mat <- gen_noise_dims(
      n = dim(df1)[1], num_noise = num_noise,
      min_n = min_n, max_n = max_n
    )
    df1 <- cbind(df1, noise_mat)
  }

  df2 <- gen_bkg_noise(n = n * 0.6 / 2.6, num_dims = NCOL(df1), mean = 3, sd = 5)
  df <- rbind(df1, df2)

  return(list(df = df, n = NROW(df)))
}

#' Generate One Grid with Different Offset
#'
#' This function generates a single grid dataset with a different offset.
#'
#' @param n_value The number of grid points along each axis for the grids.
#' @param num_noise The number of additional noise dimensions to add to the data.
#' @param min_n The minimum value for the noise dimensions.
#' @param max_n The maximum value for the noise dimensions.
#' @return A list containing the grid dataset with different offsets and the sample size.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' two_grid_comb <- two_grid_comb(
#'   n_value = 10, num_noise = 2, min_n = -0.05,
#'   max_n = 0.05
#' )
two_grid_comb <- function(n_value, num_noise, min_n, max_n) {
  if (n_value <= 0) {
    stop("The number of grid points along the x and y axes should be a positive number.")
  }

  if (num_noise < 0) {
    stop("Number of noise dimensions should be a positive number.")
  }

  if (missing(n_value)) {
    stop("Missing n_value.")
  }

  if (missing(num_noise)) {
    stop("Missing num_noise.")
  }


  df1 <- one_grid(nx = n_value, ny = n_value, num_noise = 0)
  df2 <- df1 + 3
  df <- rbind(df1, df2)

  if (num_noise != 0) {
    if (missing(min_n)) {
      stop("Missing min_n.")
    }

    if (missing(max_n)) {
      stop("Missing max_n.")
    }

    noise_mat <- gen_noise_dims(
      n = dim(df)[1], num_noise = num_noise,
      min_n = min_n, max_n = max_n
    )
    df <- cbind(df, noise_mat)
  }

  return(list(df = df, n = NROW(df)))
}
