#' Generate points on a plane in 2D space
#'
#' This function generates points on a plane in 3D space based on the provided coefficients,
#' intercepts, and ranges for the parameters.
#'
#' @param sample_size The number of points to generate.
#' @param coefficient_x_1 The coefficient of the first parameter in the x-dimension equation.
#' @param coefficient_x_2 The coefficient of the second parameter in the x-dimension equation.
#' @param coefficient_y_1 The coefficient of the first parameter in the y-dimension equation.
#' @param coefficient_y_2 The coefficient of the second parameter in the y-dimension equation.
#' @param intercept_x The intercept for the x-dimension equation.
#' @param intercept_y The intercept for the y-dimension equation.
#' @param u_min The minimum value for the first parameter (u) range.
#' @param u_max The maximum value for the first parameter (u) range.
#' @param v_min The minimum value for the second parameter (v) range.
#' @param v_max The maximum value for the second parameter (v) range.
#' @param num_noise_dims The number of noise dimensions to add to the generated points.
#' @param min_noise The minimum value for the noise dimensions.
#' @param max_noise The maximum value for the noise dimensions.
#'
#' @return A matrix containing the generated points on the plane.
#'
#' @examples
#' plane_points <- plane(sample_size = 100, coefficient_x_1 = 1, coefficient_x_2 = 1,
#'                      coefficient_y_1 = -1, coefficient_y_2 = 1, intercept_x = -10,
#'                      intercept_y = 8, u_min = 10, u_max = 30, v_min = 10, v_max = 20,
#'                      num_noise_dims = 2, min_noise = 0, max_noise = 1)
#'
#' @export
plane <- function(sample_size, coefficient_x_1, coefficient_x_2, coefficient_y_1,
                  coefficient_y_2, intercept_x, intercept_y, u_min, u_max, v_min,
                  v_max, num_noise_dims, min_noise, max_noise) {

  u <- stats::runif(sample_size, min = u_min, max = u_max)
  v <- stats::runif(sample_size, min = v_min, max = v_max)
  x <- coefficient_x_1 * u + coefficient_x_2 * v + intercept_x
  y <- coefficient_y_1 * u + coefficient_y_2 * v + intercept_y

  plane_mat <- matrix(c(x, y), ncol = 2)

  if (num_noise_dims != 0) {

    noise_mat <- gen_noise_dims(n = dim(plane_mat)[1], num_noise_dims = num_noise_dims,
                                min_noise = min_noise, max_noise = max_noise)
    plane_mat <- cbind(plane_mat, noise_mat)

    plane_mat

  } else {

    plane_mat

  }

}


three_diff_linear_with_noise <- function(sample_size = 150, with_seed = NULL, num_of_noise_dim = 8,
                                         min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by three
  if ((sample_size%%3) != 0) {
    warning("The sample size should be a product of number of clusters.")
    cluster_size <- floor(sample_size/3)

  } else {
    cluster_size <- sample_size/3
  }


  df_2_split <- snedata::long_cluster_data(n = cluster_size) %>%
    group_by(color) %>%
    group_split()

  df_2_split_1 <- df_2_split[[1]]
  df_2_split_1$x <- df_2_split_1$x - 250
  df_2_split_1$y <- df_2_split_1$y - 20

  df_2_split_3 <- tibble::tibble(x = -df_2_split[[1]]$y, y = df_2_split[[1]]$x)

  df <- dplyr::bind_rows(df_2_split_1, df_2_split[[2]], df_2_split_3) %>%
    select(-color)

  names(df) <- paste0(rep("x",2), 1:2)

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)

  df

}


four_diff_long_clutsers_with_noise <- function(sample_size = 200, with_seed = NULL, num_of_noise_dim = 8,
                                               min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by three
  if ((sample_size%%3) != 0) {
    warning("The sample size should be a product of number of clusters.")
    cluster_size <- floor(sample_size/4)

  } else {
    cluster_size <- sample_size/4
  }


  df_2_split <- snedata::long_cluster_data(n = cluster_size) %>%
    group_by(color) %>%
    group_split()

  df_2_split_1 <- df_2_split[[1]]
  df_2_split_1$x <- df_2_split_1$x - 150
  df_2_split_1$y <- df_2_split_1$y - 20

  df_2_split_3 <- tibble::tibble(x = df_2_split[[1]]$y - 70, y = -df_2_split[[1]]$x)

  df_2_split_4 <- tibble::tibble(x = df_2_split_3$x, y = df_2_split_3$y + 150)

  df <- dplyr::bind_rows(df_2_split_1, df_2_split[[2]], df_2_split_3, df_2_split_4) %>%
    select(-color)

  names(df) <- paste0(rep("x",2), 1:2)

  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df) + 1):((NCOL(df) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)

  df

}

plane_2D_with_hole <- function(sample_size = 100, with_seed = NULL, num_of_noise_dim = 2, min_noise = 0, max_noise = 1) {

  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by four
  if ((sample_size%%4) != 0) {
    stop("The sample size should be a product of 4.")

  } else {
    cluster_size <- sample_size/4
  }

  u <- runif(cluster_size, min = 10, max = 30)
  v <- runif(cluster_size, min = 10, max = 20)
  x <- u + v - 10
  y <- v - u + 8

  df1 <- tibble::tibble(x1 = x, x2 = y)

  anchor <- c(1, 1)
  indices <- rowSums((sweep(df1, 2, anchor, `-`))) > 20
  df1 <- df1[indices, ]
  rownames(df1) <- NULL

  df2 <- tibble::tibble(x1 = -df1$x2 + 26, x2 = df1$x1 - 15)
  df3 <- tibble::tibble(x1 = df1$x2 + 30, x2 = -df1$x1 + 25)

  df <- dplyr::bind_rows(df1 - 10, df1 + 10, df2, df3)

  sample_size <- NROW(df)


  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), 3:(3 + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df, df_noise)

  return(list(df = df, sample_size = sample_size))

}
