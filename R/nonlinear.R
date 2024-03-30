curvilinear_2D <- function(sample_size = 100, with_seed = NULL, num_of_noise_dim = 2, min_noise = -1, max_noise = 1){
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  x <- runif(sample_size, 0, 2)
  y <- -(x^3 + runif(sample_size, 0, 3)) + runif(sample_size, 0, 0.5)

  df <- tibble::tibble(x1 = x, x2 = y)

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
  df

}


nonlinear_2D <- function(sample_size = 100, with_seed = NULL, num_of_noise_dim = 2,
                         min_noise = -1, max_noise = 1) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  theta = runif(sample_size, 0.2, 0.6 * pi)
  x = cos(theta) + rnorm(sample_size, 10, 0.03)
  y = sin(theta) + rnorm(sample_size, 10, 0.03)

  df <- tibble::tibble(x1 = x, x2 = y)

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
  df

}
