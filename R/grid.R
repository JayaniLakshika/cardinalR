#need to write function for grid
three_grid_with_noise <- function(n_value = 19, with_seed = NULL, num_of_noise_dim = 8,
                                  min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  df1 <- snedata::grid_data(n = n_value)
  df1 <- df1 %>%
    select(-color)

  names(df1) <- paste0(rep("x",2), 1:2)
  df1$x3 <- runif(nrow(df1), -0.01, 0.01)
  df1$x4 <- runif(nrow(df1), -0.01, 0.01)

  df2 <- snedata::grid_data(n = n_value)
  df2 <- df2 %>%
    select(-color)

  names(df2) <- paste0(rep("x",2), c(1, 3))
  df2$x2 <- runif(nrow(df2), -0.01, 0.01)
  df2$x4 <- runif(nrow(df2), -0.01, 0.01)
  df2 <- df2 %>%
    select(x1, x2, x3, x4)

  df3 <- snedata::grid_data(n = n_value)
  df3 <- df3 %>%
    select(-color)

  names(df3) <- paste0(rep("x",2), c(1, 4))
  df3$x2 <- runif(nrow(df3), -0.01, 0.01)
  df3$x3 <- runif(nrow(df3), -0.01, 0.01)
  df3 <- df3 %>%
    select(x1, x2, x3, x4)


  df <- bind_rows(df1, df2, df3)

  sample_size <- NROW(df)


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

  return(list(df = df, sample_size = sample_size))

}

one_grid_diff_with_bkg_noise <- function(sample_size = 260, with_seed = NULL, num_of_noise_dim = 5,
                                         min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }


  if (((sample_size - (sample_size * 6/26)) %% 2) != 0) {

    stop("The sample size should be a product of two.")

  } else {

    if (((sqrt((sample_size - (sample_size * 6/26)) / 2)) %% 1) != 0) {

      stop("The square root should exists.")

    } else {

      n_value <- sqrt((sample_size - (sample_size * 0.6/2.6)) / 2)

    }

  }



  df1 <- snedata::grid_data(n = n_value)
  df1 <- df1 |>
    dplyr::select(-color)

  names(df1) <- paste0(rep("x",2), 1:2)

  df3 <- df1 + 3

  df1 <- dplyr::bind_rows(df1, df3)


  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df1) + 1):((NCOL(df1) + 1) + num_of_noise_dim))

  #sample_size <- NROW(df1) + NROW(df1) * 0.6/2

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(NROW(df1),
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(NROW(df1),
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df1 <- dplyr::bind_cols(df1, df_noise)

  ## To add background noise
  column_names_bkg <- paste0(rep("x", NCOL(df1)), 1:NCOL(df1))

  noise_bkg_val_list <- list()

  for (j in 1:NCOL(df1)) {
    noise_bkg_val_list[[column_names_bkg[j]]] <- rnorm(sample_size * 0.6/2.6, mean = 3, sd = 5)


  }

  df2 <- tibble::as_tibble(noise_bkg_val_list)


  df <- dplyr::bind_rows(df1, df2)

  df

}

two_grid_with_bkg_noise <- function(n_value = 10, with_seed = NULL, num_of_noise_dim = 8,
                                    min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }



  df1 <- snedata::grid_data(n = n_value)
  df1 <- df1 %>%
    select(-color)

  names(df1) <- paste0(rep("x",2), 1:2)

  df3 <- df1 + 5

  df1 <- dplyr::bind_rows(df1, df3)


  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df1) + 1):((NCOL(df1) + 1) + num_of_noise_dim))

  sample_size <- NROW(df1) + NROW(df1) * 0.6/2

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(NROW(df1),
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(NROW(df1),
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df1 <- dplyr::bind_cols(df1, df_noise)

  ## To add background noise
  column_names_bkg <- paste0(rep("x", NCOL(df1)), 1:NCOL(df1))

  noise_bkg_val_list <- list()

  for (j in 1:NCOL(df1)) {
    noise_bkg_val_list[[column_names_bkg[j]]] <- rnorm(sample_size * 0.6/2.6, mean = 3, sd = 5)


  }

  df2 <- tibble::as_tibble(noise_bkg_val_list)


  df <- dplyr::bind_rows(df1, df2)

  return(list(df = df, sample_size = sample_size))

}

one_grid_diff <- function(sample_size = 200, with_seed = NULL, num_of_noise_dim = 2,
                          min_noise = -0.05, max_noise = 0.05) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  if ((sample_size %% 2) != 0) {

    stop("The sample size should be a product of two.")

  } else {

    if (((sqrt(sample_size/2)) %% 1) != 0) {

      stop("The square root should exists.")

    } else {

      n_value <- sqrt(sample_size/2)

    }

  }



  df1 <- snedata::grid_data(n = n_value)
  df1 <- df1 |>
    dplyr::select(-color)

  names(df1) <- paste0(rep("x",2), 1:2)

  df3 <- df1 + 3

  df1 <- dplyr::bind_rows(df1, df3)


  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df1) + 1):((NCOL(df1) + 1) + num_of_noise_dim))

  sample_size <- NROW(df1)

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(NROW(df1),
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(NROW(df1),
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df <- dplyr::bind_cols(df1, df_noise)



  return(list(df = df, sample_size = NROW(df)))

}
