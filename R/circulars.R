three_circulars_with_noise <- function(sample_size = 300, with_seed = NULL, num_of_noise_dim = 8,
                                       min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by three
  if ((sample_size%%3) != 0) {
    warning("The sample size should be a product of four.")
    cluster_size <- floor(sample_size/3)

  } else {
    cluster_size <- sample_size/3
  }

  theta = runif(cluster_size, 0.0,2 * pi)
  x = cos(theta) + rnorm(cluster_size, 10, 0.03)
  y = sin(theta) + rnorm(cluster_size, 10, 0.03)

  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df1 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x = 0.5 * cos(theta) + rnorm(cluster_size, 10, 0.03)
  y = 0.5 * sin(theta) + rnorm(cluster_size, 10, 0.03)

  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df2 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x = rnorm(cluster_size, 10, 0.03)
  y = rnorm(cluster_size, 10, 0.03)

  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df3 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  df <- bind_rows(df1, df2, df3)
  names(df) <- paste0(rep("x", NCOL(df)), 1:NCOL(df))

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

cell_cycle_with_noise <- function(sample_size = 300, with_seed = NULL, num_of_noise_dim = 8,
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


  r1 <- 2
  r2 <- 1

  theta = runif(cluster_size, 0, 2 * pi)
  x <- rep(0, cluster_size)
  y <- r1 * cos(theta)
  z <- r2 * sin(theta)

  df1 <- tibble::tibble(x1=x, x2=y, x3=z)

  x <- r2 * cos(theta)
  y <- rep(0, cluster_size)
  z <- r1 * sin(theta)

  df2 <- tibble::tibble(x1=x, x2=y, x3=z)

  x <- r1 * cos(theta)
  y <- r2 * sin(theta)
  z <- rep(0, cluster_size)

  df3 <- tibble::tibble(x1=x, x2=y, x3=z)

  df <- bind_rows(df1, df2, df3)

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

curvy_cell_cycle_with_noise <- function(sample_size = 300, with_seed = NULL, num_of_noise_dim = 8,
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


  r = sqrt(3)/3

  theta = runif(cluster_size, 0, 2 * pi)
  x <- cos(theta)
  y <- r + sin(theta)
  z <- cos(3 * theta)/3

  df1 <- tibble::tibble(x1=x, x2=y, x3=z)

  x <- cos(theta) + 0.5
  y <- sin(theta) - r/2
  z <- cos(3 * theta)/3

  df2 <- tibble::tibble(x1=x, x2=y, x3=z)

  x <- cos(theta) - 0.5
  y <- sin(theta) - r/2
  z <- cos(3 * theta)/3

  df3 <- tibble::tibble(x1=x, x2=y, x3=z)

  df <- bind_rows(df1, df2, df3)

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
