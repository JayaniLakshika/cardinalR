curvy_tree_with_noise <- function(sample_size = 300, with_seed = NULL, num_of_noise_dim = 8,
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


  x <- runif(cluster_size, -2, 2)
  y <- -(x^3 + runif(cluster_size, 0, 6)) + runif(cluster_size, 0, 0.2)

  z <- rnorm(cluster_size, 10, 0.1)
  w <- rnorm(cluster_size, 10, 0.1)

  df1 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x <- runif(cluster_size, 0, 2)
  y <- (x^3 + runif(cluster_size, 0, 6)) + runif(cluster_size, 0, 0.2)

  z <- rnorm(cluster_size, 10, 0.1)
  w <- rnorm(cluster_size, 10, 0.1)

  df2 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x <- runif(cluster_size, -2, 0)
  y <- -(x^3 + runif(cluster_size, 0, 6)) + runif(cluster_size, 0, 0.2) + 10

  z <- rnorm(cluster_size, 10, 0.1)
  w <- rnorm(cluster_size, 10, 0.1)

  df3 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

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


tree_with_noise <- function(sample_size = 300, with_seed = NULL, num_of_noise_dim = 8,
                            min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by three
  if ((sample_size%%5) != 0) {
    warning("The sample size should be a product of number of clusters.")
    cluster_size <- floor(sample_size/5)

  } else {
    cluster_size <- sample_size/5
  }


  x <- runif(cluster_size, -3, 3)
  y <- abs(0.5 * x)

  z <- rnorm(cluster_size, 10, 0.03)
  w <- rnorm(cluster_size, 10, 0.03)

  df1 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x <- runif(cluster_size, -0.5, 0.5)
  y <- abs(10*x)

  z <- rnorm(cluster_size, 10, 0.03)
  w <- rnorm(cluster_size, 10, 0.03)

  df2 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x <- runif(cluster_size, -6, 3)
  y <- (-1) * abs(0.5 * x + 5)

  z <- rnorm(cluster_size, 10, 0.03)
  w <- rnorm(cluster_size, 10, 0.03)

  df3 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x <- runif(cluster_size, -0.5, 0.5)
  y <- (-1) * abs(10 * x) - 5

  z <- rnorm(cluster_size, 10, 0.03)
  w <- rnorm(cluster_size, 10, 0.03)

  df4 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x <- runif(cluster_size, -5, 5)
  y <- x

  z <- rnorm(cluster_size, 10, 0.03)
  w <- rnorm(cluster_size, 10, 0.03)

  df5 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  df <- bind_rows(df1, df2, df3, df4, df5)

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

seven_branching_data_with_noise <- function(sample_size = 210, with_seed = NULL, num_of_noise_dim = 8,
                                            min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by three
  if ((sample_size%%7) != 0) {
    warning("The sample size should be a product of number of clusters.")
    cluster_size <- floor(sample_size/7)

  } else {
    cluster_size <- sample_size/7
  }



  x <- runif(cluster_size, -2, 2)
  y <- -(x^3 + runif(cluster_size, 0, 1)) + runif(cluster_size, 0, 0.2)

  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df1 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x <- runif(cluster_size, -2, 1.5)
  y <- (x^3 + runif(cluster_size, 0, 1)) + runif(cluster_size, 0, 0.2)

  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df2 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x <- runif(cluster_size, -2, 1.5)
  y <- (1 + (x-3)^2 + runif(cluster_size, 0, 1)) + runif(cluster_size, 0, 0.1)

  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df3 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x <- runif(cluster_size, -0.5, 3)
  y <- (1 + -(x-3)^2 + runif(cluster_size, 0, 1)) + runif(cluster_size, 0, 0.1)

  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df4 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x <- runif(cluster_size, -1, 1)
  y <- (20 + x^3 + runif(cluster_size, 0, 0.1)) + runif(cluster_size, 0, 0.01)

  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df5 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x <- runif(cluster_size, -2, 2)
  y <- (x^2 + runif(cluster_size, 0, 0.1)) + runif(cluster_size, 0, 0.01) + 10

  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df6 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x <- runif(cluster_size, -2, 2)
  y <- (x^2 + runif(cluster_size, 0, 0.2)) + runif(cluster_size, 0, 0.01) + 15

  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df7 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  df <- bind_rows(df1, df2, df3, df4, df5, df6, df7)

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


four_branching_data_with_noise <- function(sample_size = 400, with_seed = NULL, num_of_noise_dim = 8,
                                           min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by three
  if (((sample_size - sample_size * 0.1)%%4) != 0) {
    warning("The sample size should be a product of number of clusters.")
    cluster_size <- floor((sample_size - sample_size * 0.1)/4)

  } else {
    cluster_size <- (sample_size - sample_size * 0.1)/4
  }




  x <- runif(cluster_size, -5, 1)
  y <- (exp(x) + runif(cluster_size, 0, 0.1)) + runif(cluster_size, 0, 0.2)

  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df1 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x <- runif(cluster_size, -1, 5)
  y <- (exp(-x) + runif(cluster_size, 0, 0.1)) + runif(cluster_size, 0, 0.2)

  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df2 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x <- runif(cluster_size, 0, 5)
  y <- (log(x) + runif(cluster_size, 0, 0.1)) + runif(cluster_size, 0, 0.2)

  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df3 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x <- runif(cluster_size, -5, 0)
  y <- (log(-x) + runif(cluster_size, 0, 0.1)) + runif(cluster_size, 0, 0.2)

  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df4 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x <- runif(sample_size * 0.1, -5, 0)
  y <- runif(sample_size * 0.1, 0, 0.8) + runif(sample_size * 0.1, 0, 0.8)

  z <- rep(0, sample_size * 0.1) + rnorm(sample_size * 0.1, 10, 0.03)
  w <- rep(0, sample_size * 0.1) - rnorm(sample_size * 0.1, 10, 0.03)

  df5 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  df <- bind_rows(df1, df2, df3, df4, df5)

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

eight_branching_data_with_noise <- function(sample_size = 400, with_seed = NULL, num_of_noise_dim = 8,
                                            min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by three
  if ((sample_size%%8) != 0) {
    warning("The sample size should be a product of number of clusters.")
    cluster_size <- floor(sample_size/8)

  } else {
    cluster_size <- sample_size/8
  }




  x <- runif(cluster_size, -1, 2)
  y <- (exp(x) + runif(cluster_size, 0, 0.1)) + runif(cluster_size)

  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df1 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x <- runif(cluster_size, -1, 1)
  y <- (exp(2*x) + runif(cluster_size, 0, 0.1)) + runif(cluster_size, 0, 0.2)

  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df2 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x <- runif(cluster_size, -1, 0.6)
  y <- (exp(3*x) + runif(cluster_size, 0, 0.1)) + runif(cluster_size, 0, 0.2)

  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df3 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x <- runif(cluster_size, -1, 3)
  y <- (exp(0.5*x) + runif(cluster_size, 0, 0.1)) + runif(cluster_size, 0, 0.2)

  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df4 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x <- runif(cluster_size, -2, 1)
  y <- (exp(-x) + runif(cluster_size, 0, 0.1)) + runif(cluster_size, 0, 0.2)

  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df5 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x <- runif(cluster_size, -1, 1)
  y <- (exp(2*-x) + runif(cluster_size, 0, 0.1)) + runif(cluster_size, 0, 0.2)

  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df6 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x <- runif(cluster_size, -0.6, 1)
  y <- (exp(3*-x) + runif(cluster_size, 0, 0.1)) + runif(cluster_size, 0, 0.2)

  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df7 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  x <- runif(cluster_size, -3, 1)
  y <- (exp(0.5*-x) + runif(cluster_size, 0, 0.1)) + runif(cluster_size, 0, 0.2)

  z <- rep(0, cluster_size) + rnorm(cluster_size, 10, 0.03)
  w <- rep(0, cluster_size) - rnorm(cluster_size, 10, 0.03)

  df8 <- tibble::tibble(x1 = x, x2 = y, x3 = z, x4 = w)

  df <- bind_rows(df1, df2, df3, df4, df5, df6, df7, df8)

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
