s_curve <- function(n_samples = 100) {
  tt <- 3 * pi * stats::runif(n = n_samples, min = -0.5, max = 0.5)
  x <- sin(tt)
  y <- 2.0 * stats::runif(n = n_samples)
  z <- sign(tt) * (cos(tt) - 1)
  X <- cbind(x, y, z)

  data.frame(X, stringsAsFactors = FALSE)
}


two_s_curves_with_noise <- function(sample_size = 200, with_seed = NULL, num_of_noise_dim = 8,
                                    min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by three
  if ((sample_size%%3) != 0) {
    warning("The sample size should be a product of number of clusters.")
    cluster_size <- floor(sample_size/2)

  } else {
    cluster_size <- sample_size/2
  }


  df1 <- snedata::s_curve(n_samples = cluster_size)
  df1 <- df1 %>%
    select(-color)
  names(df1) <- paste0(rep("x",3), 1:3)

  df2 <- tibble::tibble(x1 = -df1$x1 + 5, x2 = df1$x2 + 1, x3 = df1$x3 + 1)

  df <- dplyr::bind_rows(df1, df2)

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


mirror_s_curves_with_noise <- function(sample_size = 200, with_seed = NULL, num_of_noise_dim = 8,
                                       min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  # To check that the assigned sample_size is divided by three
  if ((sample_size%%3) != 0) {
    warning("The sample size should be a product of number of clusters.")
    cluster_size <- floor(sample_size/2)

  } else {
    cluster_size <- sample_size/2
  }


  df1 <- snedata::s_curve(n_samples = cluster_size)
  df1 <- df1 %>%
    select(-color)
  names(df1) <- paste0(rep("x",3), 1:3)

  df2 <- tibble::tibble(x1 = -df1$x1 + 2, x2 = df1$x2, x3 = df1$x3)

  df <- dplyr::bind_rows(df1, df2)

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
