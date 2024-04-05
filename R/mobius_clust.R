mobius_cluster_with_noise <- function(sample_size = 200, with_seed = NULL, num_of_noise_dim = 8,
                                      min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  mobius <- geozoo::mobius.experiment(p = 5, n = sample_size* 0.80)

  df1 <- tibble::as_tibble(mobius$points)

  names(df1) <- paste0(rep("x", length(names(df1))), 1: length(names(df1)))


  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df1) + 1):((NCOL(df1) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(sample_size* 0.80,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(sample_size* 0.80,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df1 <- dplyr::bind_cols(df1, df_noise)

  ## To add background noise
  column_names_bkg <- paste0(rep("x", NCOL(df1)), 1:NCOL(df1))

  noise_bkg_val_list <- list()

  for (j in 1:NCOL(df1)) {
    noise_bkg_val_list[[column_names_bkg[j]]] <- rnorm(sample_size * 0.20, mean = 0, sd = 0.3)


  }

  df2 <- tibble::as_tibble(noise_bkg_val_list)


  df <- dplyr::bind_rows(df1, df2)


  df

}
