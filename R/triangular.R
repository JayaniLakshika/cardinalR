traingular_3D_with_noise <- function(sample_size = 150, with_seed = NULL, num_of_noise_dim = 8,
                                     min_noise = -0.5, max_noise = 0.5) {
  # To check the seed is not assigned
  if (!is.null(with_seed)) {
    set.seed(with_seed)
  }

  trace.point <- runif(3)
  corner.points <- tibble::tibble(x1 =c(  0,  1, 0.5, 0.5),
                                  x2 =c(  0,  0,   1, 0.5),
                                  x3 =c(  0,  0,   0,   1))
  df <- tibble::tibble(x1 =rep(0,sample_size),
                       x2 =rep(0,sample_size),
                       x3 =rep(0,sample_size))
  for(i in 1:sample_size){
    trace.point    = (corner.points[sample(4,1),]+trace.point)/2
    df[i,] = trace.point
  }


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


triangular_plane_with_bkg_noise <- function(sample_size = 675, with_seed = NULL, num_of_noise_dim = 8,
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


  trace.point <- runif(2)
  corner.points <- tibble::tibble(x1 =c(  0,  1, 0.5),
                                  x2 =c(  0,  0,   1))
  df1 <- tibble::tibble(x1 =rep(0,cluster_size),
                        x2 =rep(0,cluster_size))
  for(i in 1:cluster_size){
    trace.point    = (corner.points[sample(3,1),]+trace.point)/2
    df1[i,] = trace.point
  }


  # To generate column names for noise dimensions
  column_names <- paste0(rep("x", num_of_noise_dim), (NCOL(df1) + 1):((NCOL(df1) + 1) + num_of_noise_dim))

  # Initialize an empty list to store the vectors with column
  # values
  noise_dim_val_list <- list()

  for (j in 1:num_of_noise_dim) {
    if ((j%%2) == 0) {
      noise_dim_val_list[[column_names[j]]] <- runif(cluster_size,
                                                     min = min_noise, max = max_noise)
    } else {
      noise_dim_val_list[[column_names[j]]] <- (-1) * runif(cluster_size,
                                                            min = min_noise, max = max_noise)
    }


  }

  df_noise <- tibble::as_tibble(noise_dim_val_list)
  df1 <- dplyr::bind_cols(df1, df_noise)

  ## To add background noise
  column_names_bkg <- paste0(rep("x", NCOL(df1)), 1:NCOL(df1))

  noise_bkg_val_list <- list()

  for (j in 1:NCOL(df1)) {
    noise_bkg_val_list[[column_names_bkg[j]]] <- rnorm(cluster_size, mean = 0.025, sd = 0.5)


  }

  df2 <- tibble::as_tibble(noise_bkg_val_list)


  df <- dplyr::bind_rows(df1, df2, -df1)

  df

}
