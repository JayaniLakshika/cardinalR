## nx = grid points along the x axis
## ny = grid points along the y axis
grid_data <- function(nx, ny, num_noise_dims, min_noise, max_noise) {
  df <- expand.grid(1:nx, 1:ny)

  if (num_noise_dims != 0) {

    noise_mat <- gen_noise_dims(n = dim(df)[1], num_noise_dims = num_noise_dims,
                                min_noise = min_noise, max_noise = max_noise)
    df <- cbind(df, noise_mat)

    df

  } else {

    df

  }
}

three_grid_with_noise <- function(n_value = 19, num_noise_dims = 4,
                                  min_noise = -0.05, max_noise = 0.05) {

  df1 <- grid_data(nx = n_value, ny = n_value, num_noise_dims = 0)

  names(df1) <- paste0(rep("x",2), 1:2)
  df1$x3 <- runif(nrow(df1), -0.01, 0.01)
  df1$x4 <- runif(nrow(df1), -0.01, 0.01)

  df2 <- grid_data(nx = n_value, ny = n_value, num_noise_dims = 0)

  names(df2) <- paste0(rep("x",2), c(1, 3))
  df2$x2 <- runif(nrow(df2), -0.01, 0.01)
  df2$x4 <- runif(nrow(df2), -0.01, 0.01)
  df2 <- df2 |>
    select(x1, x2, x3, x4)

  df3 <- grid_data(nx = n_value, ny = n_value, num_noise_dims = 0)

  names(df3) <- paste0(rep("x",2), c(1, 4))
  df3$x2 <- runif(nrow(df3), -0.01, 0.01)
  df3$x3 <- runif(nrow(df3), -0.01, 0.01)
  df3 <- df3 |>
    select(x1, x2, x3, x4)

  df <- rbind(df1, df2, df3)

  sample_size <- NROW(df)

  if (num_noise_dims != 0) {

    noise_mat <- gen_noise_dims(n = dim(df)[1], num_noise_dims = num_noise_dims,
                                min_noise = min_noise, max_noise = max_noise)
    df <- cbind(df, noise_mat)

  }

  return(list(df = df, sample_size = sample_size))

}

one_grid_diff_with_bkg_noise <- function(sample_size = 260, num_noise_dims = 5,
                                         min_noise = -0.05, max_noise = 0.05) {


  if (((sample_size - (sample_size * 6/26)) %% 2) != 0) {

    stop("The sample size should be a product of two.")

  } else {

    if (((sqrt((sample_size - (sample_size * 6/26)) / 2)) %% 1) != 0) {

      stop("The square root should exists.")

    } else {

      n_value <- sqrt((sample_size - (sample_size * 0.6/2.6)) / 2)

    }

  }



  df1 <- grid_data(nx = n_value, ny = n_value, num_noise_dims = 0)

  df3 <- df1 + 3

  df1 <- dplyr::bind_rows(df1, df3)


  if (num_noise_dims != 0) {

    noise_mat <- gen_noise_dims(n = dim(df1)[1], num_noise_dims = num_noise_dims,
                                min_noise = min_noise, max_noise = max_noise)
    df1 <- cbind(df1, noise_mat)

  }

  ## To add background noise
  column_names_bkg <- paste0(rep("x", NCOL(df1)), 1:NCOL(df1))

  noise_bkg_val_list <- list()

  for (j in 1:NCOL(df1)) {
    noise_bkg_val_list[[j]] <- stats::rnorm(sample_size * 0.6/2.6, mean = 3, sd = 5)


  }

  df2 <- matrix(unlist(noise_bkg_val_list), ncol = length(noise_bkg_val_list))
  df <- rbind(df1, df2)
  df

}

two_grid_with_bkg_noise <- function(n_value = 10, num_noise_dims = 4,
                                    min_noise = -0.05, max_noise = 0.05) {

  df1 <- grid_data(nx = n_value, ny = n_value, num_noise_dims = 0)

  df3 <- df1 + 5

  df1 <- rbind(df1, df3)


  sample_size <- NROW(df1) + NROW(df1) * 0.6/2

  if (num_noise_dims != 0) {

    noise_mat <- gen_noise_dims(n = dim(df1)[1], num_noise_dims = num_noise_dims,
                                min_noise = min_noise, max_noise = max_noise)
    df1 <- cbind(df1, noise_mat)

  }

  ## To add background noise
  column_names_bkg <- paste0(rep("x", NCOL(df1)), 1:NCOL(df1))

  noise_bkg_val_list <- list()

  for (j in 1:NCOL(df1)) {
    noise_bkg_val_list[[j]] <- stats::rnorm(sample_size * 0.6/2.6, mean = 3, sd = 5)


  }

  df2 <- matrix(unlist(noise_bkg_val_list), ncol = length(noise_bkg_val_list))
  df <- rbind(df1, df2)

  return(list(df = df, sample_size = sample_size))

}

one_grid_diff <- function(sample_size = 200, num_noise_dims = 2,
                          min_noise = -0.05, max_noise = 0.05) {

  if ((sample_size %% 2) != 0) {

    stop("The sample size should be a product of two.")

  } else {

    if (((sqrt(sample_size/2)) %% 1) != 0) {

      stop("The square root should exists.")

    } else {

      n_value <- sqrt(sample_size/2)

    }

  }



  df1 <- grid_data(nx = n_value, ny = n_value, num_noise_dims = 0)
  df3 <- df1 + 3

  df <- dplyr::bind_rows(df1, df3)

  if (num_noise_dims != 0) {

    noise_mat <- gen_noise_dims(n = dim(df)[1], num_noise_dims = num_noise_dims,
                                min_noise = min_noise, max_noise = max_noise)
    df <- cbind(df, noise_mat)

  }

  return(list(df = df, sample_size = NROW(df)))

}
