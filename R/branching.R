#' Generate data with exponential shaped branches
#'
#' This function generates a dataset representing a structure with exponential shaped branches.
#'
#' @param n An integer value (default: 400) representing the sample size.
#' @param k An integer value (default: 4) representing the number of branches.
#' @return A tibble containing exponential shaped branches.
#'
#' @export
#'
#' @examples
#' set.seed(20240412)
#' expbranches <- gen_expbranches(n = 400, k = 4)
gen_expbranches <- function(n = 400, k = 4) {

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  if (k <= 0) {
    cli::cli_abort("k should be positive.")
  }

  n_vec <- gen_nsum(n = n, k = k)

  scale_vec <- sample(seq(0.5, 2, by = 0.1), size = k)

  df <- NULL

  for (i in 1:k) {

    df1 <- matrix(0, nrow = n_vec[i], ncol = 2)

    # gen the core curvilinear pattern in 2D
    df1[, 1] <- stats::runif(n_vec[i], -2, 2)


    if (i %% 2 != 0) {
      # i is odd
      df1[, 2] <- exp(-scale_vec[i] * df1[, 1]) + stats::runif(n_vec[i], 0, 0.1) # To generate mirror pattern

    } else {
      df1[, 2] <- exp(scale_vec[i] * df1[, 1]) + stats::runif(n_vec[i], 0, 0.1)
    }

    df <- rbind(df, df1)

  }

  df <- tibble::as_tibble(df, .name_repair = "minimal")
  names(df) <- paste0("x", 1:2)

  cli::cli_alert_success("Data generation completed successfully!!!")
  return(df)

}


#' Generate data with curvy shaped branches in a initial point
#'
#' This function generates a dataset representing a structure with curvy shaped branches.
#'
#' @param n A numeric value (default: 400) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @param k A numeric value (default: 4) representing the number of branches.
#' @param allow_share A logical value (default: TRUE). If TRUE, multiple branches may share the same 2D subspace. If FALSE, branches are sampled without replacement from all possible 2D subspaces until exhausted.
#' @return A data containing curvy shaped branches originated in one point.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' orgcurvybranches <- gen_orgcurvybranches(n = 400, k = 4)
gen_orgcurvybranches <- function(n = 400, p = 4, k = 4,
                                 allow_share = TRUE) {

  if (p < 2) cli::cli_abort("p should be greater than 2.")
  if (n <= 0) cli::cli_abort("n should be positive.")
  if (k <= 0) cli::cli_abort("k should be positive.")

  n_vec <- gen_nsum(n = n, k = k)

  # --- Generate all 2D subspace combinations ---
  comb <- gtools::combinations(p, 2)         # matrix, 2 columns
  comb <- tibble::as_tibble(comb, .name_repair = "minimal")            # convert to tibble
  colnames(comb) <- paste0("V", 1:ncol(comb)) # safely assign names based on actual column count


  # --- Sample combinations depending on allow_share ---
  if (allow_share) {
    comb_select <- dplyr::sample_n(comb, size = k, replace = TRUE)
    scale_vec <- sample(seq(1, 8, by = 0.5), size = k, replace = TRUE)
  } else {
    if (k <= NROW(comb)) {
      comb_select <- dplyr::sample_n(comb, size = k, replace = FALSE)
      scale_vec <- rep(1, k)
    } else {
      remaining_needed <- k - NROW(comb)
      comb_select <- dplyr::bind_rows(comb,
                                      dplyr::sample_n(comb, size = remaining_needed, replace = TRUE))
      scale_vec <- sample(seq(1, 8, by = 0.5), size = k, replace = TRUE)
    }
  }

  df <- matrix(0, nrow = 0, ncol = p)

  for (i in 1:k) {
    index1 <- comb_select$V1[i]
    index2 <- comb_select$V2[i]

    a <- stats::runif(n_vec[i], 0, 2)
    poly_basis <- stats::poly(a, degree = 2, raw = TRUE)
    b <- -scale_vec[i] * poly_basis[, 2] + stats::runif(n_vec[i], 0, 0.5)

    df1 <- matrix(c(a, b), ncol = 2)
    colnames(df1) <- paste0("x", c(index1, index2))

    # --- Add noise dimensions ---
    if (p > 2) {

      noise_df <- gen_noisedims(n = n_vec[i], p = (p-2), m = rep(0, p-2), s = rep(0.05, p - 2))
      if (!is.matrix(noise_df)) noise_df <- as.matrix(noise_df)

      vector <- 1:p
      filter_values <- c(index1, index2)
      colnames(noise_df) <- paste0("x", vector[!(vector %in% filter_values)])

      df1 <- cbind(df1, noise_df)[, paste0("x", 1:p)]
    }

    df <- rbind(df, df1)
  }

  df <- tibble::as_tibble(df, .name_repair = "minimal")
  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Data generation completed successfully!!!")
  return(df)
}



#' Generate data with linear shaped branches in a initial point
#'
#' This function generates a dataset representing a structure with linear shaped branches.
#'
#' @param n A numeric value (default: 400) representing the sample size.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @param k A numeric value (default: 4) representing the number of branches.
#' @param allow_share A logical value (default: TRUE).
#' If TRUE, multiple branches may share the same 2D subspace.
#' If FALSE, branches are sampled without replacement from all possible 2D
#' subspaces until exhausted.
#' @return A data containing linear shaped branches originated in one point.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' orglinearbranches <- gen_orglinearbranches(n = 400, p = 4, k = 4)
gen_orglinearbranches <- function(n = 400, p = 4, k = 4,
                                  allow_share = TRUE) {

  if (p < 2) cli::cli_abort("p should be greater than 2.")
  if (n <= 0) cli::cli_abort("n should be positive.")
  if (k <= 0) cli::cli_abort("k should be positive.")

  n_vec <- gen_nsum(n = n, k = k)

  # --- Generate all 2D subspace combinations ---
  comb <- gtools::combinations(p, 2)         # matrix, 2 columns
  comb <- tibble::as_tibble(comb, .name_repair = "minimal")            # convert to tibble
  colnames(comb) <- paste0("V", 1:ncol(comb)) # safely assign names based on actual column count


  # --- Sample combinations depending on allow_share ---
  if (allow_share) {
    comb_select <- dplyr::sample_n(comb, size = k, replace = TRUE)
    scale_vec <- sample(seq(1, 8, by = 0.5), size = k, replace = TRUE)
  } else {
    if (k <= NROW(comb)) {
      comb_select <- dplyr::sample_n(comb, size = k, replace = FALSE)
      scale_vec <- rep(1, k)
    } else {
      # Fill with replacement if needed
      remaining_needed <- k - NROW(comb)
      comb_select <- dplyr::bind_rows(comb,
                                      dplyr::sample_n(comb, size = remaining_needed, replace = TRUE))
      scale_vec <- sample(seq(1, 8, by = 0.5), size = k, replace = TRUE)
    }
  }

  df <- matrix(0, nrow = 0, ncol = p)

  for (i in 1:k) {
    index1 <- comb_select$V1[i]
    index2 <- comb_select$V2[i]

    a <- stats::runif(n_vec[i], 0, 2)
    poly_basis <- stats::poly(a, degree = 1, raw = TRUE)
    b <- -scale_vec[i] * poly_basis[, 1] + stats::runif(n_vec[i], 0, 0.5)

    df1 <- matrix(c(a, b), ncol = 2)
    colnames(df1) <- paste0("x", c(index1, index2))

    # --- Add noise dimensions ---
    if (p > 2) {

      noise_df <- gen_noisedims(n = n_vec[i], p = (p-2), m = rep(0, p-2), s = rep(0.05, p - 2))
      if (!is.matrix(noise_df)) noise_df <- as.matrix(noise_df)

      # Assign column names for noise dims
      vector <- 1:p
      filter_values <- c(index1, index2)
      colnames(noise_df) <- paste0("x", vector[!(vector %in% filter_values)])

      # Combine main and noise dimensions in order
      df1 <- cbind(df1, noise_df)[, paste0("x", 1:p)]
    }

    df <- rbind(df, df1)
  }

  df <- tibble::as_tibble(df, .name_repair = "minimal")
  names(df) <- paste0("x", 1:p)

  cli::cli_alert_success("Data generation completed successfully!!!")
  return(df)
}



#' Generate data with linear shaped branches
#'
#' This function generates a dataset representing a structure with linear shaped branches.
#'
#' @param n A numeric value (default: 400) representing the sample size.
#' @param k A numeric value (default: 4) representing the number of branches.
#' @return A data containing linear shaped branches.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' linearbranches <- gen_linearbranches(n = 400, k = 4)
gen_linearbranches <- function(n = 400, k = 4) {

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  if (k <= 0) {
    cli::cli_abort("k should be positive.")
  }

  n_vec <- gen_nsum(n = n, k = k)

  ## Initialize main branch 1
  x1 <- stats::runif(n_vec[1], -2, 8)
  poly_basis_1 <- stats::poly(x1, degree = 1, raw = TRUE)
  x2 <- 0.5 * poly_basis_1[, 1] + stats::runif(n_vec[1], 0, 0.5)
  df1 <- matrix(c(x1, x2), ncol = 2)

  ## Initialize main branch 2
  x1 <- stats::runif(n_vec[2], -6, 2)
  poly_basis_2 <- stats::poly(x1, degree = 1, raw = TRUE)
  x2 <- -0.5 * poly_basis_2[, 1] + stats::runif(n_vec[2], 0, 0.5)
  df2 <- matrix(c(x1, x2), ncol = 2)

  df <- rbind(df1, df2)

  if(k > 2) {

    # Define the excluded ranges for x and y coordinates of starting points
    excluded_x_range <- c(-8, -7, -2, 2, 7, 8)
    excluded_y_range <- c(7, 8)

    # Define the full sequence
    full_sequence <- seq(-3, 3, by = 0.1)

    # Define the values to exclude
    excluded_values <- c(-0.5, 0.5)

    # Filter out the excluded values from the sequence
    filtered_sequence <- full_sequence[!(full_sequence %in% excluded_values)]

    # Sample from the filtered sequence
    scale_vec <- sample(filtered_sequence, size = k-2, replace = TRUE)

    for (i in 3:k) {
      start_point <- NA
      while (TRUE) {
        # Randomly select a starting point (a row) from the existing 'df'
        start_point_index <- sample(1:NROW(df), 1)
        potential_start_point <- df[start_point_index, ]

        # Check if the starting point's x and y coordinates are within the excluded ranges
        x_within_excluded <- (potential_start_point[1] >= excluded_x_range[1] & potential_start_point[1] <= excluded_x_range[2]) |
          (potential_start_point[1] >= excluded_x_range[3] & potential_start_point[1] <= excluded_x_range[4]) |
          (potential_start_point[1] >= excluded_x_range[5] & potential_start_point[1] <= excluded_x_range[6])

        # Check if the starting point's y coordinate is within the excluded y range
        y_within_excluded <- potential_start_point[2] >= excluded_y_range[1] & potential_start_point[2] <= excluded_y_range[2]

        # If the starting point is NOT within either excluded range, accept it
        if (!x_within_excluded & !y_within_excluded) {
          start_point <- potential_start_point
          break
        }
        # Otherwise, continue sampling
      }

      # Define parameters for the new branch (you can customize these)
      branch_length <- n_vec[i] # Number of points in the new branch
      x1_start <- start_point[1] # Adjust starting x1
      x1_end <- start_point[1] + 1   # Adjust ending x1

      # Generate x1 values for the new branch
      x1 <- stats::runif(branch_length, x1_start, x1_end)
      poly_basis_branch <- stats::poly(x1, degree = 1, raw = TRUE)
      x2 <- scale_vec[i-2] * (poly_basis_branch[, 1] - start_point[1]) + start_point[2] + stats::runif(branch_length, 0, 0.2)

      # Create the new branch data frame
      df_branch <- matrix(c(x1, x2), ncol = 2)

      # Combine the new branch with the main data frame
      df <- rbind(df, df_branch)
    }

  }



  df <- tibble::as_tibble(df, .name_repair = "minimal")
  names(df) <- paste0("x", 1:2)

  cli::cli_alert_success("Data generation completed successfully!!!")
  return(df)

}

#' Generate data with curvy shaped branches
#'
#' This function generates a dataset representing a structure with non-linear shaped branches.
#'
#' @param n A numeric value (default: 400) representing the sample size.
#' @param k A numeric value (default: 4) representing the number of branches.
#' @return A data containing non-linear shaped branches.
#' @export
#'
#' @examples
#' set.seed(20240412)
#' curvybranches <- gen_curvybranches(n = 400, k = 4)
gen_curvybranches <- function(n = 400, k = 4) {

  if (n <= 0) {
    cli::cli_abort("n should be positive.")
  }

  if (k <= 0) {
    cli::cli_abort("k should be positive.")
  }

  n_vec <- gen_nsum(n = n, k = k)

  ## Initialize main branch 1
  x1 <- stats::runif(n_vec[1], 0, 1)
  poly_basis_1 <- stats::poly(x1, degree = 2, raw = TRUE)
  x2 <- 0.1 * poly_basis_1[, 1] + 1 * poly_basis_1[, 2] + stats::runif(n_vec[1], 0, 0.05)
  df1 <- matrix(c(x1, x2), ncol = 2)

  ## Initialize main branch 2
  x1 <- stats::runif(n_vec[2], -1, 0)
  poly_basis_1 <- stats::poly(x1, degree = 2, raw = TRUE)
  x2 <- 0.1 * poly_basis_1[, 1] - 2 * poly_basis_1[, 2] + stats::runif(n_vec[2], 0, 0.05)
  df2 <- matrix(c(x1, x2), ncol = 2)

  df <- rbind(df1, df2)
  df_initial <- df # Initialize df with the connected initial branches

  if(k > 2) {

    allowed_x_range <- c(-0.15, 0.15)
    full_sequence <- seq(-2, 2, by = 0.5)
    excluded_scale_values <- c(2, -1)
    filtered_sequence <- full_sequence[!(full_sequence %in% excluded_scale_values)]
    scale_vec <- sample(filtered_sequence, size = k - 2, replace = TRUE)

    for (i in 3:k) {
      start_point <- NA
      while (TRUE) {
        start_point_index <- sample(1:NROW(df_initial), 1)
        potential_start_point <- df_initial[start_point_index, ]
        x_within_allowed <- potential_start_point[1] >= allowed_x_range[1] & potential_start_point[1] <= allowed_x_range[2]
        if (x_within_allowed) {
          start_point <- potential_start_point
          break
        }
      }

      # Define parameters for the new branch (you can customize these)
      branch_length <- n_vec[i] # Number of points in the new branch
      x1_start <- start_point[1] # Adjust starting x1
      x1_end <- start_point[1] + 1   # Adjust ending x1

      # Generate x1 values for the new branch
      x1 <- stats::runif(branch_length, x1_start, x1_end)
      poly_basis_branch <- stats::poly(x1, degree = 2, raw = TRUE)
      x2 <-  0.1 * poly_basis_branch[, 1] - scale_vec[i-2] * (poly_basis_branch[, 2] - start_point[1]) + start_point[2]

      # Create the new branch data frame
      df_branch <- matrix(c(x1, x2), ncol = 2)

      # Combine the new branch with the main data frame
      df <- rbind(df, df_branch)
    }

  }

  df <- tibble::as_tibble(df, .name_repair = "minimal")
  names(df) <- paste0("x", 1:2)

  cli::cli_alert_success("Data generation completed successfully!!!")
  return(df)

}
