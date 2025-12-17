#' Generate Multiple Clusters
#'
#' This function generates a dataset with multiple clusters in high-dimensional space.
#' Each cluster can have a different shape, scale, rotation, and centroid,
#' allowing the construction of complex synthetic datasets.
#'
#' @param n An integer vector (default: c(200, 500, 300)) representing the sample sizes
#'   for each cluster. Must have length \code{k}.
#' @param k An integervalue (default: 3) representing the number of clusters.
#' @param loc A numeric matrix giving the centroids of the clusters.
#'   The number of rows must equal \code{k}; the number of columns should match
#'   the maximum dimensionality across all shapes.
#' @param scale A numeric vector (default: c(3, 1, 2)) giving the scaling factors
#'   for each cluster. Must have length \code{k}.
#' @param shape A character vector (default: c("gaussian", "cone", "unifcube"))
#'   specifying the generator function to use for each cluster. Must have length \code{k}.
#' @param rotation A list of rotation matrices (one per cluster), or \code{NULL}.
#'   Each matrix must be square with dimension equal to the total number of
#'   structural dimensions in the dataset. Rotation matrices can be generated
#'   using \code{\link{gen_rotation}} for convenience.
#' @param add_bkg Logical (default: FALSE). If \code{TRUE}, adds background noise
#'   sampled from a multivariate normal distribution centered on the dataset mean
#'   with standard deviations matching the observed spread.
#' @param ... Additional arguments passed to the cluster generator functions.
#'
#' @return A tibble containing all generated clusters, with columns \code{x1, x2, ...}
#'   for dimensions and a \code{cluster} label.
#'
#' @export
#'
#' @examples
#' set.seed(20240412)
#'
#' # Example rotation matrices for 4D space
#' rot1 <- gen_rotation(p = 4, planes_angles = list(list(plane = c(1, 2), angle = 60),
#'                                                 list(plane = c(3, 4), angle = 90)))
#' rot2 <- gen_rotation(p = 4, planes_angles = list(list(plane = c(1, 3), angle = 30)))
#' rot3 <- gen_rotation(p = 4, planes_angles = list(list(plane = c(2, 4), angle = 45)))
#'
#' clust_data <- gen_multicluster(
#'   n = c(200, 300, 500),
#'   k = 3,
#'   loc = matrix(c(
#'     0, 0, 0, 0,
#'     5, 9, 0, 0,
#'     3, 4, 10, 7
#'   ), nrow = 3, byrow = TRUE),
#'   scale = c(3, 1, 2),
#'   shape = c("gaussian", "cone", "unifcube"),
#'   rotation = list(rot1, rot2, rot3),
#'   add_bkg = FALSE
#' )
gen_multicluster <- function(n = c(200, 300, 500),
                             k = 3,
                             loc = matrix(c(
                               0, 0, 0, 0,
                               5, 9, 0, 0,
                               3, 4, 10, 7
                             ), nrow = 3, byrow = TRUE),
                             scale = c(3, 1, 2),
                             shape = c("gaussian", "bluntedcorn", "unifcube"),
                             rotation = NULL,
                             add_bkg = FALSE,
                             ...) {

  # --- checks ---
  if (k < 1) cli::cli_abort("k should be greater than 1.")
  if (length(n) != k) cli::cli_abort("n should contain exactly {.val {k}} values.")
  if (any(n < 0)) cli::cli_abort("Values in n should be positive.")
  if (length(scale) != k) cli::cli_abort("scale should contain exactly {.val {k}} values.")
  if (any(scale < 0)) cli::cli_abort("Values in scale should be positive.")
  if (length(shape) != k) cli::cli_abort("shape should contain exactly {.val {k}} values.")

  # --- handle rotation argument ---
  if (!is.null(rotation)) {
    if (is.matrix(rotation)) {
      # Single rotation matrix: apply to all clusters
      rotation <- replicate(k, rotation, simplify = FALSE)
    } else if (!is.list(rotation)) {
      # Single non-list (e.g., numeric or character)
      rotation <- replicate(k, rotation, simplify = FALSE)
    } else if (is.list(rotation) && length(rotation) < k) {
      # Extend shorter lists
      rotation <- c(rotation, rep(list(NULL), k - length(rotation)))
    }
  } else {
    # Default: no rotation
    rotation <- vector("list", k)
  }

  # --- generate clusters ---
  raw_clusters <- vector("list", k)
  max_dims <- 0L

  for (i in seq_len(k)) {
    fn <- tryCatch(match.fun(paste0("gen_", shape[i])),
                   error = function(e) cli::cli_abort("No generator found for shape {.val {shape[i]}}."))

    fn_args <- names(formals(fn))
    extra_args <- list(...)
    pass_args <- extra_args[names(extra_args) %in% fn_args]

    cluster_df <- do.call(fn, c(list(n = n[i]), pass_args))
    cluster_df <- as.data.frame(cluster_df)
    d_i <- ncol(cluster_df)
    max_dims <- max(max_dims, d_i)

    raw_clusters[[i]] <- cluster_df
  }

  # --- pad clusters to max_dims and apply transforms ---
  dfs <- vector("list", k)
  for (i in seq_len(k)) {
    cluster_df <- raw_clusters[[i]]
    d_i <- ncol(cluster_df)

    # pad with Gaussian noise if fewer than max_dims
    if (d_i < max_dims) {
      mean_vals <- colMeans(cluster_df, na.rm = TRUE)
      m <- rep(mean(mean_vals), max_dims - d_i)
      s <- rep(0.2, max_dims - d_i)
      pad <- gen_noisedims(n = n[i], p = max_dims - d_i, m = m, s = s)
      cluster_df <- cbind(cluster_df, pad)
    }

    # --- rotation ---
    rot_mat <- rotation[[i]]
    if (!is.null(rot_mat)) {
      if (is.character(rot_mat) && rot_mat == "identity") {
        rot_mat <- diag(ncol(cluster_df))
      }

      if (!is.matrix(rot_mat)) {
        cli::cli_abort("rotation[[{i}]] must be a matrix if provided.")
      }

      if (!all(dim(rot_mat) == c(ncol(cluster_df), ncol(cluster_df)))) {
        cli::cli_abort("rotation[[{i}]] must be square with dimension {.val {ncol(cluster_df)}}.")
      }

      cluster_df <- t(rot_mat %*% t(cluster_df))
    }

    # normalize, scale, and translate
    cluster_df <- normalize_data(cluster_df)
    cluster_df <- scale[i] * cluster_df
    cluster_df <- apply(cluster_df, 2, function(col) col - mean(col))

    if (!is.null(loc)) {
      if (NCOL(loc) != max_dims) {
        cli::cli_abort("Number of cols in loc must match {.val {max_dims}} dimensions.")
      }
      cluster_df <- cluster_df + matrix(rep(loc[i, ], NROW(cluster_df)), ncol = max_dims, byrow = TRUE)
    }

    # add cluster label
    cluster_df <- tibble::as_tibble(cluster_df, .name_repair = "minimal")
    names(cluster_df)[1:max_dims] <- paste0("x", 1:max_dims)
    dfs[[i]] <- cluster_df |> dplyr::mutate(cluster = paste0("cluster", i))
  }

  # --- combine ---
  df <- dplyr::bind_rows(dfs)

  # background noise
  if (isTRUE(add_bkg)) {
    mean_vals <- colMeans(df[sapply(df, is.numeric)])
    std_vals  <- sapply(df[sapply(df, is.numeric)], stats::sd)
    noise_df <- gen_bkgnoise(n = max(n) * 0.1, p = max_dims, m = mean_vals, s = std_vals) |>
      dplyr::mutate(cluster = "bkg_noise")
    df <- dplyr::bind_rows(df, noise_df)
  }

  df <- randomize_rows(df)
  cli::cli_alert_success("Multiple clusters generation completed successfully!!!")
  return(df)
}


