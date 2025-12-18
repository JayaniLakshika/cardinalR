#' Generate Gaussian cluster with the Mobius Cluster
#'
#' This function generates a dataset consisting of a mobius cluster and Gaussian cluster.
#'
#' @param n An integer vector (default: c(200, 100)) representing the sample sizes.
#' @param p An integer value (default: 4) representing the number of dimensions.
#' @return A tibble containing the mobius cluster and Gaussian cluster.
#' @export
#'
#' @examples
#' mobgau <- make_mobiusgau(n = c(200, 100), p = 4)
make_mobiusgau <- function(n = c(200, 100), p = 4) {

  if (p < 3) {
    cli::cli_abort("p should be greater than or equal to 3.")
  }

  if (length(n) != 2) {
    cli::cli_abort("n should contain exactly two values.")
  }

  if (any(n < 0)) {
    cli::cli_abort("Values in n should be positive.")
  }

  ## To generate data
  df <- gen_multicluster(n = n, p = p, k = 2,
                         loc = matrix(c(
                           rep(0, p),
                           rep(0, p)
                         ), nrow = 2, byrow = TRUE),
                         scale = c(1, 0.3),
                         shape = c("mobius", "gaussian"),
                         rotation = NULL,
                         is_bkg = FALSE)

  return(df)
}

#' Generate Multiple Gaussian Clusters
#'
#' This function generates a dataset consisting of multiple Gaussian clusters.
#'
#' @param n A numeric vector (default: c(300, 200, 500)) representing the sample sizes.
#' @param p A numeric value (default: 4) representing the number of dimensions.
#' @param k A numeric value (default: 5) representing the number of clusters.
#' @param loc A numeric matrix (default: NULL) representing the locations/centroids of clusters.
#' @param scale A numeric vector (default: NULL) representing the scaling factors of clusters.
#' @return A data containing the Gaussian clusters.
#' @export
#'
#' @examples
#' loc_matrix <- matrix(c(0, 0, 0, 0,
#' 5, 9, 0, 0,
#' 3, 4, 10, 7
#' ), nrow = 3, byrow = TRUE)
#' multigau <- make_multigau(n = c(300, 200, 500),
#' p = 4, k = 3,
#' loc = loc_matrix, scale = c(0.2, 1.5, 0.5))
make_multigau <- function(n = c(300, 200, 500), p = 4, k = 3, loc = NULL, scale = NULL) {

  if (p < 2) {
    cli::cli_abort("p should be greater than 2.")
  }

  if (k < 1) {
    cli::cli_abort("k should be greater than 1.")
  }

  if (length(n) != k) {
    cli::cli_abort("n should contain exactly {.val {k}} values.")
  }

  if (any(n < 0)) {
    cli::cli_abort("Values in n should be positive.")
  }

  if (length(scale) != k) {
    cli::cli_abort("scale should contain exactly {.val {k}} values.")
  }

  if (any(scale < 0)) {
    cli::cli_abort("Values in scale should be positive.")
  }

  if (!is.matrix(loc)) {
    cli::cli_abort("loc should be a matrix.")
  }

  if (NROW(loc) != k) {
    cli::cli_abort("Number of rows in loc should be {.val {p}}.")
  }

  if (NCOL(loc) != p) {
    cli::cli_abort("Number of rows in loc should be {.val {k}}.")
  }

  if (is.null(loc)) {
    loc <- gen_clustloc(p = p, k = k)
  }

  if(is.null(scale)) {
    scale <- sample(seq(0.5, 2.3), k, replace = TRUE)
  }

  ## To generate data
  df <- gen_multicluster(n = n, p = p, k = k,
                         loc = loc,
                         scale = scale,
                         shape = rep("gaussian", k),
                         rotation = NULL,
                         is_bkg = FALSE)

  return(df)

}

#' Generate Curvy Quadratic and Gaussian Clusters
#'
#' This function generates synthetic high-dimensional data containing
#' two clusters: one quadratic-shaped cluster and one Gaussian-shaped
#' cluster. The clusters are positioned apart in feature space with
#' different scaling factors.
#'
#' @param n A numeric vector of length 2, specifying the number of
#'   observations in each cluster. All values must be positive.
#' @param p Integer. Number of dimensions. Must be at least 3.
#'
#'
#' @return A tibble containing \eqn{n[1] + n[2]} rows and \eqn{p} columns,
#'   with generated features (\code{x1, x2, ..., xp}) and a
#'   \code{cluster} label.
#'
#' @examples
#' # Generate 2 clusters in 4D: one quadratic, one Gaussian
#' curvygau <- make_curvygau()
#'
#' @export
make_curvygau <- function(n = c(200, 100), p = 4) {

  if (p < 3) {
    cli::cli_abort("p should be greater than or equal to 3.")
  }

  if (length(n) != 2) {
    cli::cli_abort("n should contain exactly two values.")
  }

  if (any(n < 0)) {
    cli::cli_abort("Values in n should be positive.")
  }

  ## To generate data
  df <- gen_multicluster(n = n, p = p, k = 2,
                         loc = matrix(c(
                           -0.7, -0.7, 0, 0,
                           0, 0, 0.7, 0.7
                         ), nrow = 2, byrow = TRUE),
                         scale = c(1, 0.1),
                         shape = c("quadratic", "gaussian"),
                         rotation = NULL,
                         is_bkg = FALSE)

  return(df)


}

#' Generate Multiple Interlocked Circles in High-Dimensional Space
#'
#' This function generates \eqn{k} interlocked circular clusters in a
#' \eqn{p}-dimensional space. The circles are constructed using
#' \code{gen_multicluster()}, with each circle positioned in a different
#' coordinate plane and slightly offset so that they interlock with a
#' central circle (hub-like structure).
#'
#' @param n An integer vector of length \eqn{k} giving the number of points
#'   in each circle. Default is \code{c(200, 100)}.
#' @param p Integer, the dimensionality of the embedding space. Must be
#'   at least 3. Default is \code{4}.
#' @param k Integer, the number of circles to generate. Default is \code{2}.
#' @param offset Numeric, the amount of positional shift applied to each
#'   circle along the second coordinate axis to prevent complete overlap.
#'   Default is \code{0.5}.
#'
#'
#' @return A data frame (or tibble, depending on \code{gen_multicluster()})
#'   containing the generated points and cluster assignments.
#' @export
#'
#' @examples
#' # Generate two interlocked circles in 4-D
#' twolink_circles <- make_klink_circles()
make_klink_circles <- function(n = c(200, 100), p = 4, k = 2, offset = 0.5) {
  if (length(n) != k) {
    stop("Length of n must equal k (number of circles).")
  }

  # Locations: each circle gets a small offset along axis 2
  loc <- matrix(0, nrow = k, ncol = p)
  loc[, 2] <- seq(0, by = offset, length.out = k)

  # Build rotations:
  # Circle 1 stays in (1,2) plane; subsequent ones rotated into (2,3), (3,4), etc.
  rotations <- vector("list", k)
  angle <- 90

  for (i in seq_len(k)) {
    if (i == 1) {
      # Circle 1: no rotation (identity matrix)
      rotations[[i]] <- diag(p)
    } else {
      # Circle i: rotated into (i, i+1) plane
      plane <- c(i, i + 1)
      if (max(plane) > p) {
        plane <- (plane - 1) %% p + 1  # wrap around
      }

      theta <- angle * pi / 180  # degrees -> radians
      R <- diag(p)
      R[plane, plane] <- matrix(
        c(cos(theta), -sin(theta),
          sin(theta),  cos(theta)),
        nrow = 2
      )
      rotations[[i]] <- R
    }
  }
  names(rotations) <- paste0("cluster", seq_len(k))

  # Generate with gen_multicluster
  df <- gen_multicluster(
    n = n, p = p, k = k,
    loc = loc,
    scale = rep(1, k),
    shape = rep("circle", k),
    rotation = rotations,
    is_bkg = FALSE
  )

  return(df)
}


#' Generate a Chain of Interlocked Circles in High-Dimensional Space
#'
#' This function generates \eqn{k} interlocked circular clusters in a
#' \eqn{p}-dimensional space. Unlike \code{make_klink_circles()}, the circles
#' are arranged in a **chain-like structure**, where each circle interlocks
#' only with its immediate neighbor, resembling links in a chain.
#'
#' @param n An integer vector of length \eqn{k} giving the number of points
#'   in each circle. Default is \code{c(200, 100)}.
#' @param p Integer, the dimensionality of the embedding space. Must be
#'   at least 3. Default is \code{4}.
#' @param k Integer, the number of circles to generate. Default is \code{2}.
#' @param offset Numeric, the positional shift applied to each circle along
#'   its linking axis to ensure interlocking instead of overlap. Default is
#'   \code{0.5}.
#' @param angle Numeric, the rotation angle (in degrees) used when placing
#'   each subsequent circle into its respective plane. Default is \code{90}.
#'
#'
#' @return A data frame (or tibble, depending on \code{gen_multicluster()})
#'   containing the generated points and cluster assignments.
#' @export
#'
#' @examples
#' # Generate two chain-linked circles in 4-D
#' twochain_circles <- make_chain_circles()
make_chain_circles <- function(n = c(200, 100), p = 4, k = 2, offset = 0.5, angle = 90) {
  if (length(n) != k) {
    stop("Length of n must equal k (number of circles).")
  }
  if (p < 3) {
    stop("Need at least 3 dimensions to interlock circles.")
  }

  # Initialize locations: each circle offset on its linking axis
  loc <- matrix(0, nrow = k, ncol = p)

  # Example: cluster i is shifted along axis (i+1)
  for (i in seq_len(k)) {
    shift_axis <- (i %% (p - 1)) + 2   # use axes 2..p cyclically
    loc[i, shift_axis] <- offset
  }

  # Build rotations
  rotations <- vector("list", k)
  for (i in seq_len(k)) {
    if (i == 1) {
      # First circle: no rotation
      rotations[[i]] <- diag(p)
    } else {
      # Circle i rotated in (i, i+1) plane, wrapping if needed
      plane <- c(i, i + 1)
      plane[plane > p] <- plane[plane > p] - (p - 1)

      theta <- angle * pi / 180  # degrees -> radians
      R <- diag(p)               # start with identity
      R[plane, plane] <- matrix(
        c(cos(theta), -sin(theta),
          sin(theta),  cos(theta)),
        nrow = 2
      )
      rotations[[i]] <- R
    }
  }
  names(rotations) <- paste0("cluster", seq_len(k))

  # Generate circles
  df <- gen_multicluster(
    n = n, p = p, k = k,
    loc = loc,
    scale = rep(1, k),
    shape = rep("circle", k),
    rotation = rotations,
    is_bkg = FALSE
  )

  return(df)
}


#' Generate Multiple Interlocked curvycycle in High-Dimensional Space
#'
#' This function generates \eqn{k} interlocked circular clusters in a
#' \eqn{p}-dimensional space. The curvycycle are constructed using
#' \code{gen_multicluster()}, with each curvycycle positioned in a different
#' coordinate plane and slightly offset so that they interlock with a
#' central curvycycle (hub-like structure).
#'
#' @param n An integer vector of length \eqn{k} giving the number of points
#'   in each curvycycle. Default is \code{c(200, 100)}.
#' @param p Integer, the dimensionality of the embedding space. Must be
#'   at least 3. Default is \code{4}.
#' @param k Integer, the number of curvycycle to generate. Default is \code{2}.
#' @param offset Numeric, the amount of positional shift applied to each
#'   curvycycle along the second coordinate axis to prevent complete overlap.
#'   Default is \code{0.5}.
#'
#'
#' @return A data frame (or tibble, depending on \code{gen_multicluster()})
#'   containing the generated points and cluster assignments.
#' @export
#'
#' @examples
#' # Generate two interlocked curvycycle in 4-D
#' twolink_curvycycle <- make_klink_curvycycle()
make_klink_curvycycle <- function(n = c(200, 100), p = 4, k = 2, offset = 0.5) {
  if (length(n) != k) {
    stop("Length of n must equal k (number of curvycycle).")
  }

  # Locations: each curvycycle gets a small offset along axis 2
  loc <- matrix(0, nrow = k, ncol = p)
  loc[, 2] <- seq(0, by = offset, length.out = k)

  # Build rotations:
  # curvycycle 1 stays in (1,2) plane; subsequent ones rotated into (2,3), (3,4), etc.
  rotations <- vector("list", k)

  angle <- 90
  for (i in seq_len(k)) {
    if (i == 1) {
      # First cluster: no rotation (identity)
      rotations[[i]] <- diag(p)
    } else {
      # Subsequent clusters rotated into different orthogonal planes
      plane <- c(i, i + 1)
      plane[plane > p] <- plane[plane > p] - (p - 1)

      theta <- angle * pi / 180  # convert to radians
      R <- diag(p)               # identity matrix

      # insert 2D rotation block into plane
      R[plane, plane] <- matrix(
        c(cos(theta), -sin(theta),
          sin(theta),  cos(theta)),
        nrow = 2
      )
      rotations[[i]] <- R
    }
  }
  names(rotations) <- paste0("cluster", seq_len(k))

  # Generate with gen_multicluster
  df <- gen_multicluster(
    n = n, p = p, k = k,
    loc = loc,
    scale = rep(1, k),
    shape = rep("curvycycle", k),
    rotation = rotations,
    is_bkg = FALSE
  )

  return(df)
}


#' Generate a Chain of Interlocked curvycycle in High-Dimensional Space
#'
#' This function generates \eqn{k} interlocked circular clusters in a
#' \eqn{p}-dimensional space. Unlike \code{make_klink_curvycycle()}, the curvycycle
#' are arranged in a **chain-like structure**, where each curvycycle interlocks
#' only with its immediate neighbor, resembling links in a chain.
#'
#' @param n An integer vector of length \eqn{k} giving the number of points
#'   in each curvycycle. Default is \code{c(200, 100)}.
#' @param p Integer, the dimensionality of the embedding space. Must be
#'   at least 3. Default is \code{4}.
#' @param k Integer, the number of curvycycle to generate. Default is \code{2}.
#' @param offset Numeric, the positional shift applied to each curvycycle along
#'   its linking axis to ensure interlocking instead of overlap. Default is
#'   \code{0.5}.
#' @param angle Numeric, the rotation angle (in degrees) used when placing
#'   each subsequent curvycycle into its respective plane. Default is \code{90}.
#'
#'
#' @return A data frame (or tibble, depending on \code{gen_multicluster()})
#'   containing the generated points and cluster assignments.
#' @export
#'
#' @examples
#' # Generate two chain-linked curvycycle in 4-D
#' twochain_curvycycle <- make_chain_curvycycle()
make_chain_curvycycle <- function(n = c(200, 100), p = 4, k = 2, offset = 0.5, angle = 90) {
  if (length(n) != k) {
    stop("Length of n must equal k (number of curvycycle).")
  }
  if (p < 3) {
    stop("Need at least 3 dimensions to interlock curvycycle.")
  }

  # Initialize locations: each curvycycle offset on its linking axis
  loc <- matrix(0, nrow = k, ncol = p)

  # Example: cluster i is shifted along axis (i+1)
  for (i in seq_len(k)) {
    shift_axis <- (i %% (p - 1)) + 2   # use axes 2..p cyclically
    loc[i, shift_axis] <- offset
  }

  # Build rotations
  rotations <- vector("list", k)
  for (i in seq_len(k)) {
    if (i == 1) {
      # cluster 1: no rotation (identity)
      rotations[[i]] <- diag(p)
    } else {
      # rotate cluster i in the (i, i+1) plane by `angle`
      plane <- c(i, i + 1)
      plane[plane > p] <- plane[plane > p] - (p - 1)

      theta <- angle * pi / 180  # convert to radians
      R <- diag(p)               # identity

      # insert 2D rotation block into the plane
      R[plane, plane] <- matrix(
        c(cos(theta), -sin(theta),
          sin(theta),  cos(theta)),
        nrow = 2
      )
      rotations[[i]] <- R
    }
  }
  names(rotations) <- paste0("cluster", seq_len(k))

  # Generate curvycycle
  df <- gen_multicluster(
    n = n, p = p, k = k,
    loc = loc,
    scale = rep(1, k),
    shape = rep("curvycycle", k),
    rotation = rotations,
    is_bkg = FALSE
  )

  return(df)
}

#' Generate Concentric Circles with a Gaussian Cluster in High Dimensions
#'
#' This function generates a dataset consisting of multiple circular clusters
#' together with a single Gaussian cluster in a \eqn{p}-dimensional space.
#' The circles are placed concentrically at the origin with varying scales,
#' while the Gaussian cluster serves as an additional background or center cluster.
#'
#' @param n An integer vector of length \code{num_circles + 1}, giving the
#'   number of points in each cluster (circles first, followed by the Gaussian).
#'   Default is \code{c(200, 100, 100)}.
#' @param p Integer, the dimensionality of the embedding space. Must be at least 3.
#'   Default is \code{4}.
#' @param num_circles Integer, the number of circular clusters to generate.
#'   Default is \code{2}.
#' @param scale_circles Numeric vector of length \code{num_circles}, giving
#'   the scale (radius) of each circular cluster. Default is \code{c(1, 2)}.
#'
#' @return A data frame (or tibble, depending on \code{gen_multicluster()})
#'   containing the generated dataset with cluster assignments.
#'
#' @examples
#' # Two circles (radii 1 and 2) plus one Gaussian cluster in 4-D
#' gaucircles <- make_gaucircles()
#'
#'
#' @export
make_gaucircles <- function(n = c(200, 100, 100), p = 4, num_circles = 2, scale_circles = c(1, 2)) {

  k <- num_circles + 1

  if (p < 3) {
    cli::cli_abort("p should be greater than or equal to 3.")
  }

  if (length(n) != k) {
    cli::cli_abort("n should contain exactly {.val {k}} values.")
  }

  if (length(scale_circles) != num_circles) {
    cli::cli_abort("scale_circles should contain exactly {.val {num_circles}} values.")
  }

  if (any(n < 0)) {
    cli::cli_abort("Values in n should be positive.")
  }

  ## To generate data
  df <- gen_multicluster(n = n, p = p, k = k,
                         loc = matrix(rep(rep(0, p), k),
                                      nrow = k, byrow = TRUE),
                         scale = c(scale_circles, 0.1),
                         shape = c(rep( "circle", num_circles), "gaussian"),
                         rotation = NULL,
                         is_bkg = FALSE)

  return(df)
}


#' Generate Concentric Curvycycles with a Gaussian Cluster in High Dimensions
#'
#' This function generates a dataset consisting of multiple circular clusters
#' together with a single Gaussian cluster in a \eqn{p}-dimensional space.
#' The curvycycle are placed concentrically at the origin with varying scales,
#' while the Gaussian cluster serves as an additional background or center cluster.
#'
#' @param n An integer vector of length \code{num_curvycycle + 1}, giving the
#'   number of points in each cluster (curvycycle first, followed by the Gaussian).
#'   Default is \code{c(200, 100, 100)}.
#' @param p Integer, the dimensionality of the embedding space. Must be at least 3.
#'   Default is \code{4}.
#' @param num_curvycycle Integer, the number of circular clusters to generate.
#'   Default is \code{2}.
#' @param scale_curvycycle Numeric vector of length \code{num_curvycycle}, giving
#'   the scale (radius) of each circular cluster. Default is \code{c(1, 2)}.
#'
#' @return A data frame (or tibble, depending on \code{gen_multicluster()})
#'   containing the generated dataset with cluster assignments.
#'
#' @examples
#' # Two curvycycle (radii 1 and 2) plus one Gaussian cluster in 4-D
#' gaucurvycycle <- make_gaucurvycycle()
#'
#'
#' @export
make_gaucurvycycle <- function(n = c(200, 100, 100), p = 4, num_curvycycle = 2, scale_curvycycle = c(1, 2)) {

  k <- num_curvycycle + 1

  if (p < 3) {
    cli::cli_abort("p should be greater than or equal to 3.")
  }

  if (length(n) != k) {
    cli::cli_abort("n should contain exactly {.val {k}} values.")
  }

  if (length(scale_curvycycle) != num_curvycycle) {
    cli::cli_abort("scale_curvycycle should contain exactly {.val {num_curvycycle}} values.")
  }

  if (any(n < 0)) {
    cli::cli_abort("Values in n should be positive.")
  }

  ## To generate data
  df <- gen_multicluster(n = n, p = p, k = k,
                         loc = matrix(rep(rep(0, p), k),
                                      nrow = k, byrow = TRUE),
                         scale = c(scale_curvycycle, 0.1),
                         shape = c(rep( "curvycycle", num_curvycycle), "gaussian"),
                         rotation = NULL,
                         is_bkg = FALSE)

  return(df)
}

#' Generate a Single Grid Cluster in High Dimensions
#'
#' This function generates a dataset consisting of one grid-like cluster
#' (a structured cube grid) in 2D, with optional Gaussian noise dimensions
#' added to extend the dataset into higher dimensions.
#'
#' @param n Integer, the number of points in the grid cluster. Must be positive.
#'   Default is \code{500}.
#'
#'
#' @return A tibble containing the generated dataset with columns:
#'   \itemize{
#'     \item \code{x1, x2, x3, x4} — coordinates of the data points.
#'     \item \code{cluster} — cluster assignment (always 1 for the grid).
#'   }
#'
#'
#' @examples
#' # Default: 500 points, 4D space (grid in 2D + 2 noise dimensions)
#' onegrid <- make_onegrid()
#'
#'
#' @export
make_onegrid <- function(n = 500){

  if (length(n) != 1) {
    cli::cli_abort("n should contain exactly one values.")
  }

  if (any(n < 0)) {
    cli::cli_abort("Values in n should be positive.")
  }

  ## To generate data
  df <- gen_multicluster(n = n, p = 2, k = 1,
                         loc = NULL,
                         scale = c(1),
                         shape = c("gridcube"),
                         rotation = NULL,
                         is_bkg = FALSE)


  noise_df <- gen_noisedims(n = NROW(df), p = 2, m = rep(0, 2), s = rep(0.01, 2))
  names(noise_df) <- paste0("x", 3:4)

  df <- cbind(df, noise_df)

  df <- df |>
    dplyr::select(dplyr::starts_with("x"), "cluster")

  return(df)

}

#' Generate Two Overlapping Grid Clusters in High Dimensions
#'
#' This function generates a dataset consisting of two overlapping
#' grid-like clusters in a 2D space, with optional noise dimensions
#' added to reach higher-dimensional spaces. The overlap is controlled
#' by scaling factors for the grids.
#'
#' @param n A numeric vector of length 2 specifying the number of points
#'   in each grid cluster.
#'
#' @return A tibble with \code{n[1] + n[2]} rows and 5 columns:
#'   \itemize{
#'     \item \code{x1,x2, x3, x4} — coordinates of the generated points.
#'     \item \code{cluster} — cluster membership label.
#'   }
#'
#'
#' @examples
#' # Generate two overlapping grid clusters in 4-D
#' df <- make_twogrid_overlap()
#'
#' @export
make_twogrid_overlap <- function(n = c(500, 500)){

  if (length(n) != 2) {
    cli::cli_abort("n should contain exactly one values.")
  }

  if (any(n < 0)) {
    cli::cli_abort("Values in n should be positive.")
  }

  # locations for the two grids (shifted along x1 and x2)
  loc <- matrix(0, nrow = 2, ncol = 2)
  loc[1, 1] <- 0         # first grid at origin
  loc[1, 2] <- 0
  loc[2, 1] <- 0.6     # second grid shifted in x1
  loc[2, 2] <- 0.6     # and shifted in x2

  ## To generate data
  df <- gen_multicluster(n = n, k = 2,
                         loc = loc,
                         scale = rep(1, 2),
                         shape = rep("gridcube", 2),
                         rotation = NULL,
                         is_bkg = FALSE,
                         p = 2)

  noise_df <- gen_noisedims(n = NROW(df), p = 2, m = rep(0, 2), s = rep(0.01, 2))
  names(noise_df) <- paste0("x", 3:4)

  df <- cbind(df, noise_df)

  df <- df |>
    dplyr::select(dplyr::starts_with("x"), "cluster")


  return(df)

}

#' Generate Two Shifted Grid Clusters in High Dimensions
#'
#' This function generates two grid-shaped clusters in a 2D space, where one grid
#' is shifted relative to the other. Optionally, additional noise dimensions
#' can be added to embed the structure in a higher-dimensional space.
#'
#' @param n A numeric vector of length 2 specifying the number of points in each cluster.
#'   Default is \code{c(500, 500)}.
#'
#'
#' @return A tibble with \code{n[1] + n[2]} rows and 5 columns:
#' \itemize{
#'   \item \code{x1, x2, x3, x4}: Numeric coordinates of the points.
#'   \item \code{cluster}: Cluster membership label (factor with 2 levels).
#' }
#'
#' @examples
#' # Generate 2 shifted grid clusters in 4-D
#' make_twogrid_shift <- make_twogrid_shift()
#'
#'
#' @export
make_twogrid_shift <- function(n = c(500, 500)){

  if (length(n) != 2) {
    cli::cli_abort("n should contain exactly one values.")
  }

  if (any(n < 0)) {
    cli::cli_abort("Values in n should be positive.")
  }

  # locations for the two grids (shifted along x1 and x2)
  loc <- matrix(0, nrow = 2, ncol = 2)
  loc[1, 1] <- 0         # first grid at origin
  loc[1, 2] <- 0
  loc[2, 1] <- 2     # second grid shifted in x1
  loc[2, 2] <- 3     # and shifted in x2

  ## To generate data
  df <- gen_multicluster(n = n, k = 2,
                         loc = loc,
                         scale = rep(1, 2),
                         shape = rep("gridcube", 2),
                         rotation = NULL,
                         is_bkg = FALSE,
                         p = 2)

  noise_df <- gen_noisedims(n = NROW(df), p = 2, m = rep(0, 2), s = rep(0.01, 2))
  names(noise_df) <- paste0("x", 3:4)

  df <- cbind(df, noise_df)

  df <- df |>
    dplyr::select(dplyr::starts_with("x"), "cluster")


  return(df)

}


#' Generate Parallel Multi-Shape Clusters
#'
#' This function generates synthetic high-dimensional data consisting of
#' \eqn{k} clusters of a specified shape (e.g., crescents), arranged in
#' parallel along alternating dimensions. The first cluster is shifted
#' along the first dimension, the second along the third dimension,
#' the third along the first dimension again, and so on.
#'
#' @param n A numeric vector of length \eqn{k}, specifying the number of
#'   observations in each cluster. All values must be positive.
#' @param k Integer. Number of clusters to generate. Must be greater than 1.
#' @param shift Numeric. The distance between cluster centers along the
#'   alternating dimensions (default is `0.4`).
#' @param shape Character string. Shape of the clusters to generate (e.g.,
#'   `"crescent"`, `"gridcube"`, etc.). Must be a single value.
#'
#'
#' @return A tibble containing \eqn{\sum n} rows and \eqn{p} columns, with
#'   the generated features (`x1, x2, x3, x4`) and a `cluster` label.
#'
#' @examples
#' # Generate 2 crescent-shaped clusters in 4D
#' twocrescent <- make_shape_para(n = c(500, 300), k = 2, shape = "crescent")
#'
#'
#' @export
make_shape_para <- function(n = c(500, 300), k = 2, shift = 1, shape = "crescent") {


  if (k < 1) {
    cli::cli_abort("k should be greater than 1.")
  }

  if (length(shape) != 1) {
    cli::cli_abort("shape should contain exactly one value.")
  }

  if (length(n) != k) {
    cli::cli_abort("n should contain exactly {.val {k}} values.")
  }

  if (any(n < 0)) {
    cli::cli_abort("Values in n should be positive.")
  }

  ## Construct location matrix
  loc <- matrix(0, nrow = k, ncol = 2)
  for (i in seq_len(k)) {
    if (i %% 2 == 1) {
      loc[i, 1] <- shift * ((i - 1) %/% 2 + 1)  # shift along x1
    } else {
      loc[i, 2] <- shift * (i %/% 2)            # shift along x2
    }
  }

  ## To generate data
  df <- gen_multicluster(n = n, k = k,
                         loc = t(loc),
                         scale = rep(1, k),
                         shape = rep(shape, k),
                         rotation = NULL,
                         is_bkg = FALSE)

  noise_df <- gen_noisedims(n = NROW(df), p = 2, m = rep(0, 2), s = rep(0.1, 2))
  names(noise_df) <- paste0("x", NCOL(df):(NCOL(df) + 1))

  df <- cbind(df, noise_df)

  df <- df |>
    dplyr::select(dplyr::starts_with("x"), "cluster")

  return(df)

}
