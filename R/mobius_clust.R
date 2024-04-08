#' Generate a Single Row for a 5-D Mobius Strip
#'
#' This function generates a single row of data representing a point on a 5-dimensional Mobius strip.
#'
#' @return A vector containing the coordinates of the point on the Mobius strip.
#' @export
#'
#' @examples
#' mobius_row <- mobius_5d_row()
mobius_5d_row <- function(){

  ##Generates Angles
  a <- stats::runif(1, min = 0, max = 2 * pi)
  a <- c(a, a / 2)

  ##Generates Small Radius
  radius <- c(1, stats::runif(1, min = -.4, max = .4))

  ##Generates Row of Data
  mobius <- c(
    (cos(a[2]) * radius[2] + radius[1]) * cos(a[1]),
    (cos(a[2]) * radius[2] + radius[1]) * sin(a[1]),
    sin(a[2]) * radius[2]
  )

  k <- stats::runif(1, min = 0, max = pi)
  ## Rot over x axis
  rot_1 <- matrix(
    c(
      0,
      cos(k),
      -sin(k),
      1, 0, 0, 0,
      sin(k),
      cos(k)
    ),
    ncol = 3, byrow = TRUE
  )
  ## Rot over z axis
  rot_2 <- matrix(
    c(
      cos(2 * k),
      -sin(2 * k),
      0,
      sin(2 * k),
      cos(2 * k),
      0, 0, 0, 1
    ),
    ncol = 3, byrow = TRUE
  )
  ## Trans perpendicular to z axis
  trans <- matrix(c(4 * cos(2 * k), 4 * sin(2 * k), 0), ncol = 1)

  mobius <- rot_2 %*% rot_1 %*% mobius + trans

  mobius
}

#' Generate a 5-D Mobius Strip
#'
#' This function generates a dataset representing a 5-dimensional Mobius strip.
#'
#' @param sample_size The number of points to generate for the Mobius strip.
#' @param num_noise_dims The number of additional noise dimensions to add to the data.
#' @param min_noise The minimum value for the noise dimensions.
#' @param max_noise The maximum value for the noise dimensions.
#' @return A matrix containing the generated Mobius strip.
#' @export
#'
#' @examples
#' mobius_data <- mobius_5d(sample_size = 100, num_noise_dims = 3,
#'                          min_noise = -0.05, max_noise = 0.05)
mobius_5d <- function(sample_size, num_noise_dims, min_noise, max_noise){

  df <- matrix(
    do.call(
      "rbind",
      as.list(
        replicate(sample_size, mobius_5d_row())
      )
    ),
    ncol = 3,
    byrow = TRUE
  )

  if (num_noise_dims != 0) {

    noise_mat <- gen_noise_dims(n = dim(df)[1], num_noise_dims = num_noise_dims,
                                min_noise = min_noise, max_noise = max_noise)
    df <- cbind(df, noise_mat)

    df

  } else {

    df

  }

}

#' Generate Mobius Cluster with Noise
#'
#' This function generates a dataset consisting of a mobius cluster with added noise.
#'
#' @param sample_size The total number of samples to generate.
#' @param num_noise_dims The number of additional noise dimensions to add to the data.
#' @param min_noise The minimum value for the noise dimensions.
#' @param max_noise The maximum value for the noise dimensions.
#' @return A matrix containing the mobius cluster with added noise.
#' @export
#'
#' @examples
#' mobius_cluster <- mobius_cluster_with_noise(sample_size = 200, num_noise_dims = 8,
#'                                             min_noise = -0.05, max_noise = 0.05)
mobius_cluster_with_noise <- function(sample_size, num_noise_dims, min_noise,
                                      max_noise) {

  df1 <- mobius_5d(sample_size = sample_size * 0.80, num_noise_dims = 0)

  if (num_noise_dims != 0) {

    noise_mat <- gen_noise_dims(n = dim(df1)[1], num_noise_dims = num_noise_dims,
                                min_noise = min_noise, max_noise = max_noise)
    df1 <- cbind(df1, noise_mat)

  }

  ## To add background noise
  df2 <- gen_bkg_noise(n = sample_size * 0.20, num_dims = NCOL(df1), mean = 0, sd = 0.3)
  df <- rbind(df1, df2)
  df

}
