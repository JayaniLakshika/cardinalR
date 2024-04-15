#' Generate points on a conic spiral in 3D space.
#'
#' This function generates points on a conic spiral in 3D space.
#'
#' @param a Final radius of the cone.
#' @param b Height of the object.
#' @param c Inner radius.
#' @param w Number of spirals.
#'
#' @return A matrix containing the generated points on the conic spiral.
#' @importFrom stats runif
#'
#' @examples
#' set.seed(20240412)
#' conic_spiral_3d_row(1, 2, 0.5, 3)
#'
#' @export
conic_spiral_3d_row <- function(a, b, c, w) {
  u <- stats::runif(1, min = 0, max = 2 * pi)
  v <- stats::runif(1, min = 0, max = 2 * pi)

  x <- a * (1 - v / (2 * pi)) * cos(w * v) * (1 + cos(u)) +
    c * cos(w * v)
  y <- a * (1 - v / (2 * pi)) * sin(w * v) * (1 + cos(u)) +
    c * sin(w * v)
  z <- (b * v + a * (1 - v / (2 * pi)) * sin(u)) / (2 * pi)
  return(cbind(x, y, z))
}

#' Generate data points along a conic spiral curve with optional noise.
#'
#' This function generates data points along a conic spiral curve with optional noise.
#'
#' @param n Total number of data points to generate.
#' @param num_noise Number of additional noise dimensions to add to the data.
#' @param min_n Minimum value for the noise added to the data.
#' @param max_n Maximum value for the noise added to the data.
#'
#' @return A matrix containing the generated data points with or without added noise.
#' @importFrom purrr reduce
#'
#' @examples
#' set.seed(20240412)
#' conic_spiral_3d(n = 100, num_noise = 2, min_n = -0.05, max_n = 0.05)
#'
#' @export
conic_spiral_3d <- function(n, num_noise, min_n, max_n) {
  if (n <= 0) {
    stop("Number of points should be a positive number.")
  }

  if (num_noise < 0) {
    stop("Number of noise dimensions should be a positive number.")
  }

  if (missing(n)) {
    stop("Missing n.")
  }

  if (missing(num_noise)) {
    stop("Missing num_noise.")
  }

  df_list <- lapply(1:n, function(i) {
    conic_spiral_3d_row(
      a = 0.2, b = 1, c = 0.1,
      w = 2
    )
  })
  df <- purrr::reduce(df_list, rbind)

  if (num_noise != 0) {
    if (missing(min_n)) {
      stop("Missing min_n.")
    }

    if (missing(max_n)) {
      stop("Missing max_n.")
    }

    noise_mat <- gen_noise_dims(
      n = dim(df)[1], num_noise = num_noise,
      min_n = min_n, max_n = max_n
    )
    df <- cbind(df, noise_mat)

    df
  } else {
    df
  }
}

#' Generate points on a Dini's surface.
#'
#' This function generates points on a Dini's surface.
#'
#' @param a Outer radius of the surface.
#' @param b Space between loops.
#'
#' @return A matrix containing the generated points on the surface.
#'
#' @examples
#' set.seed(20240412)
#' dini_surface_3d_row(a = 1, b = 1)
#'
#' @export
dini_surface_3d_row <- function(a = 1, b = 1) {
  u <- stats::runif(1, min = 0, max = 4 * pi)
  v <- stats::runif(1, min = 0.0000000001, max = 2)
  x <- a * cos(u) * sin(v)
  y <- a * sin(u) * sin(v)
  z <- a * (cos(v) + log(tan(v / 2))) + (b * u)
  return(cbind(x, y, z))
}

#' Generate points sampled from the Dini surface with optional noise.
#'
#' This function generates points sampled from the Dini surface along with optional noise.
#'
#' @param n Total number of data points to generate.
#' @param num_noise Number of additional noise dimensions to add to the data.
#' @param min_n Minimum value for the noise added to the data.
#' @param max_n Maximum value for the noise added to the data.
#'
#' @return A matrix containing the generated data points with or without added noise.
#' @importFrom purrr reduce
#'
#' @examples
#' set.seed(20240412)
#' dini_surface_3d(n = 100, num_noise = 2, min_n = -0.05, max_n = 0.05)
#'
#' @export
dini_surface_3d <- function(n, num_noise, min_n, max_n) {
  if (n <= 0) {
    stop("Number of points should be a positive number.")
  }

  if (num_noise < 0) {
    stop("Number of noise dimensions should be a positive number.")
  }

  if (missing(n)) {
    stop("Missing n.")
  }

  if (missing(num_noise)) {
    stop("Missing num_noise.")
  }

  df_list <- lapply(1:n, function(i) dini_surface_3d_row(a = 1, b = 1))
  df <- purrr::reduce(df_list, rbind)

  if (num_noise != 0) {
    if (missing(min_n)) {
      stop("Missing min_n.")
    }

    if (missing(max_n)) {
      stop("Missing max_n.")
    }

    noise_mat <- gen_noise_dims(
      n = dim(df)[1], num_noise = num_noise,
      min_n = min_n, max_n = max_n
    )
    df <- cbind(df, noise_mat)

    df
  } else {
    df
  }
}

#' Generate points on a Roman surface in 3D space.
#'
#' This function generates points on a Roman surface in 3D space.
#'
#' @param a Maximum radius of the object.
#'
#' @return A matrix containing the generated points on the Roman surface in 3D space.
#'
#' @examples
#' set.seed(20240412)
#' roman_surface_3d_row(a = 1)
#'
#' @export
roman_surface_3d_row <- function(a = 1) {
  u <- stats::runif(1, min = 0, max = pi)
  v <- stats::runif(1, min = 0, max = pi)
  x <- a^2 * cos(v) * cos(v) * sin(2 * u) / 2
  y <- a^2 * sin(u) * sin(2 * v) / 2
  z <- a^2 * cos(u) * sin(2 * v) / 2
  return(cbind(x, y, z))
}

#' Generate data points on a Roman surface with optional noise.
#'
#' This function generates data points on a Roman surface with optional noise.
#'
#' @param n Total number of data points to generate.
#' @param num_noise Number of additional noise dimensions to add to the data.
#' @param min_n Minimum value for the noise added to the data.
#' @param max_n Maximum value for the noise added to the data.
#'
#' @return A matrix containing the generated data points with or without added noise.
#' @importFrom purrr reduce
#'
#' @examples
#' set.seed(20240412)
#' roman_surface_3d(n = 100, num_noise = 2, min_n = -0.05, max_n = 0.05)
#'
#' @export
roman_surface_3d <- function(n, num_noise, min_n, max_n) {
  if (n <= 0) {
    stop("Number of points should be a positive number.")
  }

  if (num_noise < 0) {
    stop("Number of noise dimensions should be a positive number.")
  }

  if (missing(n)) {
    stop("Missing n.")
  }

  if (missing(num_noise)) {
    stop("Missing num_noise.")
  }

  df_list <- lapply(1:n, function(i) roman_surface_3d_row(a = 1))
  df <- purrr::reduce(df_list, rbind)

  if (num_noise != 0) {
    if (missing(min_n)) {
      stop("Missing min_n.")
    }

    if (missing(max_n)) {
      stop("Missing max_n.")
    }

    noise_mat <- gen_noise_dims(
      n = dim(df)[1], num_noise = num_noise,
      min_n = min_n, max_n = max_n
    )
    df <- cbind(df, noise_mat)

    df
  } else {
    df
  }
}

#' Generate a spiral dataset with optional noise.
#'
#' This function generates a dataset arranged in a spiral pattern with optional noise.
#'
#' @param n Total number of data points to generate.
#' @param num_dims Number of effective dimensions for each data point.
#' @param num_noise Number of additional noise dimensions to add to the data.
#' @param min_n Minimum value for the noise added to the data.
#' @param max_n Maximum value for the noise added to the data.
#'
#' @return A matrix containing the generated data points with or without added noise.
#' @importFrom purrr reduce
#'
#' @examples
#' set.seed(20240412)
#' spiral_3d(n = 100, num_dims = 10, num_noise = 2, min_n = -0.05, max_n = 0.05)
#'
#' @export
spiral_3d <- function(n, num_dims, num_noise, min_n, max_n) {
  if (n <= 0) {
    stop("Number of points should be a positive number.")
  }

  if (num_noise < 0) {
    stop("Number of noise dimensions should be a positive number.")
  }

  if (num_dims < 0) {
    stop("Number of effective dimensions should be a positive number.")
  }

  if (missing(n)) {
    stop("Missing n.")
  }

  if (missing(num_noise)) {
    stop("Missing num_noise.")
  }

  if (missing(num_dims)) {
    stop("Missing num_dims.")
  }

  u <- array(stats::runif(n = (n * 1), min = 0, max = 5), dim = c(n, 1))
  df <- array(cos(pi * u), dim = c(n, num_dims))
  y <- u * sin(pi * u)
  if (num_dims > 1) {
    for (i in 1:(num_dims - 1)) {
      df[, i] <- y * df[, i, drop = FALSE]^i
    }
  }
  df[, num_dims] <- u * df[, num_dims, drop = FALSE]

  if (num_noise != 0) {
    if (missing(min_n)) {
      stop("Missing min_n.")
    }

    if (missing(max_n)) {
      stop("Missing max_n.")
    }

    noise_mat <- gen_noise_dims(
      n = dim(df)[1], num_noise = num_noise,
      min_n = min_n, max_n = max_n
    )
    df <- cbind(df, noise_mat)

    df
  } else {
    df
  }
}

#' Generate a row of data points for a 3D torus.
#'
#' This function generates a row of data points for a 3D torus with given radii.
#'
#' @param radius A numeric vector containing the radii of the torus, from largest
#' to smallest.
#'
#' @return A vector representing a row of data points for the 3D torus.
#' @importFrom purrr reduce
#'
#' @examples
#' set.seed(20240412)
#' torus_3d_row(c(2, 1))
#'
#' @export
torus_3d_row <- function(radius) {
  ## Generates Angles
  t <- stats::runif(2, min = 0, max = 2 * pi)

  ## Generates Row of Data
  torus <- c(
    rep(cos(t[2]) * radius[2], 2),
    sin(t[2]) * radius[2]
  )

  torus[1] <- (torus[1] + radius[1]) * cos(t[1])
  torus[2] <- (torus[2] + radius[1]) * sin(t[1])

  torus
}

#' Generate a torus-shaped dataset with optional noise.
#'
#' This function generates a torus-shaped dataset along with optional noise.
#'
#' @param n Total number of data points to generate.
#' @param num_noise Number of additional noise dimensions to add to the data.
#' @param min_n Minimum value for the noise added to the data.
#' @param max_n Maximum value for the noise added to the data.
#'
#' @return A matrix containing the generated torus-shaped data points with or
#' without added noise.
#' @importFrom purrr reduce
#'
#' @examples
#' set.seed(20240412)
#' torus_3d(n = 100, num_noise = 2, min_n = -0.05, max_n = 0.05)
#'
#' @export
torus_3d <- function(n, num_noise, min_n, max_n) {
  if (n <= 0) {
    stop("Number of points should be a positive number.")
  }

  if (num_noise < 0) {
    stop("Number of noise dimensions should be a positive number.")
  }

  if (missing(n)) {
    stop("Missing n.")
  }

  if (missing(num_noise)) {
    stop("Missing num_noise.")
  }

  df_list <- lapply(1:n, function(i) torus_3d_row(radius = 2^(1:0)))
  df <- purrr::reduce(df_list, rbind)


  if (num_noise != 0) {
    if (missing(min_n)) {
      stop("Missing min_n.")
    }

    if (missing(max_n)) {
      stop("Missing max_n.")
    }

    noise_mat <- gen_noise_dims(
      n = dim(df)[1], num_noise = num_noise,
      min_n = min_n, max_n = max_n
    )
    df <- cbind(df, noise_mat)

    df
  } else {
    df
  }
}

#' Generate a 3D cube with optional noise.
#'
#' This function generates a 3D cube along with optional noise.
#'
#' @param num_dims Number of effective dimensions (default is 3 for a 3D cube).
#' @param num_noise Number of additional noise dimensions to add to the data.
#' @param min_n Minimum value for the noise added to the data.
#' @param max_n Maximum value for the noise added to the data.
#'
#' @return A list containing the generated data matrix and the sample size.
#' @importFrom purrr reduce
#'
#' @examples
#' set.seed(20240412)
#' cube_3d(num_dims = 3, num_noise = 2, min_n = -0.01, max_n = 0.01)
#'
#' @export
cube_3d <- function(num_dims, num_noise, min_n, max_n) {
  if (num_dims <= 0) {
    stop("Number of effective dimensions should be a positive number.")
  }

  if (num_noise < 0) {
    stop("Number of noise dimensions should be a positive number.")
  }

  if (missing(num_noise)) {
    stop("Missing num_noise.")
  }

  if (missing(num_dims)) {
    stop("Missing num_dims.")
  }

  df1 <- expand.grid(replicate(num_dims, (0:11) / 11, simplify = FALSE))
  df2 <- expand.grid(replicate(num_dims, c(0, 1), simplify = FALSE))

  df <- unique(rbind(as.matrix(df1), as.matrix(df2)))

  if (num_noise != 0) {
    if (missing(min_n)) {
      stop("Missing min_n.")
    }

    if (missing(max_n)) {
      stop("Missing max_n.")
    }

    noise_mat <- gen_noise_dims(
      n = dim(df)[1], num_noise = num_noise,
      min_n = min_n, max_n = max_n
    )
    df <- cbind(df, noise_mat)
  }

  return(list(df = df, n = NROW(df)))
}
