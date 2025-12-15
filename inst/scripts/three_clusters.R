#' Generate a Three-Cluster Dataset in High Dimensions
#'
#' This function generates a dataset consisting of three distinct clusters
#' in a 4-dimensional space. Each cluster is generated with a specified
#' shape and location using the underlying `gen_multicluster()` function.
#'
#' @param n An integer vector of length 3 specifying the number of points in
#'   each cluster. Default is \code{c(700, 300, 500)}.
#'
#'
#' @return A data frame (or tibble, depending on `gen_multicluster()`)
#'   containing the generated dataset with cluster assignments.
#'
#' @examples
#' # Generate default three clusters in 4-D
#' three_clust_01 <- make_three_clust_01()
#'
#' @export
make_three_clust_01 <- function(n = c(700, 300, 500)) {

  ## To generate data
  df <- gen_multicluster(n = n, k = 3,
                         loc = matrix(c(
                           0, 0, 0, 0,
                           5, 0, 2, 0,
                           3, 4, 10, 7
                         ), nrow = 3, byrow = TRUE) * 0.25,
                         scale = c(2, 1.5, 0.5),
                         shape = c("quadratic", "cone", "gaussian"),
                         rotation = NULL,
                         is_bkg = FALSE)

  return(df)

}

#' Generate a Three-Cluster Dataset in High Dimensions
#'
#' This function generates a dataset consisting of three distinct clusters
#' in a 4-dimensional space. Each cluster is generated with a specified
#' shape and location using the underlying `gen_multicluster()` function.
#'
#' @param n An integer vector of length 3 specifying the number of points in
#'   each cluster. Default is \code{c(700, 300, 500)}.
#'
#'
#' @return A data frame (or tibble, depending on `gen_multicluster()`)
#'   containing the generated dataset with cluster assignments.
#'
#' @examples
#' # Generate default three clusters in 4-D
#' three_clust_02 <- make_three_clust_02()
#'
#' @export
make_three_clust_02 <- function(n = c(700, 300, 500)) {

  ## To generate data
  df <- gen_multicluster(n = n, k = 3,
                         loc = matrix(c(
                           0, 0, 0, 0,
                           5, 0, 2, 0,
                           3, 4, 10, 7
                         ), nrow = 3, byrow = TRUE) * 0.25,
                         scale = c(2, 1.5, 0.5),
                         shape = c("crescent", "pyrrect", "unifcube"),
                         rotation = NULL,
                         is_bkg = FALSE)

  return(df)

}

#' Generate a Three-Cluster Dataset in High Dimensions
#'
#' This function generates a dataset consisting of three distinct clusters
#' in a 4-dimensional space. Each cluster is generated with a specified
#' shape and location using the underlying `gen_multicluster()` function.
#'
#' @param n An integer vector of length 3 specifying the number of points in
#'   each cluster. Default is \code{c(700, 300, 500)}.
#'
#'
#' @return A data frame (or tibble, depending on `gen_multicluster()`)
#'   containing the generated dataset with cluster assignments.
#'
#' @examples
#' # Generate default three clusters in 4-D
#' three_clust_03 <- make_three_clust_03()
#'
#' @export
make_three_clust_03 <- function(n = c(700, 300, 500)) {

  ## To generate data
  df <- gen_multicluster(n = n, k = 3,
                         loc = matrix(c(
                           0, 0, 0, 0,
                           5, 0, 2, 0,
                           3, 4, 10, 7
                         ), nrow = 3, byrow = TRUE) * 0.25,
                         scale = c(2, 1.5, 0.5),
                         shape = c("curvycylinder", "pyrtri", "hemisphere"),
                         rotation = NULL,
                         is_bkg = FALSE)

  return(df)

}

#' Generate a Three-Cluster Dataset in High Dimensions
#'
#' This function generates a dataset consisting of three distinct clusters
#' in a 4-dimensional space. Each cluster is generated with a specified
#' shape and location using the underlying `gen_multicluster()` function.
#'
#' @param n An integer vector of length 3 specifying the number of points in
#'   each cluster. Default is \code{c(700, 300, 500)}.
#'
#'
#' @return A data frame (or tibble, depending on `gen_multicluster()`)
#'   containing the generated dataset with cluster assignments.
#'
#' @examples
#' # Generate default three clusters in 4-D
#' three_clust_04 <- make_three_clust_04()
#'
#' @export
make_three_clust_04 <- function(n = c(700, 300, 500)) {

  ## To generate data
  df <- gen_multicluster(n = n, k = 3,
                         loc = matrix(c(
                           0, 0, 0, 0,
                           5, 0, 2, 0,
                           3, 4, 10, 7
                         ), nrow = 3, byrow = TRUE) * 0.25,
                         scale = c(2, 1.5, 0.5),
                         shape = c("sphericalspiral", "pyrtri", "unifsphere"),
                         rotation = NULL,
                         is_bkg = FALSE)

  return(df)

}

#' Generate a Three-Cluster Dataset in High Dimensions
#'
#' This function generates a dataset consisting of three distinct clusters
#' in a 4-dimensional space. Each cluster is generated with a specified
#' shape and location using the underlying `gen_multicluster()` function.
#'
#' @param n An integer vector of length 3 specifying the number of points in
#'   each cluster. Default is \code{c(700, 300, 500)}.
#'
#'
#' @return A data frame (or tibble, depending on `gen_multicluster()`)
#'   containing the generated dataset with cluster assignments.
#'
#' @examples
#' # Generate default three clusters in 4-D
#' three_clust_05 <- make_three_clust_05()
#'
#' @export
make_three_clust_05 <- function(n = c(700, 300, 500)) {

  ## To generate data
  df <- gen_multicluster(n = n, k = 3,
                         loc = matrix(c(
                           0, 0, 0, 0,
                           5, 0, 2, 0,
                           3, 4, 10, 7
                         ), nrow = 3, byrow = TRUE) * 0.25,
                         scale = c(1, 2, 0.5),
                         shape = c("helicalspiral", "pyrstar", "hemisphere"),
                         rotation = NULL,
                         is_bkg = FALSE)

  return(df)

}


#' Generate a Three-Cluster Dataset in High Dimensions
#'
#' This function generates a dataset consisting of three distinct clusters
#' in a 4-dimensional space. Each cluster is generated with a specified
#' shape and location using the underlying `gen_multicluster()` function.
#'
#' @param n An integer vector of length 3 specifying the number of points in
#'   each cluster. Default is \code{c(700, 300, 500)}.
#'
#'
#' @return A data frame (or tibble, depending on `gen_multicluster()`)
#'   containing the generated dataset with cluster assignments.
#'
#' @examples
#' # Generate default three clusters in 4-D
#' three_clust_06 <- make_three_clust_06()
#'
#' @export
make_three_clust_06 <- function(n = c(700, 300, 500)) {

  ## To generate data
  df <- gen_multicluster(n = n, k = 3,
                         loc = matrix(c(
                           0, 0, 0, 0,
                           5, 0, 2, 0,
                           3, 4, 10, 7
                         ), nrow = 3, byrow = TRUE) * 0.25,
                         scale = c(2, 1, 0.5),
                         shape = c("conicspiral", "cone", "hemisphere"),
                         rotation = NULL,
                         is_bkg = FALSE)

  return(df)

}

#' Generate a Three-Cluster Dataset in High Dimensions
#'
#' This function generates a dataset consisting of three distinct clusters
#' in a 4-dimensional space. Each cluster is generated with a specified
#' shape and location using the underlying `gen_multicluster()` function.
#'
#' @param n An integer vector of length 3 specifying the number of points in
#'   each cluster. Default is \code{c(700, 300, 500)}.
#'
#'
#' @return A data frame (or tibble, depending on `gen_multicluster()`)
#'   containing the generated dataset with cluster assignments.
#'
#' @examples
#' # Generate default three clusters in 4-D
#' three_clust_07 <- make_three_clust_07()
#'
#' @export
make_three_clust_07 <- function(n = c(700, 300, 500)) {

  ## To generate data
  df <- gen_multicluster(n = n, k = 3,
                         loc = matrix(c(
                           0, 0, 0, 0,
                           5, 0, 2, 0,
                           3, 4, 10, 7
                         ), nrow = 3, byrow = TRUE) * 0.2,
                         scale = c(2, 1, 0.5),
                         shape = c("nonlinear", "pyrrect", "gaussian"),
                         rotation = NULL,
                         is_bkg = FALSE)

  return(df)

}

#' Generate a Three-Cluster Dataset in High Dimensions
#'
#' This function generates a dataset consisting of three distinct clusters
#' in a 4-dimensional space. Each cluster is generated with a specified
#' shape and location using the underlying `gen_multicluster()` function.
#'
#' @param n An integer vector of length 3 specifying the number of points in
#'   each cluster. Default is \code{c(700, 300, 500)}.
#'
#'
#' @return A data frame (or tibble, depending on `gen_multicluster()`)
#'   containing the generated dataset with cluster assignments.
#'
#' @examples
#' # Generate default three clusters in 4-D
#' three_clust_08 <- make_three_clust_08()
#'
#' @export
make_three_clust_08 <- function(n = c(700, 300, 500)) {

  ## To generate data
  df <- gen_multicluster(n = n, k = 3,
                         loc = matrix(c(
                           0, 0, 0, 0,
                           5, 0, 2, 0,
                           3, 4, 10, 7
                         ), nrow = 3, byrow = TRUE) * 0.25,
                         scale = c(2, 1.5, 0.5),
                         shape = c("crescent", "cone", "hemisphere"),
                         rotation = NULL,
                         is_bkg = FALSE)

  return(df)

}

#' Generate a Three-Cluster Dataset in High Dimensions
#'
#' This function generates a dataset consisting of three distinct clusters
#' in a 4-dimensional space. Each cluster is generated with a specified
#' shape and location using the underlying `gen_multicluster()` function.
#'
#' @param n An integer vector of length 3 specifying the number of points in
#'   each cluster. Default is \code{c(700, 300, 500)}.
#'
#'
#' @return A data frame (or tibble, depending on `gen_multicluster()`)
#'   containing the generated dataset with cluster assignments.
#'
#' @examples
#' # Generate default three clusters in 4-D
#' three_clust_09 <- make_three_clust_09()
#'
#' @export
make_three_clust_09 <- function(n = c(700, 300, 500)) {

  ## To generate data
  df <- gen_multicluster(n = n, k = 3,
                         loc = matrix(c(
                           0, 0, 0, 0,
                           5, 0, 2, 0,
                           3, 4, 10, 7
                         ), nrow = 3, byrow = TRUE) * 0.25,
                         scale = c(2, 1.5, 0.5),
                         shape = c("curvycylinder", "pyrstar", "hemisphere"),
                         rotation = NULL,
                         is_bkg = FALSE)

  return(df)

}

#' Generate a Three-Cluster Dataset in High Dimensions
#'
#' This function generates a dataset consisting of three distinct clusters
#' in a 4-dimensional space. Each cluster is generated with a specified
#' shape and location using the underlying `gen_multicluster()` function.
#'
#' @param n An integer vector of length 3 specifying the number of points in
#'   each cluster. Default is \code{c(700, 300, 500)}.
#'
#'
#' @return A data frame (or tibble, depending on `gen_multicluster()`)
#'   containing the generated dataset with cluster assignments.
#'
#' @examples
#' # Generate default three clusters in 4-D
#' three_clust_10 <- make_three_clust_10()
#'
#' @export
make_three_clust_10 <- function(n = c(700, 300, 500)) {

  ## To generate data
  df <- gen_multicluster(n = n, k = 3,
                         loc = matrix(c(
                           0, 0, 0, 0,
                           5, 0, 2, 0,
                           3, 4, 10, 7
                         ), nrow = 3, byrow = TRUE) * 0.25,
                         scale = c(2, 1.5, 0.5),
                         shape = c("sphericalspiral", "pyrstar", "gaussian"),
                         rotation = NULL,
                         is_bkg = FALSE)

  return(df)

}

#' Generate a Three-Cluster Dataset in High Dimensions
#'
#' This function generates a dataset consisting of three distinct clusters
#' in a 4-dimensional space. Each cluster is generated with a specified
#' shape and location using the underlying `gen_multicluster()` function.
#'
#' @param n An integer vector of length 3 specifying the number of points in
#'   each cluster. Default is \code{c(700, 300, 500)}.
#'
#'
#' @return A data frame (or tibble, depending on `gen_multicluster()`)
#'   containing the generated dataset with cluster assignments.
#'
#' @examples
#' # Generate default three clusters in 4-D
#' three_clust_11 <- make_three_clust_11()
#'
#' @export
make_three_clust_11 <- function(n = c(700, 300, 500)) {

  ## To generate data
  df <- gen_multicluster(n = n, k = 3,
                         loc = matrix(c(
                           0, 0, 0, 0,
                           5, 0, 2, 0,
                           3, 4, 10, 7
                         ), nrow = 3, byrow = TRUE) * 0.25,
                         scale = c(2, 1.5, 0.5),
                         shape = c("helicalspiral", "pyrtri", "unifsphere"),
                         rotation = NULL,
                         is_bkg = FALSE)

  return(df)

}

#' Generate a Three-Cluster Dataset in High Dimensions
#'
#' This function generates a dataset consisting of three distinct clusters
#' in a 4-dimensional space. Each cluster is generated with a specified
#' shape and location using the underlying `gen_multicluster()` function.
#'
#' @param n An integer vector of length 3 specifying the number of points in
#'   each cluster. Default is \code{c(700, 300, 500)}.
#'
#'
#' @return A data frame (or tibble, depending on `gen_multicluster()`)
#'   containing the generated dataset with cluster assignments.
#'
#' @examples
#' # Generate default three clusters in 4-D
#' three_clust_12 <- make_three_clust_12()
#'
#' @export
make_three_clust_12 <- function(n = c(700, 300, 500)) {

  ## To generate data
  df <- gen_multicluster(n = n, k = 3,
                         loc = matrix(c(
                           0, 0, 0, 0,
                           5, 0, 2, 0,
                           3, 4, 10, 7
                         ), nrow = 3, byrow = TRUE) * 0.25,
                         scale = c(2, 1, 0.5),
                         shape = c("conicspiral", "pyrstar", "unifcube"),
                         rotation = NULL,
                         is_bkg = FALSE)

  return(df)

}

#' Generate a Three-Cluster Dataset in High Dimensions
#'
#' This function generates a dataset consisting of three distinct clusters
#' in a 4-dimensional space. Each cluster is generated with a specified
#' shape and location using the underlying `gen_multicluster()` function.
#'
#' @param n An integer vector of length 3 specifying the number of points in
#'   each cluster. Default is \code{c(700, 300, 500)}.
#'
#'
#' @return A data frame (or tibble, depending on `gen_multicluster()`)
#'   containing the generated dataset with cluster assignments.
#'
#' @examples
#' # Generate default three clusters in 4-D
#' three_clust_13 <- make_three_clust_13()
#'
#' @export
make_three_clust_13 <- function(n = c(700, 300, 500)) {

  ## To generate data
  df <- gen_multicluster(n = n, k = 3,
                         loc = matrix(c(
                           0, 0, 0, 0,
                           5, 0, 2, 0,
                           3, 4, 10, 7
                         ), nrow = 3, byrow = TRUE) * 0.25,
                         scale = c(2, 1.5, 0.5),
                         shape = c("conicspiral", "pyrtri", "unifcube"),
                         rotation = NULL,
                         is_bkg = FALSE)

  return(df)

}

#' Generate a Three-Cluster Dataset in High Dimensions
#'
#' This function generates a dataset consisting of three distinct clusters
#' in a 4-dimensional space. Each cluster is generated with a specified
#' shape and location using the underlying `gen_multicluster()` function.
#'
#' @param n An integer vector of length 3 specifying the number of points in
#'   each cluster. Default is \code{c(700, 300, 500)}.
#'
#'
#' @return A data frame (or tibble, depending on `gen_multicluster()`)
#'   containing the generated dataset with cluster assignments.
#'
#' @examples
#' # Generate default three clusters in 4-D
#' three_clust_14 <- make_three_clust_14()
#'
#' @export
make_three_clust_14 <- function(n = c(700, 300, 500)) {

  ## To generate data
  df <- gen_multicluster(n = n, k = 3,
                         loc = matrix(c(
                           0, 0, 0, 0,
                           5, 0, 2, 0,
                           3, 4, 10, 7
                         ), nrow = 3, byrow = TRUE) * 0.25,
                         scale = c(2, 1.5, 0.5),
                         shape = c("conicspiral", "cone", "unifcube"),
                         rotation = NULL,
                         is_bkg = FALSE)

  return(df)

}

#' Generate a Three-Cluster Dataset in High Dimensions
#'
#' This function generates a dataset consisting of three distinct clusters
#' in a 4-dimensional space. Each cluster is generated with a specified
#' shape and location using the underlying `gen_multicluster()` function.
#'
#' @param n An integer vector of length 3 specifying the number of points in
#'   each cluster. Default is \code{c(700, 300, 500)}.
#'
#'
#' @return A data frame (or tibble, depending on `gen_multicluster()`)
#'   containing the generated dataset with cluster assignments.
#'
#' @examples
#' # Generate default three clusters in 4-D
#' three_clust_15 <- make_three_clust_15()
#'
#' @export
make_three_clust_15 <- function(n = c(700, 300, 500)) {

  ## To generate data
  df <- gen_multicluster(n = n, k = 3,
                         loc = matrix(c(
                           0, 0, 0, 0,
                           5, 0, 2, 0,
                           3, 4, 10, 7
                         ), nrow = 3, byrow = TRUE) * 0.25,
                         scale = c(2, 1.5, 0.5),
                         shape = c("nonlinear", "pyrrect", "unifcube"),
                         rotation = NULL,
                         is_bkg = FALSE)

  return(df)

}

#' Generate a Three-Cluster Dataset in High Dimensions
#'
#' This function generates a dataset consisting of three distinct clusters
#' in a 4-dimensional space. Each cluster is generated with a specified
#' shape and location using the underlying `gen_multicluster()` function.
#'
#' @param n An integer vector of length 3 specifying the number of points in
#'   each cluster. Default is \code{c(700, 300, 500)}.
#'
#'
#' @return A data frame (or tibble, depending on `gen_multicluster()`)
#'   containing the generated dataset with cluster assignments.
#'
#' @examples
#' # Generate default three clusters in 4-D
#' three_clust_16 <- make_three_clust_16()
#'
#' @export
make_three_clust_16 <- function(n = c(700, 300, 500)) {

  ## To generate data
  df <- gen_multicluster(n = n, k = 3,
                         loc = matrix(c(
                           0, 0, 0, 0,
                           5, 0, 2, 0,
                           3, 4, 10, 7
                         ), nrow = 3, byrow = TRUE) * 0.25,
                         scale = c(2, 1.5, 0.5),
                         shape = c("crescent", "pyrtri", "unifsphere"),
                         rotation = NULL,
                         is_bkg = FALSE)

  return(df)

}

#' Generate a Three-Cluster Dataset in High Dimensions
#'
#' This function generates a dataset consisting of three distinct clusters
#' in a 4-dimensional space. Each cluster is generated with a specified
#' shape and location using the underlying `gen_multicluster()` function.
#'
#' @param n An integer vector of length 3 specifying the number of points in
#'   each cluster. Default is \code{c(700, 300, 500)}.
#'
#'
#' @return A data frame (or tibble, depending on `gen_multicluster()`)
#'   containing the generated dataset with cluster assignments.
#'
#' @examples
#' # Generate default three clusters in 4-D
#' three_clust_17 <- make_three_clust_17()
#'
#' @export
make_three_clust_17 <- function(n = c(700, 300, 500)) {

  ## To generate data
  df <- gen_multicluster(n = n, k = 3,
                         loc = matrix(c(
                           0, 0, 0, 0,
                           5, 0, 2, 0,
                           3, 4, 10, 7
                         ), nrow = 3, byrow = TRUE) * 0.25,
                         scale = c(2, 1.5, 0.5),
                         shape = c("curvycylinder", "cone", "unifcube"),
                         rotation = NULL,
                         is_bkg = FALSE)

  return(df)

}

#' Generate a Three-Cluster Dataset in High Dimensions
#'
#' This function generates a dataset consisting of three distinct clusters
#' in a 4-dimensional space. Each cluster is generated with a specified
#' shape and location using the underlying `gen_multicluster()` function.
#'
#' @param n An integer vector of length 3 specifying the number of points in
#'   each cluster. Default is \code{c(700, 300, 500)}.
#'
#'
#' @return A data frame (or tibble, depending on `gen_multicluster()`)
#'   containing the generated dataset with cluster assignments.
#'
#' @examples
#' # Generate default three clusters in 4-D
#' three_clust_18 <- make_three_clust_18()
#'
#' @export
make_three_clust_18 <- function(n = c(700, 300, 500)) {

  ## To generate data
  df <- gen_multicluster(n = n, k = 3,
                         loc = matrix(c(
                           0, 0, 0, 0,
                           5, 0, 2, 0,
                           3, 4, 10, 7
                         ), nrow = 3, byrow = TRUE) * 0.25,
                         scale = c(2, 1.5, 0.5),
                         shape = c("sphericalspiral", "cone", "unifcube"),
                         rotation = NULL,
                         is_bkg = FALSE)

  return(df)

}

#' Generate a Three-Cluster Dataset in High Dimensions
#'
#' This function generates a dataset consisting of three distinct clusters
#' in a 4-dimensional space. Each cluster is generated with a specified
#' shape and location using the underlying `gen_multicluster()` function.
#'
#' @param n An integer vector of length 3 specifying the number of points in
#'   each cluster. Default is \code{c(700, 300, 500)}.
#'
#'
#' @return A data frame (or tibble, depending on `gen_multicluster()`)
#'   containing the generated dataset with cluster assignments.
#'
#' @examples
#' # Generate default three clusters in 4-D
#' three_clust_19 <- make_three_clust_19()
#'
#' @export
make_three_clust_19 <- function(n = c(700, 300, 500)) {

  ## To generate data
  df <- gen_multicluster(n = n, k = 3,
                         loc = matrix(c(
                           0, 0, 0, 0,
                           5, 0, 2, 0,
                           3, 4, 10, 7
                         ), nrow = 3, byrow = TRUE) * 0.25,
                         scale = c(2, 1.5, 0.5),
                         shape = c("helicalspiral", "pyrrect", "hemisphere"),
                         rotation = NULL,
                         is_bkg = FALSE)

  return(df)

}

#' Generate a Three-Cluster Dataset in High Dimensions
#'
#' This function generates a dataset consisting of three distinct clusters
#' in a 4-dimensional space. Each cluster is generated with a specified
#' shape and location using the underlying `gen_multicluster()` function.
#'
#' @param n An integer vector of length 3 specifying the number of points in
#'   each cluster. Default is \code{c(700, 300, 500)}.
#'
#'
#' @return A data frame (or tibble, depending on `gen_multicluster()`)
#'   containing the generated dataset with cluster assignments.
#'
#' @examples
#' # Generate default three clusters in 4-D
#' three_clust_20 <- make_three_clust_20()
#'
#' @export
make_three_clust_20 <- function(n = c(700, 300, 500)) {

  ## To generate data
  df <- gen_multicluster(n = n, k = 3,
                         loc = matrix(c(
                           0, 0, 0, 0,
                           5, 0, 0, 0,
                           3, 4, 10, 7
                         ), nrow = 3, byrow = TRUE) * 0.25,
                         scale = c(2, 1.5, 0.5),
                         shape = c("conicspiral", "pyrrect", "gaussian"),
                         rotation = NULL,
                         is_bkg = FALSE)

  return(df)

}
