library(cardinalR)
set.seed(20240412)



## To generate data
three_clust_01 <- gen_multicluster(n = c(700, 300, 500), k = 3,
                                   loc = matrix(c(
                                     0, 0, 0, 0,
                                     5, 0, 2, 0,
                                     3, 4, 10, 7
                                   ), nrow = 3, byrow = TRUE) * 0.25,
                                   scale = c(2, 1.5, 0.5),
                                   shape = c("quadratic", "cone", "gaussian"),
                                   rotation = NULL,
                                   is_bkg = FALSE)

usethis::use_data(three_clust_01, overwrite = TRUE)




## To generate data
three_clust_02 <- gen_multicluster(n = c(700, 300, 500), k = 3,
                                   loc = matrix(c(
                                     0, 0, 0, 0,
                                     5, 0, 2, 0,
                                     3, 4, 10, 7
                                   ), nrow = 3, byrow = TRUE) * 0.25,
                                   scale = c(2, 1.5, 0.5),
                                   shape = c("crescent", "pyrrect", "unifcube"),
                                   rotation = NULL,
                                   is_bkg = FALSE)

three_clust_02 <- make_three_clust_02()
usethis::use_data(three_clust_02, overwrite = TRUE)



## To generate data
three_clust_03 <- gen_multicluster(n = c(700, 300, 500), k = 3,
                                   loc = matrix(c(
                                     0, 0, 0, 0,
                                     5, 0, 2, 0,
                                     3, 4, 10, 7
                                   ), nrow = 3, byrow = TRUE) * 0.25,
                                   scale = c(2, 1.5, 0.5),
                                   shape = c("curvycylinder", "pyrtri", "hemisphere"),
                                   rotation = NULL,
                                   is_bkg = FALSE)

three_clust_03 <- make_three_clust_03()
usethis::use_data(three_clust_03, overwrite = TRUE)



## To generate data
three_clust_04 <- gen_multicluster(n = c(700, 300, 500), k = 3,
                                   loc = matrix(c(
                                     0, 0, 0, 0,
                                     5, 0, 2, 0,
                                     3, 4, 10, 7
                                   ), nrow = 3, byrow = TRUE) * 0.25,
                                   scale = c(2, 1.5, 0.5),
                                   shape = c("sphericalspiral", "pyrtri", "unifsphere"),
                                   rotation = NULL,
                                   is_bkg = FALSE)

three_clust_04 <- make_three_clust_04()
usethis::use_data(three_clust_04, overwrite = TRUE)




## To generate data
three_clust_05 <- gen_multicluster(n = c(700, 300, 500), k = 3,
                                   loc = matrix(c(
                                     0, 0, 0, 0,
                                     5, 0, 2, 0,
                                     3, 4, 10, 7
                                   ), nrow = 3, byrow = TRUE) * 0.25,
                                   scale = c(1, 2, 0.5),
                                   shape = c("helicalspiral", "pyrstar", "hemisphere"),
                                   rotation = NULL,
                                   is_bkg = FALSE)

three_clust_05 <- make_three_clust_05()
usethis::use_data(three_clust_05, overwrite = TRUE)

