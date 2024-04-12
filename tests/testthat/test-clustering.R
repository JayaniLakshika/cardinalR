test_that("gau_clust() works", {
  set.seed(20240412)
  testthat::expect_snapshot(gau_clust(n = 300, num_clust = 5,
                                      mean_matrix = rbind(c(1,0,0,0), c(0,1,0,0), c(0,0,1,0),
                                                          c(0,0,0,1), c(0,0,0,0)),
                                      var_vec = c(0.05, 0.05, 0.05, 0.05, 0.05),
                                      num_dims = 4, num_noise = 2, min_n = -0.05,
                                      max_n = 0.05))
})

test_that("gau_clust_diff() works", {
  set.seed(20240412)
  testthat::expect_snapshot(gau_clust_diff(n = 400, clust_size_vec = c(50, 100, 200, 50),
                                           num_clust = 4, mean_matrix =
                                             rbind(c(1,0,0,0,0,0), c(0,1,0,0,0,0),
                                                   c(0,0,1,0,0,0), c(0,0,0,1,0,0)),
                                           var_vec = c(0.02, 0.05, 0.06, 0.1),
                                           num_dims = 6, num_noise = 4,
                                           min_n = -0.05, max_n = 0.05))
})

test_that("clust_diff_shapes() works", {
  set.seed(20240412)
  testthat::expect_snapshot(clust_diff_shapes(n = 300, num_gau_clust = 4,
                                              num_non_gau_clust = 2, clust_sd_gau = 0.05,
                                              clust_sd_non_gau = 0.1,
                                              num_dims = 7, a = 2, b = 4))
})

test_that("clust_diff_shapes_pts() works", {
  set.seed(20240412)
  testthat::expect_snapshot(clust_diff_shapes_pts(n = 400, clust_size_vec = c(50, 50, 50, 50, 100, 100),
                                                  num_gau_clust = 4, num_non_gau_clust = 2,
                                                  clust_sd_gau = 0.05, clust_sd_non_gau = 0.1,
                                                  num_dims = 7, a = 2, b = 4))
})

test_that("gau_curvy_clust_bkg() works", {
  set.seed(20240412)
  testthat::expect_snapshot(gau_curvy_clust_bkg(n = 260, num_noise = 2,
                                                min_n = -0.05, max_n = 0.05))
})

test_that("one_doublet() works", {
  set.seed(20240412)
  testthat::expect_snapshot(one_doublet(n = 220, num_noise = 2, min_n = -0.05,
                                        max_n = 0.05))
})

test_that("three_doublets() works", {
  set.seed(20240412)
  testthat::expect_snapshot(three_doublets(n = 420, num_noise = 2, min_n = -0.05,
                                           max_n = 0.05))
})

test_that("one_doublet_four_clusts() works", {
  set.seed(20240412)
  testthat::expect_snapshot(one_doublet_four_clusts(n = 440, num_noise = 2,
                                                   min_n = -0.05, max_n = 0.05))
})

test_that("one_doublet_diff_var_clust() works", {
  set.seed(20240412)
  testthat::expect_snapshot(one_doublet_diff_var_clust(n = 260, num_noise = 2,
                                                       min_n = -0.05, max_n = 0.05))
})

test_that("one_doublet_diff_patterns() works", {
  set.seed(20240412)
  testthat::expect_snapshot(one_doublet_diff_patterns(n = 280, num_noise = 2,
                                                      min_n = -0.05, max_n = 0.05))
})

test_that("two_doublets_parallel() works", {
  set.seed(20240412)
  testthat::expect_snapshot(two_doublets_parallel(n = 440, num_noise = 2, min_n = -0.05,
                                                  max_n = 0.05))
})

test_that("one_doublet_bkg() works", {
  set.seed(20240412)
  testthat::expect_snapshot(one_doublet_bkg(n = 250, num_noise = 2, min_n = -0.05,
                                            max_n = 0.05))
})

test_that("two_doublets_bkg() works", {
  set.seed(20240412)
  testthat::expect_snapshot(two_doublets_bkg(n = 200, num_noise = 2, min_n = -0.05,
                                             max_n = 0.05))
})

test_that("two_nonlinear() works", {
  set.seed(20240412)
  testthat::expect_snapshot(two_nonlinear(n = 200, num_noise = 2, min_n = -0.05,
                                          max_n = 0.50))
})
