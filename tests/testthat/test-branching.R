test_that("curvy_tree() works", {
  set.seed(20240412)
  testthat::expect_snapshot(curvy_tree(n = 300, num_noise = 2, min_n = -0.05, max_n = 0.05))
})

test_that("tree() works", {
  set.seed(20240412)
  testthat::expect_snapshot(tree(n = 300, num_noise = 2, min_n = -0.05, max_n = 0.05))
})

test_that("seven_branch() works", {
  set.seed(20240412)
  testthat::expect_snapshot(seven_branch(n = 210, num_noise = 2, min_n = -0.05,
                                         max_n = 0.05))
})

test_that("four_branch() works", {
  set.seed(20240412)
  testthat::expect_snapshot(four_branch(n = 400, num_noise = 2, min_n = -0.05,
                                        max_n = 0.05))
})

test_that("eight_branch() works", {
  set.seed(20240412)
  testthat::expect_snapshot(eight_branch(n = 400, num_noise = 2, min_n = -0.05,
                                         max_n = 0.05))
})

test_that("curvy_branch_clust() works", {
  set.seed(20240412)
  testthat::expect_snapshot(curvy_branch_clust(n = 300, clust_vec = c(100, 150, 50),
                                               num_noise = 2, min_n = -0.05,
                                               max_n = 0.05))
})

test_that("curvy_branch_clust_bkg() works", {
  set.seed(20240412)
  testthat::expect_snapshot(curvy_branch_clust_bkg(n = 400, num_noise = 2,
                                                   min_n = -0.05, max_n = 0.05))
})

test_that("curvy_branch() works", {
  set.seed(20240412)
  testthat::expect_snapshot(curvy_branch(n = 200, num_noise = 2, min_n = -0.05,
                                         max_n = 0.05))
})
