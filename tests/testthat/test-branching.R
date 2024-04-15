test_that("curvy_tree() works", {
  set.seed(20240412)
  testthat::expect_snapshot(curvy_tree(
    n = 300, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))

  testthat::expect_error(curvy_tree(
    n = -300, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(curvy_tree(
    n = 300, num_noise = -2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(curvy_tree(
    num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(curvy_tree(n = 300, min_n = -0.05, max_n = 0.05))
  testthat::expect_error(curvy_tree(n = 300, num_noise = 2, min_n = -0.05))
  testthat::expect_error(curvy_tree(n = 300, num_noise = 2, max_n = 0.05))
  testthat::expect_snapshot(curvy_tree(n = 300, num_noise = 0))
})

test_that("tree() works", {
  set.seed(20240412)
  testthat::expect_snapshot(tree(
    n = 300, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))

  testthat::expect_error(tree(
    n = -300, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(tree(
    n = 300, num_noise = -2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(tree(
    num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(tree(n = 300, min_n = -0.05, max_n = 0.05))
  testthat::expect_error(tree(n = 300, num_noise = 2, min_n = -0.05))
  testthat::expect_error(tree(n = 300, num_noise = 2, max_n = 0.05))
  testthat::expect_snapshot(tree(n = 300, num_noise = 0))
})

test_that("seven_branch() works", {
  set.seed(20240412)
  testthat::expect_snapshot(seven_branch(
    n = 210, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))

  testthat::expect_error(seven_branch(
    n = -210, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(seven_branch(
    n = 210, num_noise = -2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(seven_branch(
    num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(seven_branch(n = 210, min_n = -0.05, max_n = 0.05))
  testthat::expect_error(seven_branch(n = 210, num_noise = 2, min_n = -0.05))
  testthat::expect_error(seven_branch(n = 210, num_noise = 2, max_n = 0.05))
  testthat::expect_snapshot(seven_branch(n = 210, num_noise = 0))
})

test_that("four_branch() works", {
  set.seed(20240412)
  testthat::expect_snapshot(four_branch(
    n = 400, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))

  testthat::expect_error(four_branch(
    n = -400, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(four_branch(
    n = 400, num_noise = -2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(four_branch(
    num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(four_branch(n = 400, min_n = -0.05, max_n = 0.05))
  testthat::expect_error(four_branch(n = 400, num_noise = 2, min_n = -0.05))
  testthat::expect_error(four_branch(n = 400, num_noise = 2, max_n = 0.05))
  testthat::expect_snapshot(four_branch(n = 400, num_noise = 0))
})

test_that("eight_branch() works", {
  set.seed(20240412)
  testthat::expect_snapshot(eight_branch(
    n = 400, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))

  testthat::expect_error(eight_branch(
    n = -400, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(eight_branch(
    n = 400, num_noise = -2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(eight_branch(
    num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(eight_branch(n = 400, min_n = -0.05, max_n = 0.05))
  testthat::expect_error(eight_branch(n = 400, num_noise = 2, min_n = -0.05))
  testthat::expect_error(eight_branch(n = 400, num_noise = 2, max_n = 0.05))
  testthat::expect_snapshot(eight_branch(n = 400, num_noise = 0))
})

test_that("curvy_branch_clust() works", {
  set.seed(20240412)
  testthat::expect_snapshot(curvy_branch_clust(
    n = 300, clust_vec = c(100, 150, 50),
    num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_snapshot(curvy_branch_clust(
    n = 300, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))

  testthat::expect_error(curvy_branch_clust(
    n = -300, clust_vec = c(100, 150, 50),
    num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(curvy_branch_clust(
    n = 300, clust_vec = c(100, 150, 50),
    num_noise = -2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(curvy_branch_clust(
    clust_vec = c(100, 150, 50), num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(curvy_branch_clust(
    n = 300, clust_vec = c(100, 150, 50),
    min_n = -0.05, max_n = 0.05
  ))
  testthat::expect_error(curvy_branch_clust(
    n = 300, clust_vec = c(100, 150, 50),
    num_noise = 2, min_n = -0.05
  ))
  testthat::expect_error(curvy_branch_clust(
    n = 300, clust_vec = c(100, 150, 50),
    num_noise = 2, max_n = 0.05
  ))
  testthat::expect_snapshot(curvy_branch_clust(
    n = 300, clust_vec = c(100, 150, 50),
    num_noise = 0
  ))
})

test_that("curvy_branch_clust_bkg() works", {
  set.seed(20240412)
  testthat::expect_snapshot(curvy_branch_clust_bkg(
    n = 400, num_noise = 2,
    min_n = -0.05, max_n = 0.05
  ))

  testthat::expect_error(curvy_branch_clust_bkg(
    n = -400, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(curvy_branch_clust_bkg(
    n = 400, num_noise = -2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(curvy_branch_clust_bkg(
    num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(curvy_branch_clust_bkg(n = 400, min_n = -0.05, max_n = 0.05))
  testthat::expect_error(curvy_branch_clust_bkg(n = 400, num_noise = 2, min_n = -0.05))
  testthat::expect_error(curvy_branch_clust_bkg(n = 400, num_noise = 2, max_n = 0.05))
  testthat::expect_snapshot(curvy_branch_clust_bkg(n = 400, num_noise = 0))
})

test_that("curvy_branch() works", {
  set.seed(20240412)
  testthat::expect_snapshot(curvy_branch(
    n = 200, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))

  testthat::expect_error(curvy_branch(
    n = -200, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(curvy_branch(
    n = 200, num_noise = -2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(curvy_branch(
    num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(curvy_branch(n = 200, min_n = -0.05, max_n = 0.05))
  testthat::expect_error(curvy_branch(n = 200, num_noise = 2, min_n = -0.05))
  testthat::expect_error(curvy_branch(n = 200, num_noise = 2, max_n = 0.05))
  testthat::expect_snapshot(curvy_branch(n = 200, num_noise = 0))
})
