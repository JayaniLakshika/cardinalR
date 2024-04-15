test_that("mobius_5d() works", {
  set.seed(20240412)
  testthat::expect_snapshot(mobius_5d(
    n = 100, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))

  testthat::expect_error(mobius_5d(
    n = -100, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(mobius_5d(
    n = 100, num_noise = -2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(mobius_5d(
    num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(mobius_5d(n = 100, min_n = -0.05, max_n = 0.05))
  testthat::expect_error(mobius_5d(n = 100, num_noise = 2, min_n = -0.05))
  testthat::expect_error(mobius_5d(n = 100, num_noise = 2, max_n = 0.05))
  testthat::expect_snapshot(mobius_5d(n = 100, num_noise = 0))
})

test_that("mobius_clust() works", {
  set.seed(20240412)
  testthat::expect_snapshot(mobius_clust(
    n = 200, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))

  testthat::expect_error(mobius_clust(
    n = -200, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(mobius_clust(
    n = 200, num_noise = -2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(mobius_clust(
    num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(mobius_clust(n = 200, min_n = -0.05, max_n = 0.05))
  testthat::expect_error(mobius_clust(n = 200, num_noise = 2, min_n = -0.05))
  testthat::expect_error(mobius_clust(n = 200, num_noise = 2, max_n = 0.05))
  testthat::expect_snapshot(mobius_clust(n = 200, num_noise = 0))
})
