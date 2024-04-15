test_that("sphere() works", {
  set.seed(20240412)
  testthat::expect_snapshot(sphere(
    radius = 1, resolution = 20, num_noise = 3,
    min_n = -0.05, max_n = 0.05
  ))

  testthat::expect_error(sphere(
    radius = -1, resolution = 20, num_noise = 3,
    min_n = -0.05, max_n = 0.05
  ))
  testthat::expect_error(sphere(
    radius = 1, resolution = -20, num_noise = 3,
    min_n = -0.05, max_n = 0.05
  ))
  testthat::expect_error(sphere(
    radius = 1, resolution = 20, num_noise = -3,
    min_n = -0.05, max_n = 0.05
  ))
  testthat::expect_error(sphere(
    resolution = 20, num_noise = 3,
    min_n = -0.05, max_n = 0.05
  ))
  testthat::expect_error(sphere(
    radius = 1, num_noise = 3,
    min_n = -0.05, max_n = 0.05
  ))
  testthat::expect_snapshot(sphere(radius = 1, resolution = 20, num_noise = 0))
  testthat::expect_error(sphere(
    radius = 1, resolution = 20, num_noise = 3,
    max_n = 0.05
  ))
  testthat::expect_error(sphere(
    radius = -1, resolution = 20, num_noise = 3,
    min_n = -0.05
  ))
})

test_that("diff_sphere() works", {
  set.seed(20240412)
  testthat::expect_snapshot(diff_sphere(
    n = 390, num_noise = 4, min_n = -0.05,
    max_n = 0.05
  ))

  testthat::expect_error(diff_sphere(
    n = -390, num_noise = 4, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(diff_sphere(
    n = 390, num_noise = -4, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_snapshot(diff_sphere(n = 390, num_noise = 0))
  testthat::expect_error(diff_sphere(n = 390, num_noise = 4, min_n = -0.05))
  testthat::expect_error(diff_sphere(n = 390, num_noise = 4, max_n = 0.05))
})
