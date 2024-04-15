test_that("curv_2d() works", {
  set.seed(20240412)
  testthat::expect_snapshot(curv_2d(
    n = 100, num_noise = 2, min_n = -0.01,
    max_n = 0.01
  ))

  testthat::expect_error(curv_2d(
    n = -100, num_noise = 2, min_n = -0.01,
    max_n = 0.01
  ))
  testthat::expect_error(curv_2d(
    n = 100, num_noise = -2, min_n = -0.01,
    max_n = 0.01
  ))
  testthat::expect_snapshot(curv_2d(n = 100, num_noise = 0))
  testthat::expect_error(curv_2d(n = 100, num_noise = 2, max_n = 0.01))
  testthat::expect_error(curv_2d(n = 100, num_noise = 2, min_n = -0.01))
})

test_that("nonlinear_2d() works", {
  set.seed(20240412)
  testthat::expect_snapshot(nonlinear_2d(
    n = 100, num_noise = 2, min_n = -0.01,
    max_n = 0.01
  ))
  testthat::expect_error(nonlinear_2d(
    n = -100, num_noise = 2, min_n = -0.01,
    max_n = 0.01
  ))
  testthat::expect_error(nonlinear_2d(
    n = 100, num_noise = -2, min_n = -0.01,
    max_n = 0.01
  ))
  testthat::expect_snapshot(nonlinear_2d(n = 100, num_noise = 0))
  testthat::expect_error(nonlinear_2d(n = 100, num_noise = 2, max_n = 0.01))
  testthat::expect_error(nonlinear_2d(n = 100, num_noise = 2, min_n = -0.01))
})

test_that("sine_curve() works", {
  set.seed(20240412)
  testthat::expect_snapshot(sine_curve(
    n = 100, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))

  testthat::expect_error(sine_curve(
    n = -100, num_noise = 2, min_n = -0.01,
    max_n = 0.01
  ))
  testthat::expect_error(sine_curve(
    n = 100, num_noise = -2, min_n = -0.01,
    max_n = 0.01
  ))
  testthat::expect_snapshot(sine_curve(n = 100, num_noise = 0))
  testthat::expect_error(sine_curve(n = 100, num_noise = 2, max_n = 0.01))
  testthat::expect_error(sine_curve(n = 100, num_noise = 2, min_n = -0.01))
})

test_that("nonlinear_connect() works", {
  set.seed(20240412)
  testthat::expect_snapshot(nonlinear_connect(
    n = 400, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(nonlinear_connect(
    n = -400, num_noise = 2, min_n = -0.01,
    max_n = 0.01
  ))
  testthat::expect_error(nonlinear_connect(
    n = 400, num_noise = -2, min_n = -0.01,
    max_n = 0.01
  ))
  testthat::expect_snapshot(nonlinear_connect(n = 400, num_noise = 0))
  testthat::expect_error(nonlinear_connect(n = 400, num_noise = 2, max_n = 0.01))
  testthat::expect_error(nonlinear_connect(n = 400, num_noise = 2, min_n = -0.01))
})

test_that("nonlinear_mirror() works", {
  set.seed(20240412)
  testthat::expect_snapshot(nonlinear_mirror(
    n = 400, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))

  testthat::expect_error(nonlinear_mirror(
    n = -400, num_noise = 2, min_n = -0.01,
    max_n = 0.01
  ))
  testthat::expect_error(nonlinear_mirror(
    n = 400, num_noise = -2, min_n = -0.01,
    max_n = 0.01
  ))
  testthat::expect_snapshot(nonlinear_mirror(n = 400, num_noise = 0))
  testthat::expect_error(nonlinear_mirror(n = 400, num_noise = 2, max_n = 0.01))
  testthat::expect_error(nonlinear_mirror(n = 400, num_noise = 2, min_n = -0.01))
})

test_that("two_curvy_panckakes() works", {
  set.seed(20240412)
  testthat::expect_snapshot(two_curvy_panckakes(
    n = 300, num_noise = 2,
    min_n = -0.05, max_n = 0.05
  ))

  testthat::expect_error(two_curvy_panckakes(
    n = -300, num_noise = 2, min_n = -0.01,
    max_n = 0.01
  ))
  testthat::expect_error(two_curvy_panckakes(
    n = 300, num_noise = -2, min_n = -0.01,
    max_n = 0.01
  ))
  testthat::expect_snapshot(two_curvy_panckakes(n = 300, num_noise = 0))
  testthat::expect_error(two_curvy_panckakes(n = 300, num_noise = 2, max_n = 0.01))
  testthat::expect_error(two_curvy_panckakes(n = 300, num_noise = 2, min_n = -0.01))
})

test_that("two_curvilinear() works", {
  set.seed(20240412)
  testthat::expect_snapshot(two_curvilinear(
    n = 250, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))

  testthat::expect_error(two_curvilinear(
    n = -250, num_noise = 2, min_n = -0.01,
    max_n = 0.01
  ))
  testthat::expect_error(two_curvilinear(
    n = 250, num_noise = -2, min_n = -0.01,
    max_n = 0.01
  ))
  testthat::expect_snapshot(two_curvilinear(n = 250, num_noise = 0))
  testthat::expect_error(two_curvilinear(n = 250, num_noise = 2, max_n = 0.01))
  testthat::expect_error(two_curvilinear(n = 250, num_noise = 2, min_n = -0.01))
})

test_that("swiss_roll() works", {
  set.seed(20240412)
  testthat::expect_snapshot(swiss_roll(
    n = 200, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))

  testthat::expect_error(swiss_roll(
    n = -200, num_noise = 2, min_n = -0.01,
    max_n = 0.01
  ))
  testthat::expect_error(swiss_roll(
    n = 200, num_noise = -2, min_n = -0.01,
    max_n = 0.01
  ))
  testthat::expect_snapshot(swiss_roll(n = 200, num_noise = 0))
  testthat::expect_error(swiss_roll(n = 200, num_noise = 2, max_n = 0.01))
  testthat::expect_error(swiss_roll(n = 200, num_noise = 2, min_n = -0.01))
})
