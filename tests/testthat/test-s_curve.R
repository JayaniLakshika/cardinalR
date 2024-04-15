test_that("scurve() works", {
  set.seed(20240412)
  testthat::expect_snapshot(scurve(
    n = 100, num_noise = 3, min_n = -0.05,
    max_n = 0.05
  ))

  testthat::expect_error(scurve(
    n = -100, num_noise = 3, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(scurve(
    n = 100, num_noise = -3, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(scurve(
    n = 100, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(scurve(
    num_noise = 3, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_snapshot(scurve(n = 100, num_noise = 0))
  testthat::expect_error(scurve(n = 100, num_noise = 3, max_n = 0.05))
  testthat::expect_error(scurve(n = 100, num_noise = 3, min_n = -0.05))
})

test_that("scurve_hole() works", {
  set.seed(20240412)
  testthat::expect_snapshot(scurve_hole(
    n = 100, num_noise = 3, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(scurve_hole(
    n = -100, num_noise = 3, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(scurve_hole(
    n = 100, num_noise = -3, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(scurve_hole(
    n = 100, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(scurve_hole(
    num_noise = 3, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_snapshot(scurve_hole(n = 100, num_noise = 0))
  testthat::expect_error(scurve_hole(n = 100, num_noise = 3, max_n = 0.05))
  testthat::expect_error(scurve_hole(n = 100, num_noise = 3, min_n = -0.05))
})

test_that("two_scurves() works", {
  set.seed(20240412)
  testthat::expect_snapshot(two_scurves(
    n = 200, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))

  testthat::expect_error(two_scurves(
    n = -200, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(two_scurves(
    n = 200, num_noise = -2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(two_scurves(
    n = 200, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(two_scurves(
    num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_snapshot(two_scurves(n = 200, num_noise = 0))
  testthat::expect_error(two_scurves(n = 200, num_noise = 2, max_n = 0.05))
  testthat::expect_error(two_scurves(n = 200, num_noise = 2, min_n = -0.05))
})

test_that("mirror_scurves() works", {
  set.seed(20240412)
  testthat::expect_snapshot(mirror_scurves(
    n = 200, num_noise = 2,
    min_n = -0.05, max_n = 0.05
  ))

  testthat::expect_error(mirror_scurves(
    n = -200, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(mirror_scurves(
    n = 200, num_noise = -2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(mirror_scurves(
    n = 200, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(mirror_scurves(
    num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_snapshot(mirror_scurves(n = 200, num_noise = 0))
  testthat::expect_error(mirror_scurves(n = 200, num_noise = 2, max_n = 0.05))
  testthat::expect_error(mirror_scurves(n = 200, num_noise = 2, min_n = -0.05))
})

test_that("two_scurve_hole() works", {
  set.seed(20240412)
  testthat::expect_snapshot(two_scurve_hole(
    n = 200, num_noise = 2,
    min_n = -0.05, max_n = 0.05
  ))

  testthat::expect_error(two_scurve_hole(
    n = -200, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(two_scurve_hole(
    n = 200, num_noise = -2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(two_scurve_hole(
    n = 200, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(two_scurve_hole(
    num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_snapshot(two_scurve_hole(n = 200, num_noise = 0))
  testthat::expect_error(two_scurve_hole(n = 200, num_noise = 2, max_n = 0.05))
  testthat::expect_error(two_scurve_hole(n = 200, num_noise = 2, min_n = -0.05))
})
