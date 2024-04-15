test_that("three_circulars() works", {
  set.seed(20240412)
  testthat::expect_snapshot(three_circulars(
    n = 300, num_noise = 2, min_n = -0.01,
    max_n = 0.01
  ))

  testthat::expect_error(three_circulars(
    n = -300, num_noise = 2, min_n = -0.01,
    max_n = 0.01
  ))
  testthat::expect_error(three_circulars(
    n = 300, num_noise = -2, min_n = -0.01,
    max_n = 0.01
  ))
  testthat::expect_error(three_circulars(
    num_noise = 2, min_n = -0.01,
    max_n = 0.01
  ))
  testthat::expect_error(three_circulars(
    n = 300, min_n = -0.01,
    max_n = 0.01
  ))
  testthat::expect_error(three_circulars(n = 300, num_noise = 2, min_n = -0.01))
  testthat::expect_error(three_circulars(n = 300, num_noise = 2, max_n = 0.01))
  testthat::expect_snapshot(three_circulars(n = 300, num_noise = 0))
})

test_that("cell_cycle() works", {
  set.seed(20240412)
  testthat::expect_snapshot(cell_cycle(
    n = 300, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))

  testthat::expect_error(cell_cycle(
    n = -300, num_noise = 2, min_n = -0.01,
    max_n = 0.01
  ))
  testthat::expect_error(cell_cycle(
    n = 300, num_noise = -2, min_n = -0.01,
    max_n = 0.01
  ))
  testthat::expect_error(cell_cycle(
    num_noise = 2, min_n = -0.01,
    max_n = 0.01
  ))
  testthat::expect_error(cell_cycle(
    n = 300, min_n = -0.01,
    max_n = 0.01
  ))
  testthat::expect_error(cell_cycle(n = 300, num_noise = 2, min_n = -0.01))
  testthat::expect_error(cell_cycle(n = 300, num_noise = 2, max_n = 0.01))
  testthat::expect_snapshot(cell_cycle(n = 300, num_noise = 0))
})

test_that("curvy_cycle() works", {
  set.seed(20240412)
  testthat::expect_snapshot(curvy_cycle(
    n = 300, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))

  testthat::expect_error(curvy_cycle(
    n = -300, num_noise = 2, min_n = -0.01,
    max_n = 0.01
  ))
  testthat::expect_error(curvy_cycle(
    n = 300, num_noise = -2, min_n = -0.01,
    max_n = 0.01
  ))
  testthat::expect_error(curvy_cycle(
    num_noise = 2, min_n = -0.01,
    max_n = 0.01
  ))
  testthat::expect_error(curvy_cycle(
    n = 300, min_n = -0.01,
    max_n = 0.01
  ))
  testthat::expect_error(curvy_cycle(n = 300, num_noise = 2, min_n = -0.01))
  testthat::expect_error(curvy_cycle(n = 300, num_noise = 2, max_n = 0.01))
  testthat::expect_snapshot(curvy_cycle(n = 300, num_noise = 0))
})

test_that("two_circulars() works", {
  set.seed(20240412)
  testthat::expect_snapshot(two_circulars(
    n = 200, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))

  testthat::expect_error(two_circulars(
    n = -200, num_noise = 2, min_n = -0.01,
    max_n = 0.01
  ))
  testthat::expect_error(two_circulars(
    n = 200, num_noise = -2, min_n = -0.01,
    max_n = 0.01
  ))
  testthat::expect_error(two_circulars(
    num_noise = 2, min_n = -0.01,
    max_n = 0.01
  ))
  testthat::expect_error(two_circulars(
    n = 200, min_n = -0.01,
    max_n = 0.01
  ))
  testthat::expect_error(two_circulars(n = 200, num_noise = 2, min_n = -0.01))
  testthat::expect_error(two_circulars(n = 200, num_noise = 2, max_n = 0.01))
  testthat::expect_snapshot(two_circulars(n = 200, num_noise = 0))
})
