test_that("one_grid() works", {
  set.seed(20240412)
  testthat::expect_snapshot(one_grid(
    nx = 10, ny = 10, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))

  testthat::expect_error(one_grid(
    nx = -10, ny = 10, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(one_grid(
    nx = 10, ny = -10, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(one_grid(
    nx = 10, ny = 10, num_noise = -2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(one_grid(
    ny = 10, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(one_grid(
    nx = 10, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(one_grid(
    nx = 10, ny = 10, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(one_grid(nx = 10, ny = 10, num_noise = 2, min_n = -0.05))
  testthat::expect_error(one_grid(nx = 10, ny = 10, num_noise = 2, max_n = 0.05))
  testthat::expect_snapshot(one_grid(nx = 10, ny = 10, num_noise = 0))
})

test_that("two_grid() works", {
  set.seed(20240412)
  testthat::expect_snapshot(two_grid(
    n_value = 19, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_length(two_grid(
    n_value = 19, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ), 2)

  testthat::expect_error(two_grid(
    n_value = -19, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(two_grid(
    n_value = 19, num_noise = -2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(two_grid(
    num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(two_grid(n_value = 19, min_n = -0.05, max_n = 0.05))
  testthat::expect_error(two_grid(n_value = 19, num_noise = 2, min_n = -0.05))
  testthat::expect_snapshot(two_grid(n_value = 19, num_noise = 0))
})

test_that("three_grid() works", {
  set.seed(20240412)
  testthat::expect_snapshot(three_grid(
    n_value = 19, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))

  testthat::expect_length(three_grid(
    n_value = 19, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ), 2)

  testthat::expect_error(three_grid(
    n_value = -19, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(three_grid(
    n_value = 19, num_noise = -2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(three_grid(
    num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(three_grid(n_value = 19, min_n = -0.05, max_n = 0.05))
  testthat::expect_error(three_grid(n_value = 19, num_noise = 2, min_n = -0.05))
  testthat::expect_snapshot(three_grid(n_value = 19, num_noise = 0))
})

test_that("one_grid_bkg() works", {
  set.seed(20240412)
  testthat::expect_snapshot(one_grid_bkg(
    n_value = 10, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))

  testthat::expect_length(one_grid_bkg(
    n_value = 10, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ), 2)

  testthat::expect_error(one_grid_bkg(
    n_value = -10, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(one_grid_bkg(
    n_value = 10, num_noise = -2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(one_grid_bkg(
    num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(one_grid_bkg(n_value = 10, min_n = -0.05, max_n = 0.05))
  testthat::expect_error(one_grid_bkg(n_value = 10, num_noise = 2, min_n = -0.05))
  testthat::expect_snapshot(one_grid_bkg(n_value = 10, num_noise = 0))
})

test_that("two_grid_comb_bkg() works", {
  set.seed(20240412)
  testthat::expect_snapshot(two_grid_comb_bkg(
    n_value = 10, num_noise = 2,
    min_n = -0.05, max_n = 0.05
  ))

  testthat::expect_length(two_grid_comb_bkg(
    n_value = 10, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ), 2)

  testthat::expect_error(two_grid_comb_bkg(
    n_value = -10, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(two_grid_comb_bkg(
    n_value = 10, num_noise = -2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(two_grid_comb_bkg(
    num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(two_grid_comb_bkg(n_value = 10, min_n = -0.05, max_n = 0.05))
  testthat::expect_error(two_grid_comb_bkg(n_value = 10, num_noise = 2, min_n = -0.05))
  testthat::expect_snapshot(two_grid_comb_bkg(n_value = 10, num_noise = 0))
})

test_that("two_grid_comb() works", {
  set.seed(20240412)
  testthat::expect_snapshot(two_grid_comb(
    n_value = 10, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))

  testthat::expect_length(two_grid_comb(
    n_value = 10, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ), 2)

  testthat::expect_error(two_grid_comb(
    n_value = -10, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(two_grid_comb(
    n_value = 10, num_noise = -2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(two_grid_comb(
    num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(two_grid_comb(n_value = 10, min_n = -0.05, max_n = 0.05))
  testthat::expect_error(two_grid_comb(n_value = 10, num_noise = 2, min_n = -0.05))
  testthat::expect_snapshot(two_grid_comb(n_value = 10, num_noise = 0))
})
