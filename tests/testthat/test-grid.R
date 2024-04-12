test_that("one_grid() works", {
  set.seed(20240412)
  testthat::expect_snapshot(one_grid(nx = 10, ny = 10, num_noise = 2, min_n = -0.05,
                                     max_n = 0.05))
})

test_that("two_grid() works", {
  set.seed(20240412)
  testthat::expect_snapshot(two_grid(n_value = 19, num_noise = 2, min_n = -0.05,
                                     max_n = 0.05))
})

test_that("three_grid() works", {
  set.seed(20240412)
  testthat::expect_snapshot(three_grid(n_value = 19, num_noise = 2, min_n = -0.05,
                                       max_n = 0.05))
})

test_that("one_grid_bkg() works", {
  set.seed(20240412)
  testthat::expect_snapshot(one_grid_bkg(n_value = 10, num_noise = 2, min_n = -0.05,
                                         max_n = 0.05))
})

test_that("two_grid_comb_bkg() works", {
  set.seed(20240412)
  testthat::expect_snapshot(two_grid_comb_bkg(n_value = 10, num_noise = 2,
                                              min_n = -0.05, max_n = 0.05))

})

test_that("two_grid_comb() works", {
  set.seed(20240412)
  testthat::expect_snapshot(two_grid_comb(n = 200, num_noise = 2, min_n = -0.05,
                                          max_n = 0.05))

})
