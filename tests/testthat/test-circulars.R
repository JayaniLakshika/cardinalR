test_that("three_circulars() works", {
  set.seed(20240412)
  testthat::expect_snapshot(gen_noise_dims(n = 50, num_noise = 3, min_n = -0.01,
                                           max_n = 0.01))
})

test_that("cell_cycle() works", {
  set.seed(20240412)
  testthat::expect_snapshot(cell_cycle(n = 300, num_noise = 2, min_n = -0.05,
                                       max_n = 0.05))
})

test_that("curvy_cycle() works", {
  set.seed(20240412)
  testthat::expect_snapshot(curvy_cycle(n = 300, num_noise = 2, min_n = -0.05,
                                        max_n = 0.05))
})

test_that("two_circulars() works", {
  set.seed(20240412)
  testthat::expect_snapshot(two_circulars(n = 200, num_noise = 2, min_n = -0.05,
                                          max_n = 0.05))
})
