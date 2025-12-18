test_that("gen_gaussian() works", {
  set.seed(20240412)
  testthat::expect_snapshot(gen_gaussian(n = 500, p = 4, m = rep(0, 4), s = diag(4)))
  testthat::expect_error(gen_gaussian(n = 500, p = 1, m = rep(0, 4), s = diag(4)))
  testthat::expect_error(gen_gaussian(n = -500, p = 4, m = rep(0, 4), s = diag(4)))
  testthat::expect_error(gen_gaussian(n = 500, p = 4, m = rep(0, 4), s = 1))
  testthat::expect_error(gen_gaussian(n = 500, p = 4, m = 3, s = diag(4)))

  mat <- matrix(1:12, nrow = 4, ncol = 3)
  testthat::expect_error(gen_gaussian(n = 500, p = 4, s = mat))

  mat <- matrix(1:6, nrow = 2, ncol = 3)
  testthat::expect_error(gen_gaussian(n = 500, p = 4, s = mat))

})
