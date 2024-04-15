test_that("gen_noise_dims() works", {
  set.seed(20240412)
  testthat::expect_snapshot(gen_noise_dims(
    n = 50, num_noise = 3, min_n = -0.01,
    max_n = 0.01
  ))

  testthat::expect_error(gen_noise_dims(
    n = -50, num_noise = 3, min_n = -0.01,
    max_n = 0.01
  ))
  testthat::expect_error(gen_noise_dims(
    n = 50, num_noise = -3, min_n = -0.01,
    max_n = 0.01
  ))
  testthat::expect_error(gen_noise_dims(
    num_noise = 3, min_n = -0.01,
    max_n = 0.01
  ))
  testthat::expect_error(gen_noise_dims(
    n = 50, min_n = -0.01,
    max_n = 0.01
  ))
  testthat::expect_error(gen_noise_dims(
    n = 50, num_noise = 3,
    max_n = 0.01
  ))
  testthat::expect_error(gen_noise_dims(
    n = 50, num_noise = 3,
    min_n = -0.01
  ))
  testthat::expect_error(gen_noise_dims(n = 50, num_noise = 3))
  testthat::expect_null(gen_noise_dims(n = 50, num_noise = 0))
})

test_that("gen_noise_dims() works", {
  set.seed(20240412)
  testthat::expect_snapshot(gen_bkg_noise(n = 50, num_dims = 3, mean = 5, sd = 2))

  testthat::expect_error(gen_bkg_noise(n = -50, num_dims = 3, mean = 5, sd = 2))
  testthat::expect_error(gen_bkg_noise(n = 50, num_dims = -3, mean = 5, sd = 2))
  testthat::expect_error(gen_bkg_noise(num_dims = 3, mean = 5, sd = 2))
  testthat::expect_error(gen_bkg_noise(n = 50, mean = 5, sd = 2))
  testthat::expect_error(gen_bkg_noise(n = 50, num_dims = 3, sd = 2))
  testthat::expect_error(gen_bkg_noise(n = 50, num_dims = 3, mean = 5))
  testthat::expect_error(gen_bkg_noise(n = 50, num_dims = 3))
  testthat::expect_null(gen_bkg_noise(n = 50, num_dims = 0))
})
