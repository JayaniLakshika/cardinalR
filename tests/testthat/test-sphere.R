test_that("gen_circle() works", {
  set.seed(20240412)
  testthat::expect_snapshot(gen_circle(n = 500, p = 4))
  testthat::expect_error(gen_circle(n = -500, p = 4))
  testthat::expect_error(gen_circle(n = 500, p = 2))
  testthat::expect_error(gen_circle(n = c(500, 100), p = 2))

})

test_that("gen_curvycycle() works", {
  set.seed(20240412)
  testthat::expect_snapshot(gen_curvycycle(n = 500, p = 4))
  testthat::expect_error(gen_curvycycle(n = -500, p = 4))
  testthat::expect_error(gen_curvycycle(n = 500, p = 2))
  testthat::expect_error(gen_curvycycle(n = c(500, 100), p = 2))

})

test_that("gen_unifsphere() works", {
  set.seed(20240412)
  testthat::expect_snapshot(gen_unifsphere(n = 500))
  testthat::expect_error(gen_unifsphere(n = 500, r = -0.5))
  testthat::expect_error(gen_unifsphere(n = -500))

})

test_that("gen_gridedsphere() works", {
  set.seed(20240412)
  testthat::expect_snapshot(gen_gridedsphere(n = 500, p = 4))
  testthat::expect_error(gen_gridedsphere(n = 500, p = 2))
  testthat::expect_error(gen_gridedsphere(n = -500, p = 4))

})

test_that("gen_clusteredspheres() works", {
  set.seed(20240412)

  clusteredspheres <- gen_clusteredspheres(n_vec = c(1000, 100), k_small = 3,
  r_vec = c(15, 3), sep = 10 / sqrt(3))

  testthat::expect_snapshot(clusteredspheres)
  testthat::expect_error(gen_clusteredspheres(n_vec = c(1000, 100, 300), k_small = 3,
                                              r_vec = c(15, 3), sep = 10 / sqrt(3)))
  testthat::expect_error(gen_clusteredspheres(n = c(1000, -100), k_small = 3,
                                              r_vec = c(15, 3), sep = 10 / sqrt(3)))
  testthat::expect_error(gen_clusteredspheres(n = c(1000, 100), k_small = 3,
                                              r_vec = c(-15, 3), sep = 10 / sqrt(3)))

})

test_that("gen_hemisphere() works", {
  set.seed(20240412)
  testthat::expect_snapshot(gen_hemisphere(n = 500))
  testthat::expect_error(gen_hemisphere(n = -500))

})
