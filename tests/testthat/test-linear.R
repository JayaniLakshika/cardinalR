test_that("plane() works", {
  set.seed(20240412)
  testthat::expect_snapshot(plane(
    n = 100, coef_x1 = 1, coef_x2 = 1,
    coef_y1 = -1, coef_y2 = 1, intercept_x = -10,
    intercept_y = 8, u_min = 10, u_max = 30, v_min = 10,
    v_max = 20, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(plane(
    n = -100, coef_x1 = 1, coef_x2 = 1,
    coef_y1 = -1, coef_y2 = 1, intercept_x = -10,
    intercept_y = 8, u_min = 10, u_max = 30, v_min = 10,
    v_max = 20, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(plane(
    n = 100, coef_x1 = 1, coef_x2 = 1,
    coef_y1 = -1, coef_y2 = 1, intercept_x = -10,
    intercept_y = 8, u_min = 10, u_max = 30, v_min = 10,
    v_max = 20, num_noise = -2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(plane(
    coef_x1 = 1, coef_x2 = 1,
    coef_y1 = -1, coef_y2 = 1, intercept_x = -10,
    intercept_y = 8, u_min = 10, u_max = 30, v_min = 10,
    v_max = 20, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(plane(
    n = 100, coef_x1 = 1, coef_x2 = 1,
    coef_y1 = -1, coef_y2 = 1, intercept_x = -10,
    intercept_y = 8, u_min = 10, u_max = 30, v_min = 10,
    v_max = 20, min_n = -0.05, max_n = 0.05
  ))
  testthat::expect_error(plane(
    n = 100, coef_x1 = 1, coef_x2 = 1,
    coef_y1 = -1, coef_y2 = 1, intercept_x = -10,
    intercept_y = 8, u_min = 10, u_max = 30, v_min = 10,
    v_max = 20, num_noise = 2, max_n = 0.05
  ))
  testthat::expect_error(plane(
    n = 100, coef_x1 = 1, coef_x2 = 1,
    coef_y1 = -1, coef_y2 = 1, intercept_x = -10,
    intercept_y = 8, u_min = 10, u_max = 30, v_min = 10,
    v_max = 20, num_noise = 2, min_n = -0.05
  ))
  testthat::expect_snapshot(plane(
    n = 100, coef_x1 = 1, coef_x2 = 1,
    coef_y1 = -1, coef_y2 = 1, intercept_x = -10,
    intercept_y = 8, u_min = 10, u_max = 30, v_min = 10,
    v_max = 20, num_noise = 0
  ))
})

test_that("two_long_clust() works", {
  set.seed(20240412)
  testthat::expect_snapshot(two_long_clust(
    n = 200, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))

  testthat::expect_error(two_long_clust(
    n = -200, num_noise = 2,
    min_n = -0.05, max_n = 0.05
  ))
  testthat::expect_error(two_long_clust(
    n = 200, num_noise = -2,
    min_n = -0.05, max_n = 0.05
  ))
  testthat::expect_error(two_long_clust(
    num_noise = 2,
    min_n = -0.05, max_n = 0.05
  ))
  testthat::expect_error(two_long_clust(n = 200, min_n = -0.05, max_n = 0.05))
  testthat::expect_error(two_long_clust(
    n = 200, num_noise = 2,
    min_n = -0.05
  ))
  testthat::expect_error(two_long_clust(
    n = 200, num_noise = 2,
    max_n = 0.05
  ))
  testthat::expect_snapshot(two_long_clust(n = 200, num_noise = 0))
})

test_that("three_diff_linear() works", {
  set.seed(20240412)
  testthat::expect_snapshot(three_diff_linear(
    n = 150, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))

  testthat::expect_error(three_diff_linear(
    n = -150, num_noise = 2,
    min_n = -0.05, max_n = 0.05
  ))
  testthat::expect_error(three_diff_linear(
    n = 150, num_noise = -2,
    min_n = -0.05, max_n = 0.05
  ))
  testthat::expect_error(three_diff_linear(
    num_noise = 2,
    min_n = -0.05, max_n = 0.05
  ))
  testthat::expect_error(three_diff_linear(
    n = 150,
    min_n = -0.05, max_n = 0.05
  ))
  testthat::expect_error(three_diff_linear(
    n = 150, num_noise = 2,
    min_n = -0.05
  ))
  testthat::expect_error(three_diff_linear(
    n = 150, num_noise = 2,
    max_n = 0.05
  ))
  testthat::expect_snapshot(three_diff_linear(n = 150, num_noise = 0))
})

test_that("four_long_clust() works", {
  set.seed(20240412)
  testthat::expect_snapshot(four_long_clust(
    n = 200, num_noise = 2,
    min_n = -0.05, max_n = 0.05
  ))

  testthat::expect_error(four_long_clust(
    n = -200, num_noise = 2,
    min_n = -0.05, max_n = 0.05
  ))
  testthat::expect_error(four_long_clust(
    n = 200, num_noise = -2,
    min_n = -0.05, max_n = 0.05
  ))
  testthat::expect_error(four_long_clust(
    num_noise = 2,
    min_n = -0.05, max_n = 0.05
  ))
  testthat::expect_error(four_long_clust(
    n = 200,
    min_n = -0.05, max_n = 0.05
  ))
  testthat::expect_error(four_long_clust(
    n = 200, num_noise = 2,
    min_n = -0.05
  ))
  testthat::expect_error(four_long_clust(
    n = 200, num_noise = 2,
    max_n = 0.05
  ))
  testthat::expect_snapshot(four_long_clust(n = 200, num_noise = 0))
})

test_that("plane_2d_hole() works", {
  set.seed(20240412)
  testthat::expect_snapshot(plane_2d_hole(
    n = 100, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))

  testthat::expect_error(plane_2d_hole(
    n = -100, num_noise = 2,
    min_n = -0.05, max_n = 0.05
  ))
  testthat::expect_error(plane_2d_hole(
    n = 100, num_noise = -2,
    min_n = -0.05, max_n = 0.05
  ))
  testthat::expect_error(plane_2d_hole(
    num_noise = 2,
    min_n = -0.05, max_n = 0.05
  ))
  testthat::expect_error(plane_2d_hole(
    n = 100,
    min_n = -0.05, max_n = 0.05
  ))
  testthat::expect_error(plane_2d_hole(
    n = 100, num_noise = 2,
    min_n = -0.05
  ))
  testthat::expect_error(plane_2d_hole(
    n = 100, num_noise = 2,
    max_n = 0.05
  ))
  testthat::expect_snapshot(plane_2d_hole(n = 100, num_noise = 0))
})

test_that("four_long_clust_bkg() works", {
  set.seed(20240412)
  testthat::expect_snapshot(four_long_clust_bkg(
    n = 400, num_noise = 2,
    min_n = -0.05, max_n = 0.05
  ))

  testthat::expect_error(four_long_clust_bkg(
    n = -400, num_noise = 2,
    min_n = -0.05, max_n = 0.05
  ))
  testthat::expect_error(four_long_clust_bkg(
    n = 400, num_noise = -2,
    min_n = -0.05, max_n = 0.05
  ))
  testthat::expect_error(four_long_clust_bkg(
    num_noise = 2,
    min_n = -0.05, max_n = 0.05
  ))
  testthat::expect_error(four_long_clust_bkg(
    n = 400,
    min_n = -0.05, max_n = 0.05
  ))
  testthat::expect_error(four_long_clust_bkg(
    n = 400, num_noise = 2,
    min_n = -0.05
  ))
  testthat::expect_error(four_long_clust_bkg(
    n = 400, num_noise = 2,
    max_n = 0.05
  ))
  testthat::expect_snapshot(four_long_clust_bkg(n = 400, num_noise = 0))
})

test_that("three_long_clust() works", {
  set.seed(20240412)
  testthat::expect_snapshot(three_long_clust(
    n = 300, num_noise = 2,
    min_n = -0.05, max_n = 0.05
  ))

  testthat::expect_error(three_long_clust(
    n = -300, num_noise = 2,
    min_n = -0.05, max_n = 0.05
  ))
  testthat::expect_error(three_long_clust(
    n = 300, num_noise = -2,
    min_n = -0.05, max_n = 0.05
  ))
  testthat::expect_error(three_long_clust(
    num_noise = 2,
    min_n = -0.05, max_n = 0.05
  ))
  testthat::expect_error(three_long_clust(
    n = 300,
    min_n = -0.05, max_n = 0.05
  ))
  testthat::expect_error(three_long_clust(
    n = 300, num_noise = 2,
    min_n = -0.05
  ))
  testthat::expect_error(three_long_clust(
    n = 300, num_noise = 2,
    max_n = 0.05
  ))
  testthat::expect_snapshot(three_long_clust(n = 300, num_noise = 0))
})

test_that("two_long_clust_diff() works", {
  set.seed(20240412)
  testthat::expect_snapshot(two_long_clust_diff(
    n = 300, num_noise = 2,
    min_n = -0.05, max_n = 0.05
  ))

  testthat::expect_error(two_long_clust_diff(
    n = -300, num_noise = 2,
    min_n = -0.05, max_n = 0.05
  ))
  testthat::expect_error(two_long_clust_diff(
    n = 300, num_noise = -2,
    min_n = -0.05, max_n = 0.05
  ))
  testthat::expect_error(two_long_clust_diff(
    num_noise = 2,
    min_n = -0.05, max_n = 0.05
  ))
  testthat::expect_error(two_long_clust_diff(
    n = 300,
    min_n = -0.05, max_n = 0.05
  ))
  testthat::expect_error(two_long_clust_diff(
    n = 300, num_noise = 2,
    min_n = -0.05
  ))
  testthat::expect_error(two_long_clust_diff(
    n = 300, num_noise = 2,
    max_n = 0.05
  ))
  testthat::expect_snapshot(two_long_clust_diff(n = 300, num_noise = 0))
})
