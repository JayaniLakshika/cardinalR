test_that("tri_3d() works", {
  set.seed(20240412)
  testthat::expect_snapshot(tri_3d(
    n = 100, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(tri_3d(
    n = -100, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(tri_3d(
    n = 100, num_noise = -2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(tri_3d(
    num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(tri_3d(
    n = -100, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(tri_3d(n = 100, num_noise = 2, max_n = 0.05))
  testthat::expect_error(tri_3d(n = 100, num_noise = 2, min_n = -0.05))
})


test_that("tri_plane_bkg() works", {
  set.seed(20240412)
  testthat::expect_snapshot(tri_plane_bkg(
    n = 216, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))

  testthat::expect_error(tri_plane_bkg(
    n = -216, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(tri_plane_bkg(
    n = 216, num_noise = -2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(tri_plane_bkg(
    num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(tri_plane_bkg(
    n = -216, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(tri_plane_bkg(n = 216, num_noise = 2, max_n = 0.05))
  testthat::expect_error(tri_plane_bkg(n = 216, num_noise = 2, min_n = -0.05))
})
