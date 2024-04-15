test_that("conic_spiral_3d() works", {
  set.seed(20240412)
  testthat::expect_snapshot(conic_spiral_3d(
    n = 100, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))

  testthat::expect_error(conic_spiral_3d(
    n = -100, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(conic_spiral_3d(
    n = 100, num_noise = -2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(conic_spiral_3d(
    num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(conic_spiral_3d(
    n = 100, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(conic_spiral_3d(n = 100, num_noise = 2, min_n = -0.05))
  testthat::expect_error(conic_spiral_3d(n = 100, num_noise = 2, max_n = 0.05))
  testthat::expect_snapshot(conic_spiral_3d(n = 100, num_noise = 0))
})

test_that("dini_surface_3d() works", {
  set.seed(20240412)
  testthat::expect_snapshot(dini_surface_3d(
    n = 100, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))

  testthat::expect_error(dini_surface_3d(
    n = -100, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(dini_surface_3d(
    n = 100, num_noise = -2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(dini_surface_3d(
    num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(dini_surface_3d(
    n = 100, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(dini_surface_3d(n = 100, num_noise = 2, min_n = -0.05))
  testthat::expect_error(dini_surface_3d(n = 100, num_noise = 2, max_n = 0.05))
  testthat::expect_snapshot(dini_surface_3d(n = 100, num_noise = 0))
})

test_that("roman_surface_3d() works", {
  set.seed(20240412)
  testthat::expect_snapshot(roman_surface_3d(
    n = 100, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))

  testthat::expect_error(roman_surface_3d(
    n = -100, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(roman_surface_3d(
    n = 100, num_noise = -2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(roman_surface_3d(
    num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(roman_surface_3d(
    n = 100, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(roman_surface_3d(n = 100, num_noise = 2, min_n = -0.05))
  testthat::expect_error(roman_surface_3d(n = 100, num_noise = 2, max_n = 0.05))
  testthat::expect_snapshot(roman_surface_3d(n = 100, num_noise = 0))
})

test_that("spiral_3d() works", {
  set.seed(20240412)
  testthat::expect_snapshot(spiral_3d(
    n = 100, num_dims = 6, num_noise = 2,
    min_n = -0.05, max_n = 0.05
  ))

  testthat::expect_error(spiral_3d(
    n = -100, num_dims = 6, num_noise = 2,
    min_n = -0.05, max_n = 0.05
  ))
  testthat::expect_error(spiral_3d(
    n = 100, num_dims = -6, num_noise = 2,
    min_n = -0.05, max_n = 0.05
  ))
  testthat::expect_error(spiral_3d(
    n = 100, num_dims = 6, num_noise = -2,
    min_n = -0.05, max_n = 0.05
  ))
  testthat::expect_error(spiral_3d(
    num_dims = 6, num_noise = 2,
    min_n = -0.05, max_n = 0.05
  ))
  testthat::expect_error(spiral_3d(
    n = 100, num_noise = 2,
    min_n = -0.05, max_n = 0.05
  ))
  testthat::expect_error(spiral_3d(
    n = 100, num_dims = 6,
    min_n = -0.05, max_n = 0.05
  ))
  testthat::expect_error(spiral_3d(
    n = 100, num_dims = 6, num_noise = 2,
    min_n = -0.05
  ))
  testthat::expect_error(spiral_3d(
    n = 100, num_dims = 6, num_noise = 2,
    max_n = 0.05
  ))
  testthat::expect_snapshot(spiral_3d(n = 100, num_dims = 6, num_noise = 0))
})

test_that("torus_3d() works", {
  set.seed(20240412)
  testthat::expect_snapshot(torus_3d(
    n = 100, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))

  testthat::expect_error(torus_3d(
    n = -100, num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(torus_3d(
    n = 100, num_noise = -2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(torus_3d(
    num_noise = 2, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(torus_3d(
    n = 100, min_n = -0.05,
    max_n = 0.05
  ))
  testthat::expect_error(torus_3d(n = 100, num_noise = 2, min_n = -0.05))
  testthat::expect_error(torus_3d(n = 100, num_noise = 2, max_n = 0.05))
  testthat::expect_snapshot(torus_3d(n = 100, num_noise = 0))
})

test_that("cube_3d() works", {
  set.seed(20240412)
  testthat::expect_snapshot(cube_3d(
    num_dims = 3, num_noise = 2, min_n = -0.01,
    max_n = 0.01
  ))
  testthat::expect_length(cube_3d(
    num_dims = 3, num_noise = 2, min_n = -0.01,
    max_n = 0.01
  ), 2)

  testthat::expect_error(cube_3d(
    num_dims = -3, num_noise = 2, min_n = -0.01,
    max_n = 0.01
  ))
  testthat::expect_error(cube_3d(
    num_dims = 3, num_noise = -2, min_n = -0.01,
    max_n = 0.01
  ))
  testthat::expect_error(cube_3d(
    num_noise = 2, min_n = -0.01,
    max_n = 0.01
  ))
  testthat::expect_error(cube_3d(
    num_dims = 3, min_n = -0.01,
    max_n = 0.01
  ))
  testthat::expect_error(cube_3d(num_dims = 3, num_noise = 2, min_n = -0.01))
  testthat::expect_error(cube_3d(num_dims = 3, num_noise = 2, max_n = 0.01))
  testthat::expect_snapshot(cube_3d(num_dims = 3, num_noise = 0))
})
