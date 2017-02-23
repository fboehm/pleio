library(pleiotropy)
library(tibble)
context("Unit tests for calc_lod in package pleiotropy")
set.seed(2017-02-23)
foo <- runif(25, max = 5)
test_that("calc_lod n_mouse argument multiplies lods as expected", {
  dat <- tibble(marker1 = rep(1:5, each = 5), marker2 = rep(1:5, 5), log10detrss = foo)
  expect_equal(calc_lod(dat, n_mouse = 100), calc_lod(dat, n_mouse = 10) * 10)
})

