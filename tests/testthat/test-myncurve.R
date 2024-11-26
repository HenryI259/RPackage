test_that("area works", {
  expect_equal(myncurve(6, 5, 2)$area, 0.6915)
})

test_that("mean is correct", {
  expect_equal(myncurve(6, 5, 2)$mu, 5)
})

test_that("sd is correct", {
  expect_equal(myncurve(6, 5, 2)$sigma, 2)
})
