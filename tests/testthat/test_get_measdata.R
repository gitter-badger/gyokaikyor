context("Get measure data vector from data frame")

test_that("get_vector() extracts vector correctly", {
  df <- data.frame(a = 1:200, b = 101:300, c = c(201:250, NA, 252:400))
  expect_equal(get_vector(1, df, 10, 20, na.rm = TRUE), 10:20)
  expect_equal(get_vector(2, df, 50, 60, na.rm = TRUE), 150:160)
  expect_equal(get_vector(3, df, 50, 60, na.rm = TRUE), c(250, 252:260))
  expect_equal(get_vector(3, df, 50, 60, na.rm = FALSE), c(250, 0, 252:260))
})
