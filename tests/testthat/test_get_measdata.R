context("Get measure data vector from data frame")

test_that("get_measdata() extracts vector correctly",{
  df <- data.frame(a = 1:200, b = 101:300, c = 201:400)
  expect_equal(get_measdata(df, 1), 8:107)
  expect_equal(get_measdata(df, 2), 108:207)
  expect_equal(get_measdata(df, 3), 208:307)
})
