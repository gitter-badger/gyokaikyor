library(gyokaikyor)
context("Get measure data vector from data frame")

test_that("get_vector() extracts vector correctly", {
  df <- data.frame(a = 1:200, b = 101:300, c = c(201:250, NA, 252:400))
  expect_equal(get_vector(1, 10:20, df, na.rm = TRUE), 10:20)
  expect_equal(get_vector(2, 50:60, df, na.rm = TRUE), 150:160)
  expect_equal(get_vector(3, 50:60, df, na.rm = TRUE), c(250, 252:260))
  expect_equal(get_vector(3, 50:60, df, na.rm = FALSE), c(250, 0, 252:260))
})

test_that("get_measdata() extracts vector correctly", {
  df <- data.frame(kumamoto_a = 1:200, kumamoto_b = c(1:100, NA, 102:200))
  expect_equal(get_measdata(1, df, prefec = "kumamoto"), 8:107)
  expect_equal(get_measdata(2, df, prefec = "kumamoto"), c(8:100, 102:107))
  expect_error(get_measdata(1, df, prefec = "foo"),
               "Unknown prefecture", fix = TRUE)
})

test_that("get_histdata() extracts vector correctly", {
  df <- data.frame(blank = 1:200,
                   class_l = seq(5, 1000, 5), class_r = seq(10, 1005, 5),
                   a = c(1:50, rep(NA, 50), 101:150,
                         sum(c(1:50, 101:150)), rep(NA, 49)))
  expect_equal(get_histdata(4, df, prefec = "nagasaki")[, 2],
               c(5:50, rep(0, 50), 101:150))
  expect_error(get_histdata(1, df, prefec = "kumamoto"),
               "Unknown prefecture", fix = TRUE)
})
