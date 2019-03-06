context("Load catch data from Excel spreadhseet and tidy it up")

test_that("fmtcatch.kumamoto() process makiami data correctly", {
  path <- "ExcelFiles/test01_H30まき網漁獲量（熊本県）.xls"
  df   <- fmtcatch.kumamoto(path, spcs = "katakuchi")
  expect_is(df, "data.frame")
  expect_equal(subset(df, year == 1989)$catch, 1:9)
  expect_equal(subset(df, year == 1990)$catch, 10:21)
  expect_equal(unique(df$type), "maki")
  expect_setequal(unique(df$month), 1:12)
})

test_that("fmtcatch.kumamoto() process bouukeami data correctly", {
  path <- "ExcelFiles/test02_H30棒受網漁獲量（熊本県）.xls"
  df   <- fmtcatch.kumamoto(path, spcs = "katakuchi")
  expect_is(df, "data.frame")
  expect_equal(subset(df, year == 1993)$catch, 1:7)
  expect_equal(subset(df, year == 1994)$catch, 8:14)
  expect_equal(unique(df$type), "bouuke")
  expect_setequal(unique(df$month), 6:12)
})

test_that("fmtcatch.kagoshima() works well", {
  path <- "ExcelFiles/test_catch_kagoshima.xlsx"
  expect_is(fmtcatch.kagoshima(path, spcs = "katakuchi", spread = TRUE),
            "data.frame")
  expect_is(fmtcatch.kagoshima(path, spcs = "katakuchi", spread = FALSE),
            "data.frame")
  expect_is(fmtcatch.kagoshima(path, spcs = "maiwashi", spread = TRUE),
            "data.frame")
  expect_is(fmtcatch.kagoshima(path, spcs = "maiwashi", spread = FALSE),
            "data.frame")
  expect_is(fmtcatch.kagoshima(path, spcs = "maiwashi",
                               spread = TRUE, maki.only = TRUE),
            "data.frame")
  expect_is(fmtcatch.kagoshima(path, spcs = "maiwashi",
                               spread = FALSE, maki.only = TRUE),
            "data.frame")
})
