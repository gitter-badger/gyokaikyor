context("Load catch data from Excel spreadhseet and tidy it up")

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

test_that("fmtcatch.nagasaki() works well", {
  path <- "ExcelFiles/test_catch_ngs_nagasaki_iwashi.xls"
  expect_is(fmtcatch.nagasaki(path, spcs = "katakuchi"), "data.frame")
})
