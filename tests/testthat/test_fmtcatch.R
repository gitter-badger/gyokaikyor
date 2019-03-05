context("Load catch data from Excel spreadhseet and tidy it up")

test_that("fmtcatch.kagoshima() works well", {
  path <- "test_catch_kagoshima.xlsx"
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
