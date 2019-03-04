context("Load blhist data from Excel spreadhseet and tidy it up")

test_that("fmtbl.kaagoshima() works well", {
  path <- "./test_bl_kagoshima.xlsx"
  expect_is(fmtbl.kagoshima(path, spcs = "katakuchi", nest = TRUE), "data.frame")
  expect_is(fmtbl.kagoshima(path, spcs = "katakuchi", nest = FALSE), "data.frame")
})
