context("Load blhist data from Excel spreadhseet and tidy it up")

test_that("fmtbl.nagasaki() works well", {
  path <- "ExcelFiles/2017.09-2018.01_test_bl_nagasaki.xls"
  expect_is(fmtbl.nagasaki(path, spcs = "katakuchi", nest = TRUE),
            "data.frame")
  expect_is(fmtbl.nagasaki(path, spcs = "katakuchi", nest = FALSE),
            "data.frame")
})

test_that("fmtbl.kumamoto() works well", {
  path <- "ExcelFiles/test_bl_kumamoto.xlsx"
  expect_is(fmtbl.kumamoto(path, spcs = "katakuchi", nest = TRUE),
            "data.frame")
  expect_is(fmtbl.kumamoto(path, spcs = "katakuchi", nest = FALSE),
            "data.frame")
})

test_that("fmtbl.kagoshima() works well", {
  path <- "ExcelFiles/test_bl_kagoshima.xlsx"
  expect_is(fmtbl.kagoshima(path, spcs = "katakuchi", nest = TRUE),
            "data.frame")
  expect_is(fmtbl.kagoshima(path, spcs = "katakuchi", nest = FALSE),
            "data.frame")
})
