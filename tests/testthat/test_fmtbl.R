library(gyokaikyor)
context("Load blhist data from Excel spreadhseet and tidy it up")

test_that("fmtbl() works well", {
  path <- "ExcelFiles/2017.09-2018.01_test_bl_nagasaki.xls"
  class(path) <- "nagasaki"
  expect_is(fmtbl(path, spcs = "katakuchi", nest = TRUE), "data.frame")
})

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

test_that("rename_class() make blclass from start of class and bin", {
  expect_equal(rename_class(10, 5), "[10,15)")
  expect_equal(rename_class(20, 5), "[20,25)")
  expect_equal(rename_class(0, 1), "[0,1)")
  expect_equal(rename_class(1, 2), "[1,3)")
})

test_that("fmtbl_fresco() tidy fresco data", {
  fname_taichou <- "ExcelFiles/test_fresco_taichou.csv"
  fname_seimitsu <- "ExcelFiles/test_fresco_seimitsu.csv"
  taichou  <- fmtbl_fresco(fname_taichou, type = "taichou",
                           date.start = "20180101", date.end = "20190331")
  seimitsu <- fmtbl_fresco(fname_seimitsu, type = "seimitsu",
                          date.start = "20180101", date.end = "20190331")
  expect_is(taichou, "data.frame")
  expect_is(seimitsu, "data.frame")
})
