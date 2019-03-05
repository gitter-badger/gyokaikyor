context("Load catch data from xlsheet and tidy it up")

test_that("fmtcatch.nagasaki() works well", {
  nagasaki <- "ExcelFiles/test_catch_ngs_nagasaki_iwashi.xls"
  expect_is(fmtcatch.nagasaki(nagasaki), "data.frame")
})
