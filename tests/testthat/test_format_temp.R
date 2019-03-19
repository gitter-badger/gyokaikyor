context("Format TW sst data")

test_that("format_temp() loads and format temp data correctly", {
  fname     <- "ExcelFiles/test_sst.xlsx"
  data_2000 <- format_temp(fname, year.end = 2000)
  data_2018 <- format_temp(fname, year.end = 2018)

  expect_equal(tail(data_2000, 1)$year, 2000)
  expect_equal(tail(data_2000, 1)$tw_degc, 606)
  expect_equal(tail(data_2018, 1)$year, 2018)
  expect_equal(tail(data_2018, 1)$tw_degc, 822)
})
