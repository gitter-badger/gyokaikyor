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

test_that("check_month() detects bad month data", {
  path     <- "ExcelFiles/2017.09-2018.01_test_bl_nagasaki_month.xls"
  alldata  <- load_alldata(path, sheet = "カタクチ")
  colpos   <- get_col2load(target = alldata[4, ],
                           regex = ".\u6708", # "tsuki" in jp kanji
                           offset = 0)
  months   <- jpmonth2num(alldata[4, colpos])
  parsedym  <- parse_ym(path)
  check_month(months, parsedym$month_start, parsedym$month_end)
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
