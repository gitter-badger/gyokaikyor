context("Be friendly with legacy files")

test_that("conv2hoshifile() converts seimitsu data correctly", {
  df     <- read.csv("ExcelFiles/test_conv2hoshifile_seimitsu.csv")
  hoshi  <- conv2hoshifile(df, prefec = "toyama", type = "seimitsu",
                           ym.start = 196901, ym.end = 196912)
  expect_is(hoshi, "data.frame")
  expect_equal(hoshi[, 1], c(1, rep(0, 29)))
  expect_equal(hoshi[, 2], c(0, 1, rep(0, 28)))
})

test_that("conv2hoshifile() converts taichou data correctly", {
  df     <- read.csv("ExcelFiles/test_conv2hoshifile_taichou.csv")
  hoshi  <- conv2hoshifile(df, prefec = "nagasaki", type = "taichou",
                           ym.start = 196901, ym.end = 196912,
                           export = TRUE, fname = "foo.csv")
  expect_is(hoshi, "data.frame")
  expect_equal(hoshi[, 1], c(1, rep(0, 29)))
  expect_equal(hoshi[, 2], c(0, 1, rep(0, 28)))
  file.remove("foo.csv")
})

test_that("iwashi2list() converts 'kakukeniwashi' file correctly", {
  fname     <- "ExcelFiles/test_kakukeniwashi.xlsx"
  urume     <- iwashi2list(fname, sheet = "ウルメイワシ",
                           year.start = 1992, year.end = 2019)
  maiwashi  <- iwashi2list(fname, sheet = "マイワシ",
                           year.start = 1992, year.end = 2019)
  katakuchi <- iwashi2list(fname, sheet = "カタクチイワシ",
               year.start = 1992, year.end = 2019)
  expect_equal(urume$山口[1, "Jan"], 1)
  expect_equal(maiwashi$山口[1, "Jan"], 11)
  expect_equal(katakuchi$山口[1, "Jan"], 21)
  expect_equal(urume$鹿児島[1, "Jan"], 51)
  expect_equal(maiwashi$鹿児島[1, "Jan"], 61)
  expect_equal(katakuchi$鹿児島[1, "Jan"], 71)
})

test_that("summarize_seikai() and iwashi2df()
 hacks 'kakukeniwashi' file correctly", {
  fname     <- "ExcelFiles/test_kakukeniwashi.xlsx"
  katakuchi <- summarize_seikai(iwashi2list(fname, sheet = "カタクチイワシ",
               year.start = 1992, year.end = 2019))
  expect_equal(katakuchi[1, "Jan"], 276)
  expect_equal(katakuchi[28, "Jan"], 2220)

  katakuchi_df <- iwashi2df(katakuchi)
  expect_equal(katakuchi_df[1, "catch"], 276)
  expect_equal(katakuchi_df[2, "catch"], 282)
})

test_that("export2kakuken_iwashi() converts df to 'iwashi' style", {
  df     <- data.frame(year = c(rep(1969, 12), rep(1970, 12)),
                   month = rep(1:12, 2), catch = 11:34)
  iwashi <- export2kakuken_iwashi(df, export.csv = TRUE, fname = "bar.csv") %>%
    as.data.frame()
  expect_equal(iwashi[1, 2], 11)
  expect_equal(iwashi[2, 2], 23)
  file.remove("bar.csv")
})

test_that("make_ymrange() makes proper ym range", {
  ymrange <- make_ymrange(1969, "Mar")
  expect_equal(ymrange$start, 196804)
  expect_equal(ymrange$end, 196903)
  expect_error(make_ymrange(1969, "Jan"), "Unknown month")
})

test_that("get_catch() pulls total catch data during fishing period", {
  fname <- "ExcelFiles/test_kakukeniwashi.xlsx"
  l     <- iwashi2list(fname, year.end = 2019, sheet = "カタクチイワシ")
  summarize_seikai(l)
  expect_equal(get_catch(2017, l, "Mar"), c(seq(2094, 2124, by = 6)))
  expect_equal(get_catch(2018, l, "Mar"), c(seq(2166, 2196, by = 6)))
})

test_that("make_summary() returns catch summary during fishing period", {
  fname <- "ExcelFiles/test_kakukeniwashi.xlsx"
  l     <- iwashi2list(fname, year.end = 2019, sheet = "カタクチイワシ")
  expect_equal(make_summary(l, 2018, "Mar")$last, 2109)
  expect_equal(make_summary(l, 2018, "Mar")$recent, 1965)
  expect_equal(make_summary(l, 2019, "Mar")$last, 2181)
  expect_equal(make_summary(l, 2019, "Mar")$recent, 2037)
})
