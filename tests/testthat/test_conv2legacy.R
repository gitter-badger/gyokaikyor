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
                           ym.start = 196901, ym.end = 196912)
  expect_is(hoshi, "data.frame")
  expect_equal(hoshi[, 1], c(1, rep(0, 29)))
  expect_equal(hoshi[, 2], c(0, 1, rep(0, 28)))
})

test_that("iwashi2list() converts 'kakukeniwashi' file correctly", {
  fname <- "ExcelFiles/test_kakukeniwashi.xlsx"
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
