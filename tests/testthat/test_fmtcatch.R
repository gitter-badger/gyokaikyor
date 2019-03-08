library(gyokaikyor)
context("Load catch data from Excel spreadhseet and tidy it up")

test_that("fmtcatch.yamaguchi() work correctly", {
  path <- "ExcelFiles/test_catch_yamaguchi.xlsx"
  df   <- fmtcatch.yamaguchi(path, spcs = "katakuchi")
  expect_is(df, "data.frame")

  expect_setequal(dplyr::filter(df,
                                year == 2018,
                                month == 1,
                                type == "sukui") %>%
                  dplyr::pull(catch),
                  seq(33, 36))
  expect_setequal(unique(df$type), c("sukui", "bouuke"))
})

test_that("fmtcatch.fukuoka() work correctly", {
  path <- "ExcelFiles/test_catch_fukuoka.xlsx"
  df   <- fmtcatch.fukuoka(path, spcs = "katakuchi", type = "maki")
  expect_is(df, "data.frame")
  expect_setequal(subset(df, year == 1977)$catch,
                  c(18, 51, 84, 117, 150, 183, 216, 249))
  expect_setequal(subset(df, year == 1978)$catch,
                  c(315, 348, 381, 414, 447, 480, 513, 546))
  expect_equal(unique(df$type), "maki")
  expect_setequal(unique(df$month), 4:12)
})

test_that("fmtcatch.kumamoto() processes makiami data correctly", {
  path <- "ExcelFiles/test_catch_kumamoto_maki.xls"
  df   <- fmtcatch.kumamoto(path, spcs = "katakuchi", type = "maki")
  expect_is(df, "data.frame")
  expect_equal(subset(df, year == 1989)$catch, 1:9)
  expect_equal(subset(df, year == 1990)$catch, 10:21)
  expect_equal(unique(df$type), "maki")
  expect_setequal(unique(df$month), 1:12)
})

test_that("fmtcatch.kumamoto() processes bouukeami data correctly", {
  path <- "ExcelFiles/test_catch_kumamoto_bouuke.xls"
  df   <- fmtcatch.kumamoto(path, spcs = "katakuchi", type = "bouuke")
  expect_is(df, "data.frame")
  expect_equal(subset(df, year == 1993)$catch, 1:7)
  expect_equal(subset(df, year == 1994)$catch, 8:14)
  expect_equal(unique(df$type), "bouuke")
  expect_setequal(unique(df$month), 6:12)
})

test_that("fmtcatch.saga() prosesses saga data correctly", {
  path <- "ExcelFiles/test_catch_saga.xls"
  df   <- fmtcatch.saga(path, spcs = "katakuchi", type = "kennai")
  expect_is(df, "data.frame")
  expect_equal(subset(df, year == 1975)$catch, 1:9)
  expect_equal(subset(df, year == 1976)$catch, 10:21)
  expect_equal(unique(df$type), "kennai")
  expect_setequal(unique(df$month), 1:12)
})

# test_that("fmtcatch.nagasaki() prosesses nagasaki data correctly", {
#   path <- "ExcelFiles/test_catch_ngs_nagasaki_iwashi.xls"
#   df   <- fmtcatch.nagasaki(path, spcs = "katakuchi")
#   expect_is(df, "data.frame")
#   expect_setequal(dplyr::filter(df,
#                                 year == 2017,
#                                 dplyr::between(month, 2, 4)) %>%
#                   dplyr::pull(catch),
#                   c(0.123, 0.456, 0.789))
# })

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
