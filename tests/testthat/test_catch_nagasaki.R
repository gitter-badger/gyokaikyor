library(gyokaikyor)
context("Formating nagasaki catch data")

test_that("make_hougan() creates vector houganshi", {
  str1 <- rep(1:10, 4) %>%
    replace(which(. %% 3  == 0), NA) %>%
    replace(which(. %% 5  == 0), "foo") %>%
    as.character()
  str2 <- c("いち", "に", "さん", "し",
            NA, "ろく", "なな", "はち", NA, "じゅう")
  str3 <- c("カ", NA, "タ", NA, "ク", NA, "チ", "イ", "ワ", "シ")
  expect_equal(make_hougan(str1), "12 4  78  12 4  78  12 4  78  12 4  78  ")
  expect_equal(make_hougan(str2), " に し      ")
  expect_equal(make_hougan(str3), "カ タ ク チイワシ")
})

test_that("ngs_locate_spcsrow(), locates row position", {
  regex <- "カ( |　)*タ( |　)*ク( |　)*チ"
  str1  <- c("カタクチ", NA, "カ タ ク チ", NA, "カ　タ　ク　チ")
  str2  <- c("カ", NA, "タ", NA, "ク", NA, "チ",
             NA, "foo", NA, "カ", "タ", "ク", "チ")
  expect_equal(ngs_locate_spcsrow(regex, str1), c(1, 3, 5))
  expect_equal(ngs_locate_spcsrow(regex, str2), c(1, 11))
})

test_that("ngs_get_monthcol() detect month column", {
  df <- tibble::tribble(~A, ~B, ~C, ~D, ~E, ~F, ~G,
                        "foo", "bar", "3月", "baz", "４　月", "bum", "5　月",
                        1, 2, 3, 4, 5, 6, 7,
                        8, 9, 10, 11, 12, 13, 14)
  expect_equal(ngs_get_monthcol(1, df), data.frame(row = c(1, 1),
               col = c(5, 7)))
  expect_equal(ngs_get_monthcol(2, df), data.frame(row = c(1, 1),
               col = c(5, 7)))
})

test_that("ngs_get_colvalue(), gets values correctly", {
  df <- tibble::tribble(
      ~A, ~B, ~C, ~D, ~E, ~F, ~G, ~H,
      "foo", "bar", "3月", "baz", "４　月", "bum", "5　月", "boo",
      "カタクチイワシ", 2, 3, 4, 5, 6, 7, 8,
      9, 10, 11, 12, 13, 14, 15, 16,
      17, 18, 19, 20, 21, 22, 23, 24,
      "foo", "bar", "3月", "baz", "４　月", "bum", "5　月", "boo",
      "カタクチイワシ", 26, 27, 28, 29, 30, 31, 32,
      33, 34, 35, 36, 37, 38, 39, 40
    )
  expect_setequal(
    ngs_get_colvalue(regex = "カタクチイワシ", df = df,
                     offset.x = 1, offset.y = 1, xtract.digit = TRUE),
                     c(6, 8, 30, 32))
  expect_setequal(
    ngs_get_colvalue(regex = "カタクチイワシ", df = df,
                     offset.x = 1, offset.y = 2, xtract.digit = TRUE),
                     c(14, 16, 38, 40))
})

test_that("ngs_make_yrvec() makes year vector correctly", {
  expect_equal(ngs_make_yrvec("2018.11-2019.03", c(11, 12, 1, 2, 3)),
               c(rep(2018, 2), rep(2019, 3)))
  expect_equal(ngs_make_yrvec("2019.01-2019.03", c(1, 2, 3)),
               rep(2019, 3))
})
