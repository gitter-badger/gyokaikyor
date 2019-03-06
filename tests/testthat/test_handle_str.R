context("Handle string")

target <-
  c("漁獲年月日", "", "2017.4.20", "操業海域", "八代海", "漁法", "まき網",
  "漁獲年月日", "", "2017.5.19", "操業海域", "八代海", "漁法", "まき網",
  "漁獲年月日", "", "2017.6.23", "操業海域", "八代海", "漁法", "まき網")

test_that("get_col2load() works well",
          expect_equal(get_col2load(target,
                          regex = "20[0-9]{2}\\.[0-9][0-9]?\\.[0-9][0-9]?",
                          offset = -2),
                       c(1, 8, 15))
          )

test_that("parse_ym() works well", {
  expect_setequal(parse_ym("2012.01-2012.09") %>% unlist(),
                  c(2012, 1, 2012, 9))
  expect_setequal(parse_ym("foo/bar/2012.01-2012.09") %>% unlist(),
                  c(2012, 1, 2012, 9))
  expect_error(parse_ym("20012.01-2012.09") %>% unlist(),
               "Failed parsing to year", fix = TRUE)
  expect_error(parse_ym("foo/bar/20012.01-2012.09") %>% unlist(),
               "Failed parsing to year", fix = TRUE)
})

test_that("get_port() extract port name correctly", {
  str1 <- "地名 ： （Ｈ．14）長崎魚市 漁業種 ： 中小型まき網"
  str2 <- "地名 ： （Ｈ．14長崎魚市 漁業種 ： 中小型まき網"
  str3 <- "地名 ： （Ｈ．8)小佐々町漁協 漁業種 ： 中小型まき網"
  str4 <- "地名 ： （Ｈ.29）長崎魚市      漁業種 ：   中小型まき網"
  expect_equal(get_port(str1), "nagasaki")
  expect_equal(get_port(str2), "nagasaki")
  expect_equal(get_port(str3), "kosasa")
  expect_equal(get_port(str4), "nagasaki")
})
