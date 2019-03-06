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
  library(stringi)
  str1 <- stri_unescape_unicode(paste0("\\u5730\\u540d \\uff1a \\uff08",
                                       "\\uff28\\uff0e14\\uff09\\u9577",
                                       "\\u5d0e\\u9b5a\\u5e02 \\u6f01",
                                       "\\u696d\\u7a2e \\uff1a \\u4e2d",
                                       "\\u5c0f\\u578b\\u307e\\u304d",
                                       "\\u7db2"))
  str2 <- stri_unescape_unicode(paste0("\\u5730\\u540d \\uff1a \\uff08",
                                       "\\uff28\\uff0e14\\uff09\\u9577",
                                       "\\u5d0e\\u9b5a\\u5e02 \\u6f01",
                                       "\\u696d\\u7a2e \\uff1a \\u4e2d",
                                       "\\u5c0f\\u578b\\u307e\\u304d",
                                       "\\u7db2"))
  str3 <- stri_unescape_unicode(paste0("\\u5730\\u540d \\uff1a \\uff08",
                                       "\\uff28\\uff0e14\\u9577\\u5d0e",
                                       "\\u9b5a\\u5e02 \\u6f01\\u696d",
                                       "\\u7a2e \\uff1a \\u4e2d\\u5c0f",
                                       "\\u578b\\u307e\\u304d\\u7db2"))
  str4 <- stri_unescape_unicode(paste0("\\u5730\\u540d \\uff1a \\uff08",
                                       "\\uff28\\uff0e8)\\u5c0f\\u4f50",
                                       "\\u3005\\u753a\\u6f01\\u5354 ",
                                       "\\u6f01\\u696d\\u7a2e \\uff1a ",
                                       "\\u4e2d\\u5c0f\\u578b\\u307e",
                                       "\\u304d\\u7db2"))
  str5 <- stri_unescape_unicode(paste0("\\u5730\\u540d \\uff1a \\uff08",
                                       "\\uff28.29\\uff09\\u9577\\u5d0e",
                                       "\\u9b5a\\u5e02      \\u6f01",
                                       "\\u696d\\u7a2e \\uff1a   ",
                                       "\\u4e2d\\u5c0f\\u578b\\u307e",
                                       "\\u304d\\u7db2\\u3000\\u3000"))
  expect_equal(get_port(str1), "nagasaki")
  expect_equal(get_port(str2), "nagasaki")
  expect_equal(get_port(str3), "nagasaki")
  expect_equal(get_port(str4), "kosasa")
  expect_equal(get_port(str5), "nagasaki")
})
