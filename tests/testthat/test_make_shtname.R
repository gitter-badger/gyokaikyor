context("Make shtname of prefecture")

test_that("make_shtname() makes sheetname for kumamoto data", {
  expect_equal(make_shtname(prefec = "kumamoto", spcs = "katakuchi"), "カタクチ")
  expect_equal(make_shtname(prefec = "kumamoto", spcs = "urume"), "ウルメ")
  expect_equal(make_shtname(prefec = "kumamoto", spcs = "maiwashi"), "マイワシ")
  expect_equal(make_shtname(prefec = "kumamoto", spcs = "sabarui"), "サバ類")
  expect_error(make_shtname(prefec = "kumamoto", spcs = "foo"),
               "Unknown spcs name")
})

test_that("make_shtname() makes sheetname for nagasaki data", {
  expect_equal(make_shtname(prefec = "nagasaki", spcs = "katakuchi"), "カタクチ")
  expect_equal(make_shtname(prefec = "nagasaki", spcs = "urume"), "ウルメ")
  expect_equal(make_shtname(prefec = "nagasaki", spcs = "maiwashi"), "マイワシ")
  expect_equal(make_shtname(prefec = "nagasaki", spcs = "masaba"), "マサバ")
  expect_equal(make_shtname(prefec = "nagasaki", spcs = "gomasaba"), "ゴマサバ")
  expect_equal(make_shtname(prefec = "nagasaki", spcs = "maaji"), "マアジ")
  expect_error(make_shtname(prefec = "nagasaki", spcs = "foo"),
               "Unknown spcs name")
})

test_that("make_shtname() makes sheetname for kagoshima data", {
  expect_equal(make_shtname(prefec = "kagoshima", spcs = "katakuchi"), "ｶﾀｸﾁ")
  expect_equal(make_shtname(prefec = "kagoshima", spcs = "urume"), "ｳﾙﾒ")
  expect_equal(make_shtname(prefec = "kagoshima", spcs = "maiwashi"), "ﾏｲﾜｼ")
  expect_equal(make_shtname(prefec = "kagoshima", spcs = "masaba"), "ﾏｻﾊﾞ")
  expect_equal(make_shtname(prefec = "kagoshima", spcs = "gomasaba"), "ｺﾞﾏｻﾊﾞ")
  expect_equal(make_shtname(prefec = "kagoshima", spcs = "maaji"), "ﾏｱｼﾞ")
  expect_error(make_shtname(prefec = "kagoshima", spcs = "foo"),
               "Unknown spcs name")
})

test_that("make_shtname() stops for unknown prefecture", {
  expect_error(make_shtname(prefec = "foo", spcs = "katakuchi"),
               "Unknown prefecture")
})
