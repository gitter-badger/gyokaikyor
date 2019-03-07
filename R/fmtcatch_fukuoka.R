#' Load and format catch data
#'
#' @param path File path to process
#' @param spcs Romaji spcs name one of
#' \itemize {
#'   \item{"maaji"}
#'   \item{"maiwashi"}
#'   \item{"sabarui"}
#'   \item{"katakuchi"}
#'   \item{"urume"}
#' }
#' @param type Character value either "maki" or "bouuke", and "others".
fmtcatch.fukuoka <- function(path, spcs, type) {
  spcs_jp <- switch(spcs,
                    "maaji"     = "ﾏｱｼﾞ",
                    "maiwashi"  = "ﾏｲﾜｼ",
                    "masaba"    = "ﾏｻﾊﾞ",
                    "gomasaba"  = "ｺﾞﾏｻﾊﾞ",
                    "katakuchi" = "ｶﾀｸﾁ",
                    "urume"     = "ｳﾙﾒ")

  sheet <- switch(type,
                  "maki"   = "まき網 ",
                  "bouuke" = "棒受網",
                  "others" = "その他漁業")
  alldata <- load_alldata(path, sheet)

  ycol     <- 2
  mcol     <- 3
  mstr     <- alldata[[mcol]]
  startrow <- get_row(mstr, "ﾂｷ", offset = 1)
  endrow   <- get_row(mstr, "KEI") %>% max()
  rows     <- startrow:endrow
  datrow   <- stringr::str_count(mstr[startrow:endrow], "KEI") == 0
  spcs_col <- get_spcscol(alldata, spcs_jp)

  out       <- NULL
  out$year  <- col2data(col = ycol, row = rows,
                        row.pick = datrow, df = alldata) %>%
    jpyear2ad(start = "showa")
  out$month <- col2data(col = mcol, row = rows,
                        row.pick = datrow, df = alldata)
  out$type  <- type
  out$catch <- col2data(col = spcs_col, row = rows,
                        row.pick = datrow, df = alldata)
  tibble::as_tibble(out)
}

col2data <- function(col, rows, row.pick, df) {
  out <- df[[col]][rows][row.pick] %>%
    as.numeric()
  out
}

get_spcscol <- function(df, spcs_jp) {
  out <- NULL
  for (i in 2:5) {
    str <- df[i, ] %>%
      unlist() %>%
      as.vector()
    out <- stringr::str_which(str, spcs_jp)
    if (length(out) > 0)
      break
  }
  out
}

jpyear2ad <- function(x, start) {
  conv <- vector(mode = "integer")
  if (start == "showa") {
   suppressMessages(pos_lastyr <- alert_decrease(x))
   x[1:96]
   x[1:97]
   conv[1:pos_lastyr] <- 1925
   conv[1:96]
   conv[1:97]
   conv[(pos_lastyr + 1):length(x)] <- 1988
  } else {
    stop("jpyear2ad")
  }
  ad <- conv + x
  plot(ad)
  ad
}
