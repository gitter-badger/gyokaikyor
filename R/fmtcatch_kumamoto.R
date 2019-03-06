fmtcatch.kumamoto <- function(path, spcs) {
  get_type <- function(path) {
    out   <- NULL
    regex <- "(?:H\\d+)(\\w+)(?=網漁獲量)"
    match <- stringr::str_match(path, regex)[2]
    match
    out   <- switch(match,
                    "まき" = "maki",
                    "棒受" = "bouuke")
    out
  }

  get_row <- function(str) {
    match <- stringr::str_which(str, "前年比")
    if (length(match) > 0) {
      out <- 4:(match - 1)
      out
    }
  }

  get_data <- function(df, type) {
    switch(type,
           "maki" = {
             judgecol <- 3
             cols     <- 4:15
             months   <- c(4:12, 1:3)
           },
           "bouuke" = {
             judgecol <- 2
             cols     <- 4:10
             months   <- 6:12
           })
    yrcol         <- 3
    out           <- NULL
    ystr          <- dplyr::pull(df, judgecol)
    rows          <- get_row(ystr)
    year          <- dplyr::pull(df, yrcol)[rows] %>%
      as.numeric()
    out           <- df[rows, cols]
    colnames(out) <- months
    out %<>% dplyr::mutate(year = year,
                           type = type) %>%
      tidyr::gather(-year, -type, key = month, value = "catch") %>%
      dplyr::mutate(year = as.integer(year),
                    month = as.integer(month),
                    catch = as.double(catch)) %>%
      dplyr::select(year, month, type, catch) %>%
      dplyr::mutate(year = ifelse(dplyr::between(month, 1, 3),
                                  year + 1,
                                  year)) %>%
      dplyr::arrange(year, month)
    out
  }

  sheet   <- switch(spcs,
                    "maaji" = "マアジ",
                    "maiwashi" = "マイワシ",
                    "sabarui" = "サバ類",
                    "katakuchi" = "カタクチイワシ",
                    "urume" = "ウルメイワシ")
  type    <- get_type(path)
  alldata <- load_alldata(path, sheet)
  out     <- get_data(alldata, type)
  out
}
