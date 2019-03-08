#' Load catch data of kumamoto and tidy it up
#'
#' @param path File path to process
#' @param spcs Romaji spcs name one of
#' \itemize{
#'   \item{"maaji"}
#'   \item{"maiwashi"}
#'   \item{"sabarui"}
#'   \item{"katakuchi"}
#'   \item{"urume"}
#' }
#' @param type Character value either "maki" or "bouke".
#'   Data processing algorithm is controled by this parameter.
fmtcatch.kumamoto <- function(path, spcs, type) {
  sheet   <- switch(spcs,
                    "maaji" = "マアジ",
                    "maiwashi" = "マイワシ",
                    "sabarui" = "サバ類",
                    "katakuchi" = "カタクチイワシ",
                    "urume" = "ウルメイワシ")
  alldata <- load_alldata(path, sheet)
  out     <- get_data(alldata, type, key.start = "年度計", key.end = "前年比")
  out
}
#' Load catch data of saga and tidy it up
#'
#' @inheritParams fmtcatch.kumamoto
#' @param spcs Romaji spcs name one of
#' \itemize{
#'   \item{"maaji"}
#'   \item{"maiwashi"}
#'   \item{"masaba"}
#'   \item{"katakuchi"}
#'   \item{"urume"}
#' }
#' @param type Character value one of
#' \itemize{
#'   \item{"karatsu"}
#'   \item{"kennai"}
#'   \item{"teichi"}
#'   \item{"chumaki"}
#' }
#'   Data processing algorithm is controled by this parameter.
fmtcatch.saga <- function(path, spcs, type) {
  spcs_jp <- switch(spcs,
                    "maaji" = "ﾏｱｼﾞ",
                    "maiwashi" = "ﾏｲﾜｼ",
                    "masaba" = "ﾏｻﾊﾞ",
                    "katakuchi" = "ｶﾀｸﾁ",
                    "urume" = "ｳﾙﾒ")
  type_jp <- switch(type,
                    "karatsu" = "（唐津港）",
                    "kennai" = "（唐津港県内船）",
                    "teichi" = "（定置）",
                    "chumaki" = "（中まき）")
  sheets  <- readxl::excel_sheets(path)
  regex   <- paste0(spcs_jp, type_jp)
  sheet   <- sheets[stringr::str_detect(sheets, regex)]
  alldata <- load_alldata(path, sheet)
  out     <- get_data(alldata, type, key.start = "年度", key.end = "平年値")
  out
}

get_data <- function(df, type, key.start, key.end) {
  switch(type,
         "maki" = {
           judgecol_start <- 18
           judgecol_end   <- 3
           cols           <- 4:15
           months         <- c(4:12, 1:3)
           yrcol          <- 3
         },
         "bouuke" = {
           judgecol_start <- 13
           judgecol_end   <- 2
           cols           <- 4:10
           months         <- 6:12
           yrcol          <- 3
         }
       , {
           if (!type %in% c("karatsu", "kennai", "teichi", "chumaki"))
             stop("Unknown type")
           judgecol_start <- 1
           judgecol_end   <- 16
           cols           <- 3:14
           months         <- c(4:12, 1:3)
           yrcol          <- 1
         })
  str_judge_start <- dplyr::pull(df, judgecol_start)
  str_judge_end   <- dplyr::pull(df, judgecol_end)
  row_start       <- get_row(str_judge_start, key.start, offset = 1)
  row_end         <- get_row(str_judge_end, key.end, offset = -1)
  rows            <- row_start:row_end
  year            <- dplyr::pull(df, yrcol)[rows] %>%
    as.numeric()
  out             <- NULL
  out             <- df[rows, cols]
  colnames(out)   <- months
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
