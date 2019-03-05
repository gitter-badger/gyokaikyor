#' Load and format catch data
 #'
 #' @inheritParams readxl::read_excel
 #' @param spcs Spcs name in romaji, one of
 #' @param nest If \code{TRUE}, data will be shown in rectangle format
 #'   whith nested catch data for quick overview.
 #' \itemize{
 #'   \item maiwashi
 #'   \item maaji
 #'   \item sabarui
 #'   \item masaba
 #'   \item gomasaba
 #'   \item katakuchi
 #'   \item urume
 #' }
 #' @export
 fmtcatch <- function(path, spcs, nest = FALSE) {
   UseMethod("fmtcatch")
 }
alert_decrease <- function(x) {
  if (any(diff(x) < 0)) {
    stop("There is a decrease in number.")
  } else {
    x
  }
}

fmtcatch.kagoshima <- function(path, spcs, spread = TRUE, maki.only = FALSE) {
  get_ym <- function(str, var) {
    split <- stringr::str_split(str, "\\.")
    if (var == "year") {
      out <- split[[1]][1] %>% as.numeric()
    } else if (var == "month"){
      out <- split[[1]][2] %>% as.numeric()
    }
    out
  }

  make_year <- function(yr_jp, jpera) {
    out <- paste0(jpera, yr_jp, "\u5E74") %>% # "nen" (year) in jp kanji
      Nippon::wareki2AD()
    out
  }

  data  <- load_alldata(path, sheet = "\uFF14\u6E2F\u8A08")
                                        # "4koukei" (four port sum) in jp kanji
  str   <- data[5:16, 1] %>%
    unlist() %>%
    as.vector()
  yr_jp <- tryCatch(
      purrr::map(str, get_ym, var = "year") %>%
        unlist() %>%
        alert_decrease(),
      error = function(c) {
        stop("fmtcatch.kagoshima() must be modified to follow jpera change.")
      },
      warning = function(c) "warning",
      message = function(c) "message"
    )

  years <- make_year(yr_jp, "\u5e73\u6210") # "heisei" in jp kanji

  months <- purrr::map(str, get_ym, var = "month") %>%
    unlist()

  load_catch_4ports <- function(path, spcs) {

    insert_fullspace <- function(str) {
      s   <- "\u3000"
      out <- paste0(s, "+", substr(str, 1, 1))
      for (i in 2:nchar(str)) {
        out <- paste0(out, s, "+", substr(str, i, i))
      }
      out
    }

    spcs_jp <- switch(spcs,
                      "maaji" = "マアジ",
                      "sabarui" = "サバ類",
                      "maiwashi" = "マイワシ",
                      "urume" = "ウルメイワシ",
                      "katakuchi" = "カタクチイワシ",
                      stop("Unknown spcs"))
    regex    <- insert_fullspace(spcs_jp)
    data     <- load_alldata(path, sheet = "４港計")
    col_spcs <- which ( (gregexpr(regex, data[3, ]) > 0) == TRUE)
    col2load <- col_spcs + 4
    out      <- get_vector(col2load, data, 5, 16, na.rm = FALSE)
    out
  }

  load_catch_bouuke <- function(path, spcs, sheet, unit = "ton") {
    data     <- load_alldata(path, sheet)
    spcs_jp <- switch(spcs,
                      "maaji" = "マアジ",
                      "sabarui" = "サバ類",
                      "maiwashi" = "マイワシ",
                      "urume" = "ウルメ",
                      "katakuchi" = "カタクチ",
                      stop("Unknown spcs"))
    cols_spcs <- which ( (gregexpr(spcs_jp, data[32, ]) > 0) == TRUE)
    col_kg    <- cols_spcs[2]
    out       <- get_vector(col_kg, data, 33, 44, na.rm = FALSE)
    if (unit == "ton") {
      out <- out / 1000
    } else if (unit == "kg") {
    } else {
      stop ("Unknown unit.")
    }
    out
  }

  catch_4ports        <- load_catch_4ports(path, spcs)
  catch_bou_akune     <- load_catch_bouuke(path, spcs, "阿久根棒受")
  catch_bou_uchinoura <- load_catch_bouuke(path, spcs, "内之浦棒受")
  out <- list(year = years,
              month = months,
              maki4ports = catch_4ports,
              bou_akune = catch_bou_akune,
              bou_uchinoura = catch_bou_uchinoura) %>%
    tibble::as_tibble()
  if (maki.only == TRUE) {
    out %<>% dplyr::select(-"bou_akune", -"bou_uchinoura")
  } else {
    out %<>%
      dplyr::mutate(total = maki4ports + bou_akune + bou_uchinoura)
    if (spread == FALSE) {
      out %<>% dplyr::select(-"total") %>%
        tidyr::gather("maki4ports", "bou_akune", "bou_uchinoura",
               key = "port", value = "catch_ton")
    }
  }
  out

fmtcatch.nagasaki <- function(path) {
  sheets <- readxl::excel_sheets(path)
  sheets
  sheet <- sheets[42]
  sheet <- sheets[43]
  data <- load_alldata(path, sheet)

  locate_spcsrow <- function(spcs, df) {
    regex <- "カ ?タ ?ク ?チ"
    spcs_col <- df[,1] %>%
      dplyr::pull(1)
    spcs_row <- which((gregexpr(regex, spcs_col) > 0))
    if (length(spcs_row) == 0) {
      spcs_str <- spcs_col %>%
        tidyr::replace_na(" ")
      spcs_str <- spcs_str[nchar(spcs_str) == 1] %>%
        stringr::str_c(collapse = "")
      spcs_row <- unlist(gregexpr(regex, spcs_str))
    }
    spcs_row
  }

  get_monthcol <- function(row, df) {
    regex <- "^[１-９]+　+月$"
    col   <- which(gregexpr(regex, df[row, ]) > 0)
    out   <- data.frame(row = row, col = col)
    out
  }
  rowcol <- purrr::map_dfr(spcs_row, get_monthcol, df = data)

  xtract_numerici <- function(str) {
    regex <- "\\D+"
    half <- Nippon::zen2han(str) %>%
      stringr::str_replace(regex, "") %>%
      readr::parse_integer()
    half
  }
  xtract_numeric <- function(str) {
    out <- purrr::map_int(str, xtract_numerici)
    out
  }

  get_month <- function(spcs, df, rowcol) {
    out <- NULL
    for (i in seq_len(nrow(rowcol))) {
      r <- rowcol[i, "row"]
      c <- rowcol[i, "col"]
      out <- append(out, dplyr::pull(df, c)[r])
    }
    out
  }
  get_month("foo", data, rowcol) %>%
    xtract_numeric()

  spcs_row <- locate_spcsrow("katakuchi", data)
  purrr::map(spcs_row, get_monthcell, df = data)

}
