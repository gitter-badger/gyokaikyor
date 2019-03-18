#' Load and format bl histogram data
#'
#' @inheritParams readxl::read_excel
#' @param prefec Name of prefecture as string, oneof
#' \itemize{
#'   \item "yamaguchi"
#'   \item "fukuoka"
#'   \item "saga"
#'   \item "nagasaki"
#'   \item "kumamoto"
#'   \item "kagoshima"
#' }
#' @param spcs Spcs name as string, one of
#' \itemize{
#'   \item maiwashi
#'   \item maaji
#'   \item sabarui
#'   \item masaba
#'   \item gomasaba
#'   \item katakuchi
#'   \item urume
#' }
#' @param nest If \code{TRUE}, data will be shown in rectangle format
#'   whith nested bl datafor quick overview.
#' @export
format_bl <- function(path, prefec, spcs, nest = FALSE) {
  class(path) <- prefec
  fmtbl(path, spcs, nest)
}

fmtbl <- function(path, spcs, nest = FALSE) {
  UseMethod("fmtbl")
}

load_alldata <- function(path, sheet) {
  suppressMessages(
    alldata   <- readxl::read_excel(path,
                                    sheet = sheet, col_names = FALSE,
                                    col_types = "text")
  )
}

make_blclass <- function(left, right) {
  left %<>% unlist() %>%
    as.vector() %>%
    as.numeric()
  right %<>% unlist() %>%
    as.vector() %>%
    as.numeric()
  out <- paste0("[", left, ",", right, ")")
  out
}

jpmonth2num <- function(x) {
  out <- x %>%
    as.vector() %>%
    gsub("\u6708", "", .) %>% # "tsuki" in jp kanji
    as.numeric()
  out
}

fmtbl.nagasaki  <- function(path, spcs, nest = TRUE) {

  check_month <- function(months, month_start, month_end) {
    if (!(month_start == months[1]) | (!month_end == rev(months)[1])) {
      # message ("Check month data")
    }
  }

  give_yr2month <- function(mvec, year.start) {
    out           <- list()
    is_yr_changed <- FALSE
    for (i in seq_along(mvec)) {
       m            <- mvec[i]
       out$month[i] <- m
       if (i >= 2) {
         if (m < out$month[i - 1]) {
         is_yr_changed <- TRUE
         }
       }

       if (is_yr_changed) {
         out$year[i] <- year.start + 1
       } else {
         out$year[i] <- year.start
       }
    }
    out
  }

  sheet     <- make_shtname(prefecture = "nagasaki", spcs = spcs)
  alldata   <- load_alldata(path, sheet)
  colpos    <- get_col2load(target = alldata[4, ],
                            regex = ".\u6708", # "tsuki" in jp kanji
                            offset = 0)
  months         <- jpmonth2num(alldata[4, colpos])
  histdata       <- purrr::map(colpos, get_histdata, df = alldata,
                          prefec = "nagasaki")
  parsedym       <- parse_ym(path)
  check_month(months, parsedym$month_start, parsedym$month_end)
  year_start     <- parsedym$year_start
  out            <- list()
  out$year       <- give_yr2month(months, year_start)$year
  out$month      <- give_yr2month(months, year_start)$month
  out$prefecture <- "nagasaki"
  out$hist       <- histdata
  out            <- tibble::as_tibble(out)
  if (nest == FALSE) {
    out <- tidyr::unnest(out)
  }
  out
}

fmtbl.kumamoto  <- function(path, spcs, nest = TRUE) {
  parse_year <- function(path) {
    if ( ( stringr::str_detect(path, "/"))) {
      fname <- stringr::str_match(path, "^.+/(\\d+\\s?【熊本県】.+)")[2]
    } else {
      fname <- path
   }
    match  <- stringr::str_match(fname, "^\\d+\\s?【熊本県】(\\w\\d+)まき")
    wareki <- match[2]
    era    <- stringr::str_sub(wareki, 1, 1)
    jpyr   <- stringr::str_replace(wareki, "^\\w", "")
    year   <- switch(era,
           "H" = paste0("heisei", jpyr, "年") %>%
             Nippon::wareki2AD()
           )

    year
  }
  sheet     <- make_shtname(prefecture = "kumamoto", spcs = spcs)
  alldata   <- load_alldata(path, sheet)
  cpos_date <- get_col2load(alldata[1, ], regex = "[0-9]+", offset = 0)
  date      <- alldata[1, cpos_date] %>%
    purrr::map_chr(tinyplyr::num2date)
  type      <- alldata[1, cpos_date + 4] %>%
    unlist() %>%
    as.vector()
  bl         <- purrr::map(cpos_date, get_measdata,
                           prefec = "kumamoto", df = alldata)

  out            <- list()
  out$date       <- date
  out$type       <- type
  out$year       <- lubridate::year(out$date)
  out$month      <- lubridate::month(out$date)
  out$scbl       <- bl
  out$prefecture <- "kumamoto"

  out <- tibble::as_tibble(out)
  if (nest == FALSE) {
    out <- tidyr::unnest(out)
  }
  out
}

fmtbl.kagoshima <- function(path, spcs, nest = TRUE) {
  sheet     <- make_shtname(prefecture = "kagoshima", spcs = spcs)
  alldata   <- load_alldata(path, sheet)
  cpos_date <- get_col2load(alldata[3, ], regex = "[0-9]+", offset = 0)
  date      <- alldata[3, cpos_date] %>%
    tinyplyr::num2date()
  type      <- alldata[6, cpos_date] %>%
    unlist() %>%
    as.vector()
  bl         <- purrr::map(cpos_date, get_histdata,
                           df = alldata, prefec = "kagoshima")
  out            <- list()
  out$date       <- date
  out$type       <- type
  out$year       <- lubridate::year(out$date)
  out$month      <- lubridate::month(out$date)
  out$bl         <- bl
  out$prefecture <- "kagoshima"

  out <- tibble::as_tibble(out)
  if (nest == FALSE) {
    out <- tidyr::unnest(out)
  }
  out
}

rename_class <- function(left, bin) {
  out <- paste0("[", left, ",", left + bin, ")")
  out
}

#' Format bldata exported from FRESCO database
#'
#' @inheritParams fmtbl
#' @param type Format of data to load either 'taichou' or 'seimitsu'.
#' @param date.start The first day of the processed data.
#' @param date.end The last day of the the processed data
#' @examples
#' \dontrun{
#'   fmtbl_fresco("2019Mar_seikai_taichou_katakuchi.csv", type = "taichou",
#'                date.start = "20180901", date.end = "20190331")
#'   fmtbl_fresco("2019Mar_seikai_seimitsu_katakuchi.csv", type = "seimitsu"
#'                date.start = "20180901", date.end = "20190331")
#' }
#' @export
fmtbl_fresco <- function(path, type, date.start, date.end) {
  dstart <- lubridate::ymd(date.start)
  dend   <- lubridate::ymd(date.end)
  if (type == "seimitsu") {
    suppressMessages(
      data <- readr::read_csv(path, locale = readr::locale(encoding = "cp932"),
                              col_types = coltypes_seimitsu)
    )
  } else if (type == "taichou") {
    suppressMessages(
      data <- readr::read_csv(path, locale = readr::locale(encoding = "cp932"),
                              col_types = coltypes_taichou)
    )
  }
  out <- data %>%
    dplyr::mutate(date  = lubridate::ymd(漁獲年月日),
                  year  = lubridate::year(date),
                  month = lubridate::month(date),
                  day   = lubridate::day(date),
                  ym    = paste0(year, formatC(month, width = 2, flag = 0)) %>%
                    as.numeric()) %>%
      dplyr::rename(spcs_code = 魚種コード,
                    prefec_code = 県コード) %>%
      dplyr::left_join(prefec_code, by = c("prefec_code" = "code"))
  if (type == "seimitsu") {
    out %<>%
      dplyr::rename(scbl = 被鱗体長,
                    bw = 体重) %>%
      dplyr::select(date, year, month, day, ym,
                    spcs_code, prefec_code, prefecture, scbl, bw)
  } else if (type == "taichou") {
    out %<>%
      dplyr::rename(blclass = 開始の階級値,
                    count = 度数) %>%
      dplyr::mutate(blclass = ifelse(階級幅 == 0.5,
                                          blclass * 10,
                                          ifelse(階級幅 == 5,
                                                 blclass,
                                                 NA))) %>%
      tidyr::drop_na(count) %>%
      dplyr::select(date, year, month, day, ym,
                    spcs_code, prefec_code, prefecture, blclass, count) %>%
      dplyr::mutate(blclass = rename_class(blclass, bin = 5))
  }
  dplyr::filter(out, dplyr::between(date, dstart, dend))
}
