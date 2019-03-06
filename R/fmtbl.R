#' Load and format bl histogram data
#'
#' @inheritParams readxl::read_excel
#' @param spcs Spcs name in romaji, one of
#' @param nest If \code{TRUE}, data will be shown in rectangle format
#'   whith nested bl datafor quick overview.
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
    as.numeric() %>%
    formatC(width = 3, flag = 0)
  right %<>% unlist() %>%
    as.vector() %>%
    as.numeric() %>%
    formatC(width = 3, flag = 0)
  out <- paste(left, right, sep = "-")
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
      stop ("Check month data")
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
  months    <- jpmonth2num(alldata[4, colpos])
  histdata  <- purrr::map(colpos, get_histdata, df = alldata,
                          prefec = "nagasaki")
  parsedym  <- parse_ym(path)
  check_month(months, parsedym$month_start, parsedym$month_end)
  year_start <- parsedym$year_start
  out       <- list()
  out$year   <- give_yr2month(month, year_start)$year
  out$month  <- give_yr2month(month, year_start)$month
  out$hist   <- histdata
  out        <- tibble::as_tibble(out)
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
  method    <- alldata[1, cpos_date + 4] %>%
    unlist() %>%
    as.vector()
  bl         <- purrr::map(cpos_date, get_measdata,
                           prefec = "kumamoto", df = alldata)

  out        <- list()
  out$date   <- date
  out$method <- method
  out$year   <- lubridate::year(out$date)
  out$month  <- lubridate::month(out$date)
  out$bl     <- bl

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
  method    <- alldata[6, cpos_date] %>%
    unlist() %>%
    as.vector()
  bl         <- purrr::map(cpos_date, get_histdata,
                           df = alldata, prefec = "kagoshima")

  out        <- list()
  out$date   <- date
  out$method <- method
  out$year   <- lubridate::year(out$date)
  out$month  <- lubridate::month(out$date)
  out$bl     <- bl

  out <- tibble::as_tibble(out)
  if (nest == FALSE) {
    out <- tidyr::unnest(out)
  }
  out
}
