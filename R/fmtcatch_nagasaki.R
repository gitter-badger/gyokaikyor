#' Make vector 'houganshi'
#'
#' This function returns vector 'houganshi' to locate the position of
#'   target word (e.g. species name) in a Excel rows or columns.
#' To keep correspondence between nubmer of cells and nchar of output string,
#' This function replaces \code{NA} and a cell value with multiple characters.
#' @param str String vector with NA or multiple characters
#' @return Long single string composed of single-word cell and whitespace
#' @examples
#' \dontrun{
#'   str <- rep(1:10, 10) %>%
#'     replace(which(. %% 3  == 0), NA) %>%
#'     replace(which(. %% 5  == 0), "foo") %>%
#'     as.character()
#'   make_hougan(str)
#' }
make_hougan <- function(str) {
  out <- tidyr::replace_na(str, " ")
  out[nchar(out) != 1] <- " "
  out %<>% stringr::str_c(collapse = "")
  out
}

#' Locate row position of the cell which contains species name
#'
#' This function locates the row position of the cell which contains
#'   specific species name.
#' @param regex Regular expression to match the name of the target species.
#' @param colstr String vector made from a Excel worksheet column.
#' @return All the row positions where the species name start.
ngs_locate_spcsrow <- function(regex, colstr) {
  spcs_row <- stringr::str_which(colstr, regex)
  if (length(spcs_row) == 0) {
    spcs_str <- make_hougan(colstr)
    spcs_row <- unlist(gregexpr(regex, spcs_str))
  }
  spcs_row
}

#' Detect month column coping with variation in row position of the month cell
#'
#' @param row Assumed position of the month cell
#' @param df Loaded Excel sheet
#' @return Row-column set of month cells in a given \code{row}
ngs_get_monthcol <- function(row, df) {
  regex <- "^([０-９]|[0-9])+　+月$"
  col   <- which(gregexpr(regex, df[row, ]) > 0)
  if (length(col) == 0) {
    row <- row - 1
    col <- which(gregexpr(regex, df[row, ]) > 0)
  }
  out   <- data.frame(row = row, col = col)
  out
}

#' Get values placed separately in column direction
#'
#' This function return values placed separately on the specific fish row
#'   in column direction.
#' @inheritParams ngs_locate_spcsrow
#' @inheritParams ngs_get_monthcol
#' @param offset.x X-offset of the target value
#'   from the position of the month cell
#' @param offset.y Y-offest of the target value
#'   from the position of the month cell
#' @param xtract.digit If \code{TRUE}, all characters were deleted so that
#'   the return value composed only of numerics.
ngs_get_colvalue <- function(regex, df, offset.x = 0, offset.y = 0,
                          xtract.digit = FALSE) {
  spcs_col <- dplyr::pull(df, 1)
  spcs_row <- ngs_locate_spcsrow(regex, spcs_col)
  rowcol   <- purrr::map_dfr(spcs_row, ngs_get_monthcol, df = df)
  out      <- purrr::map2(rowcol$col + offset.x,
                          rowcol$row + offset.y,
                          get_vector, df = df, na.rm = FALSE) %>%
    purrr::flatten_chr()
  if (xtract.digit) {
    out %<>% xtract_numeric()
  } else {
    out %<>% readr::parse_number()
  }
  out
}

#' Make year vectors considering year change
#'
#' This function makes year vector from Excel sheet name (\%Y.\%m-\%Y.\%m) and
#'   month vector. If the month vector contains year change (Dec-Jan),
#'   year vector returned is composed from two years.
#' @param sheet Sheet name of Nagasaki catch data (fmt: \%Y.\%m-\%Y.\%m).
#' @param month Month vector
#' @examples
#' \dontrun{
#'   ngs_make_yrvec(sheet = "2018.09-2019.03",
#'                  month = c(9, 10, 11, 12, 1, 2, 3))
#' }
ngs_make_yrvec <- function(sheet, month) {
  ym  <- parse_ym(sheet)
  out <- rep(ym$year_start, length(month))
  is_yr_changed <- function(month) {
    any (diff(month) < 0)
  }
  if (is_yr_changed(month)) {
    out[(which(month == 1)):length(out)] <- ym$year_start + 1
  }
  out
}

#' Get port name from string
#'
#' This function returns port name from remarks written in Excel sheet. Port
#' name with variants are extracted using regular expression and then
#' converted to four port names written in ascii characters.
#' @param str Japanes string which contains port name.
ngs_get_port <- function(str) {
  out   <- NULL
  regex <- "(?<=Ｈ(．|\\.)\\d\\d?(）|\\))?)((長崎|奈留|九十九|小佐々|橘))"
  port  <- stringr::str_extract(str, regex)
  out   <- switch(port,
                  "長崎"   = "nagasaki",
                  "奈留"   = "naru",
                  "九十九" = "kujuku",
                  "小佐々" = "kujuku",
                  "橘"     = "tachibana",
                  stop("Unknown port"))
  out
}

#' Load catch data from single Excel sheet and tidy it up
#'
#' @inheritParams readxl::read_excel
#' @inheritParams ngs_get_colvalue
ngs_fmt_sheet <- function(sheet, path, regex) {
  data   <- load_alldata(path, sheet)
  port   <- ngs_get_port(dplyr::pull(data, 1)[2])
  months <- ngs_get_colvalue(regex, data, xtract.digit = TRUE)
  years  <- ngs_make_yrvec(sheet, months)
  catch  <- ngs_get_colvalue(regex, data, offset.x = 2, offset.y = 5)
  out    <- list(year = years,
                 month = months,
                 port = port,
                 catch = catch,
                 fname = path,
                 sheet = sheet,
                 prefecture = "nagasaki") %>%
    tibble::as_tibble()
  rm(data)
  out
}

#' Make catch data reading multiple Excel worksheets
#'
#' @inheritParams fmtcatch
fmtcatch.nagasaki <- function(path, spcs, type = NULL) {
  spcs_jp <- switch(spcs,
                    "maiwashi" = "マイワシ",
                    "urume" = "ウルメイワシ",
                    "katakuchi" = "カタクチ")
  spcs_regex <- insert_regex(str = spcs_jp, regex = "( |　)",
                             prefix = FALSE, option = TRUE)
  sheets <- readxl::excel_sheets(path)
  out    <- purrr::map(sheets, ngs_fmt_sheet,
                       path = path, regex = spcs_regex) %>%
    dplyr::bind_rows()
  out
}
