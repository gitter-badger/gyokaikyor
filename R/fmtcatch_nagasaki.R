get_port <- function(str) {
   out   <- NULL
   regex <- "(?<=Ｈ(．|\\.)\\d\\d?(\\）|\\))?)((長崎|奈留|小佐々|橘))(?=\\w+ )"
   port  <- stringr::str_extract(str, regex)
   out   <- switch(port,
                   "長崎" = "nagasaki",
                   "奈留" = "naru",
                   "小佐々" = "kosasa",
                   "橘" = "tachibana")
   out
 }

 fmtcatch.nagasaki <- function(path, spcs) {
   locate_spcsrow <- function(regex, df) {
     spcs_col <- dplyr::pull(df, 1)
     spcs_row <- which( (gregexpr(regex, spcs_col) > 0))
     if (length(spcs_row) == 0) {
       spcs_str <- spcs_col %>%
         tidyr::replace_na(" ")
       spcs_str[nchar(spcs_str) != 1] <- " "
       spcs_str %<>%
         stringr::str_c(collapse = "")
       spcs_row <- unlist(gregexpr(regex, spcs_str))
     }
     spcs_row
   }

   get_monthcol <- function(row, df) {
     regex <- "^([０-９]|[0-9])+　+月$"
     col   <- which(gregexpr(regex, df[row, ]) > 0)
     if (length(col) == 0) {
       row <- row - 1
       col <- which(gregexpr(regex, df[row, ]) > 0)
     }
     out   <- data.frame(row = row, col = col)
     out
   }

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

   get_month <- function(regex, df, offset.x = 0, offset.y = 0,
                         xtract.digit = FALSE) {
     spcs_row <- locate_spcsrow(regex, df)
     rowcol   <- purrr::map_dfr(spcs_row, get_monthcol, df = df)
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

   get_year <- function(sheet, month) {
     ym  <- parse_ym(sheet)
     out <- rep(ym$year_start, length(month))
     if (any (diff(month) < 0)) {
       out[(which (diff(month) < 0) + 1):length(out)] <- ym$year_start + 1
     }
     out
   }

   fmt_sheet <- function(sheet, path, regex) {
     data   <- load_alldata(path, sheet)
     port   <- get_port(dplyr::pull(data, 1)[2])
     months <- get_month(regex, data, xtract.digit = TRUE)
     years  <- get_year(sheet, months)
     catch  <- get_month(regex, data, offset.x = 2, offset.y = 5)
     out    <- list(year = years,
                    month = months,
                    port = port,
                    catch = catch,
                    fname = path,
                    sheet = sheet) %>%
       tibble::as_tibble()
     rm(data)
     out
   }

   spcs_jp <- switch(spcs,
                     "maiwashi" = "マイワシ",
                     "urume" = "ウルメイワシ",
                     "katakuchi" = "カタクチ")
   spcs_regex <- insert_regex(str = spcs_jp, regex = "( |　)",
                              prefix = FALSE, option = TRUE)
   sheets <- readxl::excel_sheets(path)
   out    <- purrr::map(sheets, fmt_sheet, path = path, regex = spcs_regex) %>%
     dplyr::bind_rows()
   out
}
