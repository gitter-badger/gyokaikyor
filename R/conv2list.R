get_range <- function(year.end, year.start) {
  year.start <- year.start
  years      <- year.start:year.end
  n.prefec   <- 6
  maxrow     <- (length(years) + 2) * n.prefec + n.prefec - 1 + 2
  paste0("A1:N", maxrow)
}

load_iwashi <- function(path, year.end, sheet, year.start = 1992) {
  range   <- get_range(year.end, year.start)
  suppressMessages(
    out <- readxl::read_xlsx(path,
                                 sheet = sheet, skip = 2,
                                 col_names = FALSE, range = range))
  as.data.frame(out)
}

parse_num_vec <- function(col, df, to) {
  vec <- as.character(df[, col])
  out <- vec %>%
    tidyr::replace_na(0)
  if (to == "integer") {
    out %<>% readr::parse_integer()
  }
  if (to == "double") {
    out %<>% readr::parse_double()
  }
  out
}

parse_num_df <- function(cols, df, to) {
  out <- purrr::map_dfc(cols, parse_num_vec, df = df, to = to)
  out
}


get_prefec_rows <- function(str) {
  stringr::str_which(str, "県")
}

make_list <- function(row.prefec, df.iwashi, year.end, year.start = 1992){
  years      <- year.start:year.end
  row_prefec <- get_prefec_rows(df.iwashi[, 1])
  prefec_name      <- stringr::str_replace(df.iwashi[row.prefec, 1], "県.+", "")
  row_start        <- row.prefec + 2
  row_end          <- row_start + length(years) - 1
  data_prefec      <- df.iwashi[row_start:row_end, ] %>%
    parse_num_df(2:13, ., "double") %>%
    magrittr::set_colnames(month.abb)
  year             <- df.iwashi[row_start:row_end, 1] %>%
    stringr::str_replace("年", "") %>%
    readr::parse_integer()
  out              <- NULL
  out$df           <- cbind(year, data_prefec)
  names(out)       <- prefec_name
  out
}

summarize_seikai <- function(list) {
  get_month <- function(prefec, month, list) {
    list[[prefec]][month.abb[month]]
  }
  sum_month <- function(month, list, prefecs) {
    purrr::map(prefecs, get_month, month = month, list) %>%
      purrr::flatten() %>%
      Reduce(`+`, .)
  }
  prefecs <- names(list)
  year    <- list[[1]]$year
  out     <- purrr::map_dfc(1:12, sum_month, list, prefecs) %>%
    purrr::set_names(month.abb)
  out <- cbind(year, out)
  out
}

iwashi2list <- function(path, sheet, year.start = 1992, year.end) {
  df          <- load_iwashi(path = path, year.end = year.end,
                  sheet = sheet, year.start = year.start)
  prefec_rows <- get_prefec_rows(dplyr::pull(df, 1))
  out <- purrr::map(prefec_rows, make_list, df.iwashi = df,
                    year.end = year.end) %>%
    purrr::flatten()
  out
}

ym_matrix2df <- function(x) {
  out <- x %>%
    tidyr::gather(Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec,
                  key = month, value = catch) %>%
    dplyr::mutate(month = abb2num(month),
                  ym    = make_ym(year, month))
  out
}

make_ymrange <- function(year, gkk.month) {
  out <- NULL
  switch(gkk.month,
         "Mar" = {
           mstart <- 4
           mend   <- 3
         },
         "Oct" = {
         },
         stop("Unknown month"))
  out$start <- paste0(year - 1, formatC(mstart, width = 2, flag = 0)) %>%
    as.numeric()
  out$end   <- paste0(year, formatC(mend, width = 2, flag = 0)) %>%
    as.numeric()
  out
}
