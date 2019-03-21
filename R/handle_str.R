get_row <- function(str, regex, offset = 0) {
  stringr::str_which(str, regex) + offset
}

get_col2load   <- function(target, regex, offset) {
  match <- stringr::str_detect(target, regex)
  out <- which(match == TRUE) + offset
  out
}

insert_regex <- function(str, regex, prefix = FALSE, option = FALSE) {
  if (option == FALSE) {
    rep <- "+"
  } else {
    rep <- "*"
  }
  if (prefix == TRUE) {
    out <- paste0(regex, rep, substr(str, 1, 1))
  } else {
    out <- substr(str, 1, 1)
  }
  for (i in 2:nchar(str)) {
    out <- paste0(out, regex, rep, substr(str, i, i))
  }
  out
}

parse_ym <- function(path) {
  if (stringr::str_detect(path, "/")) {
    fname <- stringr::str_extract(path, "(?<=/)[^/]+$")
  } else {
    fname <- path
  }
  ym_start_match <- stringr::str_match(fname, "(\\d+)\\.((?:0|1)\\d)(?=-)")
  year_start     <- ym_start_match[2] %>% as.numeric()
  month_start    <- ym_start_match[3] %>% as.numeric()
  ym_end_match   <-
    stringr::str_match(fname, "\\d+\\.(?:0|1)\\d-(\\d+)\\.((?:0|1)\\d)")
  year_end       <- ym_end_match[2] %>% as.numeric()
  month_end      <- ym_end_match[3] %>% as.numeric()
  if ( (nchar(year_start) != 4) | (nchar(year_end) != 4))
    stop("Failed parsing to year")
  out <- list()
  out$year_start  <- year_start
  out$month_start <- month_start
  out$year_end    <- year_end
  out$month_end   <- month_end
  out
}

xtract_numeric <- function(str) {
  xtract_numerici <- function(str) {
    regex <- "\\D+"
    half <- Nippon::zen2han(str) %>%
      stringr::str_replace(regex, "") %>%
      readr::parse_integer()
    half
  }
  out <- purrr::map_int(str, xtract_numerici)
  out
}

abb2num <- function(abb) {
  lambda <- function(abb) {
    which(abb == month.abb)
  }
  purrr::map_int(abb, lambda)
}

make_ym <- function(y, m) {
  out <- paste0(y, formatC(m, width = 2, flag = 0)) %>%
    readr::parse_integer()
  out
}
