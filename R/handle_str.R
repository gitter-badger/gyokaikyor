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
  ym_start_match <- stringr::str_match(path, ".*/?(\\d{4})\\.((?:0|1)\\d)(?=-)")
  year_start     <- ym_start_match[2] %>% as.numeric()
  month_start    <- ym_start_match[3] %>% as.numeric()
  ym_end_match   <-
    stringr::str_match(path, ".*/?\\d{4}\\.(?:0|1)\\d-(\\d{4})\\.((?:0|1)\\d)")
  year_end       <- ym_end_match[2] %>% as.numeric()
  month_end      <- ym_end_match[3] %>% as.numeric()
  out <- list()
  out$year_start  <- year_start
  out$month_start <- month_start
  out$year_end    <- year_end
  out$month_end   <- month_end
  out
}
