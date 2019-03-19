fmtcatch.kagoshima <- function(path, spcs, spread = TRUE, maki.only = FALSE) {
  make_year <- function(yr_jp, jpera) {
    out <- paste0(jpera, yr_jp, "\u5E74") %>% # "nen" (year) in jp kanji
      Nippon::wareki2AD()
    out
  }
  get_ym <- function(str) {
    split <- stringr::str_split(str, "\\.")
    out   <- NULL
    jpyr.start  <- split[[1]][1] %>% as.numeric()
    jpyr.end    <- split[[length(str)]][1] %>% as.numeric()
    if (jpyr.end < jpyr.start)
      stop("Japanese era changed! Check algorithm")
    mth.start  <- split[[1]][2] %>% as.numeric()
    mth.end    <- split[[length(str)]][2] %>% as.numeric()
    yr.start   <- make_year(jpyr.start, "heisei")
    yr.end     <- make_year(jpyr.end, "heisei")
    dstart     <- as.Date(paste(yr.start, mth.start, 1, sep = "-"))
    dend       <- as.Date(paste(yr.end, mth.end, 1, sep = "-"))
    dseq       <- seq.Date(dstart, dend, "month")
    out$years  <- stringr::str_sub(dseq, 1, 4) %>%
      readr::parse_integer()
    out$months  <- stringr::str_sub(dseq, 6, 7) %>%
      readr::parse_integer()
    out
  }

  load_catch_4ports <- function(path, spcs) {
    spcs_jp <- switch(spcs,
                      "maaji" = "マアジ",
                      "sabarui" = "サバ類",
                      "maiwashi" = "マイワシ",
                      "urume" = "ウルメイワシ",
                      "katakuchi" = "カタクチイワシ",
                      stop("Unknown spcs"))
    regex    <- insert_regex(spcs_jp, "\u3000", prefix = TRUE)
    data     <- load_alldata(path, sheet = "４港計")
    col_spcs <- which((gregexpr(regex, data[3, ]) > 0) == TRUE) #nolint
    col2load <- col_spcs + 4
    out      <- get_vector(col2load, 5:16, data, na.rm = FALSE) %>%
      readr::parse_number()
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
    cols_spcs <- which((gregexpr(spcs_jp, data[32, ]) > 0) == TRUE) # nolint
    col_kg    <- cols_spcs[2]
    out       <- get_vector(col_kg, 33:44, data, na.rm = FALSE) %>%
      readr::parse_number()
    if (unit == "ton") {
      out <- out / 1000
    } else if (unit == "kg") {
    } else {
      stop ("Unknown unit.")
    }
    out
  }

  data  <- load_alldata(path, sheet = "\uFF14\u6E2F\u8A08")
                                        # "4koukei" (four port sum) in jp kanji
  str   <- data[5:16, 1] %>%
    unlist() %>%
    as.vector()

  years  <- get_ym(str)$years
  months <- get_ym(str)$months

  catch_4ports        <- load_catch_4ports(path, spcs)
  catch_bou_akune     <- load_catch_bouuke(path, spcs, sheet = "阿久根棒受")
  catch_bou_uchinoura <- load_catch_bouuke(path, spcs, sheet = "内之浦棒受")
  out <- list(year = years,
              month = months,
              maki4ports = catch_4ports,
              bou_akune = catch_bou_akune,
              bou_uchinoura = catch_bou_uchinoura,
              prefecture = "kagoshima") %>%
    tibble::as_tibble() %>%
    tidyr::gather("maki4ports", "bou_akune", "bou_uchinoura",
                  key = "type", value = "catch")
  out
}
