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

  catch_4ports        <- load_catch_4ports(path, spcs)
  catch_bou_akune     <- load_catch_bouuke(path, spcs, sheet = "阿久根棒受")
  catch_bou_uchinoura <- load_catch_bouuke(path, spcs, sheet = "内之浦棒受")
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
}
