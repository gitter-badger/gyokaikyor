conv2hoshifile <- function(df, prefec, type, ym.start, ym.end,
                           export = FALSE, fname = NULL,
                           class.start = 10, class.end = 160) {
  make_ymseq <- function(ym.start, ym.end) {
    dseq <- seq.Date(as.Date(lubridate::ymd(paste0(ym.start, "01"))),
                     as.Date(lubridate::ymd(paste0(ym.end, "01"))),
                     "month")
    out <- dseq %>%
      stringr::str_replace("-", "") %>%
      stringr::str_sub(1, 6)
    out
  }

  conv2count <- function(df, prefec, ym.start, ym.end) {
    out <- df %>%
      dplyr::mutate(blclass = cut(scbl, breaks = seq(0, 400, 5),
                    include.lowest = TRUE, right = FALSE)) %>%
      dplyr::group_by(year, month, ym, prefecture, blclass) %>%
      dplyr::summarize(count = length(blclass)) %>%
      dplyr::mutate(blclass = as.character(blclass))
    out
  }

  extract_count_ymclass <- function(ym, class, df.count) {
    y   <- substr(ym, 1, 4) %>% as.numeric()
    m   <- substr(ym, 5, 6) %>% as.numeric()
    out <- df.count %>%
      dplyr::filter(year == y,
                    month == m,
                    blclass == class) %>%
      dplyr::pull(count)
    if (length(out) == 0) {
      out <- 0
    }
    out
  }

  extract_count_class <- function(ym, classes, df.count) {
    out <- purrr::map(classes, extract_count_ymclass,
               ym = ym, df.count = df.count) %>%
      unlist() %>%
      purrr::set_names(classes)
    out
  }

  extract_count <- function(classes, ymseq, df.count) {
    names(ymseq) <- ymseq
    out <- purrr::map_df(ymseq, extract_count_class,
                         classes = classes, df.count = df.count) %>%
      as.data.frame()
    rownames(out) <- classes
    out
  }

  make_class <- function(min, max, bin) {
    left  <- seq(min, max - bin, bin)
    right <- seq(min + bin, max, bin)
    out   <- paste0("[", left, ",", right, ")")
    out
  }

  classes <- make_class(class.start, class.end, 5)
  ymseq   <- make_ymseq(ym.start, ym.end)

  if (type == "seimitsu") {
    df.count <- df %>%
      dplyr::mutate(ym = paste0(year, formatC(month, width = 2, flag = 0)) %>%
                      as.numeric()) %>%
      conv2count(prefec, ym.start, ym.end) %>%
      dplyr::filter(prefecture == prefec,
                    dplyr::between(ym, ym.start, ym.end))
  } else {
    df.count <- df %>%
      dplyr::mutate(ym = paste0(year, formatC(month, width = 2, flag = 0)) %>%
                      as.numeric()) %>%
      dplyr::filter(prefecture == prefec,
                    dplyr::between(ym, ym.start, ym.end)) %>%
      dplyr::group_by(year, month, ym, blclass) %>%
      dplyr::summarize(count = sum(count))
  }
  out <- extract_count(classes, ymseq, df.count)
  if (export == TRUE) {
    write.csv(out, fname))
  }
  out
}
