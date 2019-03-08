#' Load and format catch data of yamaguchi
#'
#' @param path File path to process
#' @param spcs Romaji spcs name one of
#' \itemize{
#'   \item{"maaji"}
#'   \item{"maiwashi"}
#'   \item{"sabarui"}
#'   \item{"katakuchi"}
#'   \item{"urume"}
#' }
#' @param type Character value either "sukui" or "bouuke".
fmtcatch.yamaguchi <- function(path, spcs, type = NULL) {
  switch(spcs,
         "maaji"    = {
           spcs_col     <- 5
           meigara_ofst <- 1:3
         },
         "sabarui"  = {
           spcs_col     <- 9
           meigara_ofst <- 1:2
         },
         "maiwashi" = {
           spcs_col     <- 12
           meigara_ofst <- 1:3
         },
         "urume"    = {
           spcs_col     <- 21
           meigara_ofst <- 1:3
         },
         "katakuchi" = {
           spcs_col     <- 16
           meigara_ofst <- 1:4
         })

  get_catch_meigara <- function(cofst, spcs_col, sheet) {
    alldata     <- load_alldata(path, sheet = sheet)
    year        <- xtract_numeric(sheet)
    row_jan     <- 5
    rows        <- row_jan:38
    mmatch      <- xtract_numeric(alldata[[1]][rows])
    mrows       <- which(!is.na(mmatch)) + row_jan - 1
    ofst_sukui  <- 1
    ofst_bouuke <- 2
    out         <- NULL
    out$year    <- year
    out$month   <- dplyr::pull(alldata, 1)[mrows] %>%
      xtract_numeric()
    out$sukui   <- col2data(col = spcs_col + cofst,
                      rows = 1:40,
                      row.pick = mrows + ofst_sukui,
                      alldata)
    out$bouuke  <- col2data(col = spcs_col + cofst,
                      rows = 1:40,
                      row.pick = mrows + ofst_bouuke,
                      alldata)
    out$meigara <- alldata[4, spcs_col + cofst] %>%
      unlist() %>% as.vector()
    out %<>% tibble::as_tibble() %>%
      tidyr::gather("sukui", "bouuke", key = "type", value = "catch")
    out
  }

  sheets <- readxl::excel_sheets(path) %>%
    stringr::str_extract("[0-9]+.+") %>%
    na.omit()

  dat1   <- purrr::map_df(meigara_ofst, get_catch_meigara,
                        spcs_col = spcs_col, sheet = sheets[1])
  dat2   <- purrr::map_df(meigara_ofst, get_catch_meigara,
                        spcs_col = spcs_col, sheet = sheets[2])
  out    <- dplyr::bind_rows(dat1, dat2) %>%
    dplyr::mutate(catch = catch / 1000)
  out
}
