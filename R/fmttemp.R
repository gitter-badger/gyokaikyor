get_tw_sst <- function(fname, year.end) {
  year.start <- 1953
  nrows      <- (year.end - year.start + 1) * 12
  range2read <- paste0("A1:F", nrows + 1)
  xldata <- readxl::read_xlsx(fname,
                              sheet = "三旬平均実測値",
                              range = range2read) %>%
    dplyr::mutate(tw_degc = 対馬暖流Ｄ,
                  year    = 0,
                  month   = 0)

  for(i in 1:nrows){
    xldata[i, "year"]  <-
      as.numeric(Nippon::zen2han(gsub("年", "", xldata[i, 1])))
    xldata[i, "month"] <-
      as.numeric(Nippon::zen2han(gsub("月", "", xldata[i, 2])))
  }

  sst_tw <- xldata %>%
    dplyr::select(year, month, tw_degc) %>%
    dplyr::mutate(year  = as.integer(year),
                  month = as.integer(month))
  sst_tw
}
