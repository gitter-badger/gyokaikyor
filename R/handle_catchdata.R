get_catch <- function(year, list_catchdata, gkk.month) {
  range <- get_ymrange(year, list_catchdata, gkk.month)
  df    <- list_catchdata %>%
    summarize_seikai() %>%
    iwashi2df()
  out <- NULL
  out <- df %>%
    dplyr::filter(dplyr::between(ym, range[1], rev(range)[1])) %>%
    dplyr::pull(catch)
  out
}

make_summary <- function(list_catchdata, year, gkk.month) {
  recentyr <- (year - 5):(year - 1)
  out            <- NULL
  out$last       <- get_catch(year - 1, list_catchdata, gkk.month)
  out$last_upr   <- out$last * 1.2
  out$last_lwr   <- out$last * 0.8
  out$recent     <- purrr::map_dfc(recentyr, get_catch, list_catchdata, gkk.month) %>%
    rowMeans()
  out$recent_upr <- out$recent * 1.2
  out$recent_lwr <- out$recent * 0.8
  out            <- sapply(out, mean)
  suppressWarnings(
    out$range_last <- get_ymrange(year - 1, list_catchdata, gkk.month))
  out
}
