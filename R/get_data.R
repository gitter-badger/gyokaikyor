get_vector <- function(col, row, df, na.rm) {
  out <- dplyr::pull(df, col)[row]
  if (na.rm) {
    out %<>% stats::na.omit() %>%
      as.vector()
  } else {
    out %<>% tidyr::replace_na(0)
  }
  out
}

get_measdata <- function(col, df, prefec) {
  switch(prefec,
         "kumamoto" = {
           startrow <- 8
           endrow   <- 107
         },
         stop("Unknown prefecture"))
  out <- get_vector(col, startrow:endrow, df, na.rm = TRUE) %>%
    as.numeric()
  out
}

locate_vecend <- function(x) {
  out <- which(!is.na(x)) %>% max()
  out
}

get_histdata <- function(col, df, prefec) {
  switch(prefec,
         "nagasaki" = {
           startrow  <- 5
           endrow    <- locate_vecend(df[, col]) - 1
           class_l   <- get_vector(col = cellranger::letter_to_num("B"),
                                   startrow:endrow, df = df, na.rm = FALSE)
           class_r   <- get_vector(col = cellranger::letter_to_num("C"),
                                   startrow:endrow, df = df, na.rm = FALSE)
           blclass   <- make_blclass(class_l, class_r)
         },
         "kagoshima" = {
           startrow <- 9
           endrow   <- stringr::str_which(dplyr::pull(df, 2), "合　計") - 1
           class_start <- df[startrow, 2] %>%
             stringr::str_replace("(?<=\\d\\.\\d)\\D", "") %>%
             stringr::str_replace("( |　)+", "") %>%
             as.double()
           class_end <- df[endrow, 2] %>%
             as.integer()
           left     <- seq(class_start * 10, class_end * 10 + 5, 5)
           blclass  <- make_blclass(left, left + 5)
         },
         stop("Unknown prefecture"))
  count <- get_vector(col, startrow:endrow, df, na.rm = FALSE) %>%
    as.numeric()
  out   <- data.frame(blclass = blclass, count = count) %>%
    dplyr::mutate(blclass = as.character(blclass))
  out
}
