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
           class    <- make_blclass(class_l, class_r)
         },
         "kagoshima" = {
           startrow <- 9
           endrow   <- 48
           class    <- make_blclass(seq(40, 235, 5), seq(45, 240, 5))
         },
         stop("Unknown prefecture"))
  count <- get_vector(col, startrow:endrow, df, na.rm = FALSE) %>%
    as.numeric()
  out   <- data.frame(class = class, count = count)
  out
}
