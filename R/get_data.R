get_vector <- function(col, df, startrow, endrow, na.rm) {
  rows <- startrow:endrow
  out  <- df[rows, col] %>%
    unlist() %>%
    as.numeric() %>%
    as.vector()
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
  out <- get_vector(col, df,
                    startrow = startrow, endrow = endrow, na.rm = TRUE)
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
                                   df = df,
                                   startrow = startrow, endrow = endrow,
                                   na.rm = FALSE)
           class_r   <- get_vector(col = cellranger::letter_to_num("C"),
                                   df = df,
                                   startrow = startrow, endrow = endrow,
                                   na.rm = FALSE)
           class    <- make_blclass(class_l, class_r)
         },
         "kagoshima" = {
           startrow <- 9
           endrow   <- 48
           class    <- make_blclass(seq(40, 235, 5), seq(45, 240, 5))
         },
         stop("Unknown prefecture"))
  count <- get_vector(col, df,
                      startrow = startrow, endrow = endrow, na.rm = FALSE)
  out   <- data.frame(class = class, count = count)
  out
}
