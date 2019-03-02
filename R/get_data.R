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

get_histdata <- function(col, df, prefec) {
  switch(prefec,
         "nagasaki" = {
           startrow <- 5
           endrow   <- 86
           class    <- make_blclass(seq(0, 520, 10), seq(10, 530, 10))
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
