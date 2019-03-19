plot_catch_monthvar <- function(df.seikai, year, gkk.month) {
  get_catch  <- function(year, gkk.month, df.seikai) {
    out      <- NULL
    ym.start <- make_ymrange(year, gkk.month)$start
    ym.end   <- make_ymrange(year, gkk.month)$end
    out      <- iwashi2df(df.seikai) %>%
      dplyr::filter(dplyr::between(ym,
                                   ym.start,
                                   ym.end)) %>%
      dplyr::arrange(ym) %>%
      dplyr::pull(catch)
    out
  }

  draw_catch <- function(l, var, year) {
    catch <- l[[var]]
    ofst  <- 0.5
    switch(var,
           "recent" = {
             polygon(c(1:12, 12:1), c(catch * 0.8, rev(catch * 1.2)),
                     col = hsv(0, 0, 0.8), border = FALSE)
             lines(1:12, catch, lwd = 4)
             points(1:12, catch, pch = 16, cex = 2)
             text(12, catch[12], "平年±20%", pos = 4, offset = ofst, cex = 4.5,
                  xpd = TRUE)
           },
           "last" = {
             col <- hsv(200 / 360, 0.8, 0.8)
             lines(1:12, catch, col = col, lwd = 5)
             points(1:12, catch, col = col, pch = 16, cex = 3)
             arrows(1:12, catch * 0.8, 1:12, catch * 1.2,
                    col = col, length = 0, lwd = 3)
             text(12, catch[12], "前年±20%",
                  col = col, pos = 4, offset = ofst, cex = 4.5, xpd = TRUE)
           },
           "this" = {
             catch <- catch[1:11]
             col <- hsv(0 / 360, 0.8, 0.8)
             lines(1:11, catch, col = col, lwd = 5)
             points(1:11, catch, col = col, pch = 16, cex = 4)
             points(1:11, catch, col = "white", pch = 16, cex = 2.5)
             text(11, catch[11], paste0(year - 1, "年度"),
                  col = col, pos = 4, offset = ofst, cex = 5, xpd = TRUE)
           })
  }

  init_plot <- function(l, gkk.month) {
    max <- l$max
    min <- l$min
    plot(1, 1, xlim = c(1, 16), ylim = c(min, max * 1.2), type = "n",
         axes = FALSE, ann = FALSE, yaxs = "i")
    switch(gkk.month,
           "Mar" = {
             rect(7.8, -10, 12.2, max * 1.1, col = hsv(20 / 360, 0.2, 1),
                         border = FALSE)
             text(10, max, "漁期", cex = 5, col = hsv(20 / 360, 0.5, 0.85))
             axis(1, at = 1:12, labels = FALSE)
             axis(1, at = 1:12, labels = month.abb[c(4:12, 1:3)],
                  cex.axis = 3, col = "transparent", pos = -0.1)
           })
  }

  draw_axes <- function(l) {
    axis(2, at = l$min:l$max, cex.axis = 3)
    mtext("漁獲量 (千トン)", side = 2, line = 6, cex = 5)
  }

  lastyr   <- year - 1
  recentyr <- (year - 4):year
  l        <- list()
  l$this   <- get_catch(year, "Mar", df.seikai) / 1000
  l$last   <- get_catch(lastyr, "Mar", df.seikai) / 1000
  l$recent <- purrr::map_dfc(recentyr, get_catch, "Mar", df.seikai) %>%
    rowMeans() / 1000
  max      <- max(unlist(l)) * 1.2
  min      <- min(unlist(l)) * 0.8
  l$max    <- max
  l$min    <- min

  init_plot(l, gkk.month)
  draw_catch(l, var = "recent", year)
  draw_catch(l, var = "last", year)
  draw_catch(l, var = "this", year)
  draw_axes(l)
}

plot_catch_prefec <- function(list, year, gkk.month) {
  get_ysum <- function(yr, prefec, gkk.month, list) {
    switch(gkk.month,
           "Mar" = {
             ym.start <- paste0(yr - 1, "11") %>% as.numeric()
             ym.end   <- paste0(yr, "01") %>% as.numeric()
           })
    out <- list[[prefec]] %>%
      iwashi2df() %>%
      dplyr::filter(dplyr::between(ym, ym.start, ym.end)) %>%
      dplyr::select(catch) %>%
      sum(na.rm = TRUE)
    out
  }

  get_prefec_catch <- function(prefec, yr, gkk.month, list) {
    lastyr     <- yr - 1
    recentyr   <- (yr - 5):(yr - 1)
    out        <- NULL
    out$this   <- get_ysum(yr, prefec, gkk.month, list)
    out$last   <- get_ysum(lastyr, prefec, gkk.month, list)
    out$recent <- purrr::map(recentyr, get_ysum, prefec, gkk.month, list) %>%
      unlist() %>%
      mean(na.rm = TRUE)
    out
  }

  plot_init <- function(sums) {
    max <- max(unlist(sums), na.rm = TRUE) / 1000
    min <- min(unlist(sums), na.rm = TRUE) / 1000
    prefecs <- names(sums)
    plot(1, 1, xlim = c(0.5, 8), ylim = c(min * 0.8, max * 1.2),
         type = "n", yaxs = "i",
         axes = FALSE, ann = FALSE)
    axis(1, at = c(1:6, 7), labels = FALSE)
    axis(1, at = c(1:6, 7), labels = prefecs, cex.axis = 3.5, pos = -0.1,
         col = "transparent")
    axis(2, at = seq(0, ceiling(max * 1.2), 1), cex.axis = 3.5)
    mtext("漁獲量（千トン）", 2, cex = 5, line = 5)
    rect(0.7, 1.6, 3.2, max * 1.2, col = hsv(0, 0, 1), xpd = TRUE)
    x1 <- 1.52
    x2 <- 2
    y1 <- 2.3
    y2 <- 2.6
    draw_point(1, 2.8, "recent", xright = 1.5)
    lines(c(x1, x2), c(y1, y2), lwd = 3)
    draw_point(x1, y1, "last")
    draw_point(x2, y2, "this")
    text(1.5, max, "平年±20%", col = hsv(0, 0, 0.2), cex = 3,
         pos = 4, offset = 1)
    text(x1, y1, "前年±20%", col = hsv(200 / 360, 0.8, 0.8), cex = 3,
         pos = 4, offset = 2)
    text(x2, y2, "今期", col = hsv(0, 0.8, 0.8), cex = 3,
         pos = 4, offset = 2)
  }
  draw_point <- function(x, y, type, xright = NULL) {
    switch(type,
           "recent" = {
             rect(x, y * 0.8, xright, y * 1.2, col = hsv(0, 0, 0.8))
             lines(c(x, xright), c(y, y), lwd = 4)

           },
           "last" = {
             arrows(x, y * 0.8, x, y * 1.2, length = 0, xpd = TRUE,
                    col = hsv(200 / 360, 0.8, 0.8), lwd = 3)
             points(x, y, pch = 16, col = hsv(200 / 360, 0.8, 0.8),
                           cex = 3.5, xpd = TRUE)
           },
           "this" = {
             points(x, y, pch = 16, col = hsv(0, 0.8, 0.8),
                    cex = 4, xpd = TRUE)
             points(x, y, pch = 16, col = "white",
                    cex = 2.5, xpd = TRUE)
           })
  }

  plot_prefec <- function(prefec, sums) {
    pdata <- sums[[prefec]]
    prefecs <- names(sums)
    x       <- which(prefec == prefecs)
    this <- pdata$this / 1000
    last <- pdata$last / 1000
    recent <- pdata$recent / 1000

    x_this <- x + 0.3
    x_last <- x + 0.1
    lrecent <- x - 0.2
    rrecent <- x_last

    draw_point(x = lrecent, y = recent,
               xright = rrecent, type = "recent")
    lines(c(x_last, x_this), c(last, this), lwd = 4)
    draw_point(x_last, last, "last")
    draw_point(x_this, this, "this")
  }

  make_sums <- function(sums, which) {
    out <- NULL
    for (i in seq_along(sums)) {
      out <- append(out, sums[[i]][which])
    }
    out %<>% unlist %>%
      sum(na.rm = TRUE)
    out
  }

  prefecs <- names(list)
  sums <- purrr::map(prefecs, get_prefec_catch, 2019, "Mar", list) %>%
    purrr::set_names(prefecs)
  sum_this <- make_sums(sums, "this")
  sum_last <- make_sums(sums, "last")
  sum_recent <- make_sums(sums, "recent")
  sums$計$this <- sum_this
  sums$計$last <- sum_last
  sums$計$recent <- sum_recent
  sums

  plot_init(sums)
  plot_prefec("山口", sums)
  plot_prefec("福岡", sums)
  plot_prefec("佐賀", sums)
  plot_prefec("長崎", sums)
  plot_prefec("熊本", sums)
  plot_prefec("鹿児島", sums)
  plot_prefec("鹿児島", sums)
  plot_prefec("計", sums)
}

plot_forecast <- function(model, seikaidata, yr, month.until) {
  plot_rects <- function(summary, yr) {
    years <- (yr - 5):(yr - 1)
    for (y in years) {
      rect(y + (4 - 1) / 12, summary$recent_lwr / 1000,
           y + (9 - 1) / 12, summary$recent_upr / 1000,
           col = hsv(0, 0, 0.8, 0.8), border = FALSE)
    }
    rect(yr + (4 - 1) / 12 + 0.05, summary$last_lwr / 1000,
         yr + (9 - 1) / 12 + 0.05, summary$last_upr / 1000,
         col = hsv(200 / 360, 0.8, 0.8, 0.4), border = FALSE)
  }

  plot_lines <- function(model, summary, yr) {
    years <- (yr - 5):(yr - 1)
    pred  <- mean(exp(model$mean[4:9])) / 1000
    for (y in years) {
      lines(c(y + (4 - 1) / 12, y + (9 - 1) / 12),
            c(summary$recent / 1000, summary$recent / 1000),
            col = hsv(0, 0, 0.2), lwd = 4)
    }
    lines(c(yr + (4 - 1) / 12 + 0.05, yr + (9 - 1) / 12 + 0.05),
          c(summary$last / 1000, summary$last / 1000),
          col = hsv(200 / 360, 1, 0.8), lwd = 4)
    lines(c(yr + 1 + (4 - 1) / 12 + 0.05, yr + 1 + (9 - 1) / 12 + 0.05),
          c(pred, pred),
          col = hsv(0 / 360, 0.8, 0.8), lwd = 4)
    text(yr + 1.8, pred, "予測平均値", pos = 4,
         col = hsv(0, 0.8, 0.8), cex = 3, xpd = TRUE)
  }

  plot_texts <- function(model, yr, summary) {
    text(yr + 1, model$upper[12, "80%"], "80%予測区間",
         pos = 4, xpd = TRUE,
         col = hsv(230 / 360, 0.14, 0.7), cex = 3)
    text(yr + 1, model$upper[12, "95%"], "95%予測区間",
         pos = 4, xpd = TRUE,
         col = hsv(230 / 360, 0.02, 0.7), cex = 3)
    rect(yr - 1.1, summary$last_lwr / 1000,
         yr + 0.1, summary$last_lwr / 1000 - 0.5,
         col = hsv(0, 0, 1, 0.7), border = FALSE)
    text(yr - 0.5, summary$last_lwr / 1000, "前年±20%",
         pos = 1, col = hsv(200 / 360, 0.8, 0.8),
         cex = 3)
    rect(yr - 3.1, summary$recent_lwr / 1000,
         yr - 1.9, summary$recent_lwr / 1000 - 0.5,
         col = hsv(0, 0, 1, 0.7), border = FALSE)
    text(yr - 2.5, summary$recent_lwr / 1000, "平年±20%",
         pos = 1, cex = 3)
  }

  real <- summarize_seikai(list_catchdata) %>%
    iwashi2df() %>%
    arrange(ym) %>%
    filter(between(ym,
                   make_ym(yr, 1),
                   make_ym(yr, 12))) %>%
    select(catch)

  real2 <- real[1:month.until, ] %>%
    ts(start = c(yr, 1), frequency = 12) / 1000

  model_org   <- model
  model$x     <- exp(model$x) / 1000
  model$mean  <- exp(model$mean) / 1000
  model$lower <- exp(model$lower) / 1000
  model$upper <- exp(model$upper) / 1000
  summary     <- make_summary(list_catchdata, 2019, "Mar")
  max         <- ceiling(max(model$upper))
  plot(model, main = "",
       axes = FALSE, ann = FALSE, yaxs = "i",
       xlim = c(yr - 6, yr + 1.5), ylim = c(0, max),
       lwd = 3)
  plot_rects(summary, yr - 1)
  plot_lines(model_org, summary, yr - 1)
  plot_texts(model, yr, summary)
  mtext("年", 1, cex = 5, line = 7)
  mtext("漁獲量（千トン）", 2, cex = 5, line = 5)
  axis(1, at = seq(yr - 6, yr + 1, 1), cex.axis = 3, labels = FALSE)
  axis(1, at = seq(yr - 6, yr + 1, 1), cex.axis = 3,
       pos = -0.2, col = "transparent", srt = 30)
  axis(2, at = seq(0, max, 2), cex.axis = 3, pos = yr - 6.3)
}
