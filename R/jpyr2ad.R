jpyr2ad <- function(x, start) {
  conv <- vector(mode = "integer")
  if (start == "showa") {
   suppressMessages(pos_lastyr <- alert_decrease(x))
   x[1:96]
   x[1:97]
   conv[1:pos_lastyr] <- 1925
   conv[1:96]
   conv[1:97]
   conv[(pos_lastyr + 1):length(x)] <- 1988
  } else {
    stop("jpyr2ad")
  }
  ad <- x + conv
  ad
}
