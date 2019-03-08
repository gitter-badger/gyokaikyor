#' Load and format catch data
#'
#' @inheritParams readxl::read_excel
#' @param spcs Spcs name in romaji, one of
#' @param nest If \code{TRUE}, data will be shown in rectangle format
#' @param type Character value either "maki" or "bouuke" to control
#'   data processing algorithm for kumamoto data.
#'   whith nested catch data for quick overview.
#' \itemize{
#'   \item maiwashi
#'   \item maaji
#'   \item sabarui
#'   \item masaba
#'   \item gomasaba
#'   \item katakuchi
#'   \item urume
#' }
#' @export
fmtcatch <- function(path, spcs, type = NULL) {
  UseMethod("fmtcatch")
}

alert_decrease <- function(x) {
  if (any(diff(x) < 0)) {
    message("There is a decrease in number.")
    which(diff(x) < 0)
  } else {
    x
  }
}
