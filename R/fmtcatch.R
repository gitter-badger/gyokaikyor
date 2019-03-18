#' Load and format catch data
#'
#' @inheritParams readxl::read_excel
#' @param spcs Spcs name in romaji, one of
#' @param prefec Prefecture name as string, one of
#' \itemize{
#'   \item "yamaguchi"
#'   \item "fukuoka"
#'   \item "saga"
#'   \item "nagasaki"
#'   \item "kumamoto"
#'   \item "kagoshima"
#' }
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
format_catch <- function(path, prefec, spcs, type = NULL) {
  class(path) <- prefec
  fmtcatch(path, spcs, type)
}

fmtcatch <- function(path, spcs, type) {
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
