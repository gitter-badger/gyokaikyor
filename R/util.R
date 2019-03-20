## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(".",
                           "maki4ports", "bou_akune", "bou_uchinoura",
                           "year", "month", "ym", "day",
                           "prefecture", "spcs_code",
                           "scbl", "blclass", "count", "bw",
                           "catch", "list_catchdata",
                           "漁獲年月日", "魚種コード", "県コード",
                           "被鱗体長", "体重",
                           "開始の階級値", "度数", "階級幅",
                           "対馬暖流Ｄ", "tw_degc",
                           "Jan", "Feb", "Mar", "Apr", "May", "Jun",
                           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
}

#' @export
#' @importFrom graphics polygon

#' @export
#' @importFrom graphics lines

#' @export
#' @importFrom graphics points

#' @export
#' @importFrom graphics text

#' @export
#' @importFrom graphics arrows

#' @export
#' @importFrom graphics plot

#' @export
#' @importFrom graphics rect

#' @export
#' @importFrom graphics mtext

#' @export
#' @importFrom graphics axis

#' @export
#' @importFrom grDevices hsv

#' @export
#' @importFrom grDevices dev.off
NULL
