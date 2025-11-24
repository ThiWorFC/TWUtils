#' Unfill Vector
#'
#' Opposite function to the fill_vec function: when following rows contain the same value, it only keeps the first one until a new value is observed, the repeated values being replaced by an empty cell ("")
#'
#' @param x The vector of elements to evaluated
#'
#' @returns A vector of a same length as x, but with following identical values being replaced by ""
#' @export
#'
#' @examples x = tibble::tibble(Col1 = c(rep("A",5),rep("B",3)), Col2 = c(1:5,1:3))
#' dplyr::mutate(x, Col1 = unfill_vec(Col1))
unfill_vec <- function(x) {

  if (is.factor(x)) x <- as.character(x)
  same <- x == dplyr::lag(x)
  ifelse(!is.na(same) & same, NA, x)

}
