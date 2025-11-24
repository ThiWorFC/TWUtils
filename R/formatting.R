#' Wrapper for Formatting Numerical Values
#'
#' Wrapper to format numerical values to the right number of decimals. If the value is missing, they are replaced with an empty string.
#'
#' @param x Numerical value or vector of numerical value to formatter
#' @param n Number of decimals after the comma.
#'
#' @returns Vector of character of the same length as x, with values formatted with the right number of decimals
#' @export
#'
#' @examples
#' x = c(1.6589, NA, 0.15681, 32)
#' formatting(x, n=2)
formatting <- function(x, n){

  v=format(round(x, n), nsmall=n)

  for (i in 1:length(v)){
    if (stringr::str_trim(v[i]) %in% c("NaN","NA")){
      v[i]=""
    }
  }

  return(v)
}
