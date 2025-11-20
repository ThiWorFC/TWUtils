unfill_vec <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  same <- x == dplyr::lag(x)
  ifelse(!is.na(same) & same, NA, x)
}
