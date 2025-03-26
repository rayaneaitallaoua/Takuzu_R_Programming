#' @useDynLib TakuzuR
#' @importFrom Rcpp sourceCpp
NULL

#' Generate a Takuzu grid with 0 and 1
#' @export
generate_takuzu_grid <- function(size = 8) {
  matrix(as.character(sample(c("0", "1"), size^2, replace = TRUE)), nrow = size, ncol = size)
}

#' Check if more than two consecutive values in a row/column
#' @export
check_rule_1 <- function(grid, i, j) {
  checkRule1(grid, i, j)
}

#' Check if number of 0s and 1s differ in a full row/column
#' @export
check_rule_2 <- function(grid, i, j) {
  checkRule2(grid, i, j)
}
