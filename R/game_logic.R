#' @useDynLib TakuzuR
#' @importFrom Rcpp sourceCpp
NULL

#' Generate a random Takuzu grid
#'
#' Creates a matrix of the specified size filled with random "0" and "1" characters.
#' This function is primarily used for testing or generating a full solution grid.
#'
#' @param size Integer specifying the number of rows and columns (grid is square).
#' @return A character matrix with values "0" and "1".
#' @export
generate_takuzu_grid <- function(size = 8) {
  matrix(as.character(sample(c("0", "1"), size^2, replace = TRUE)), nrow = size, ncol = size)
}

#' Rule 1: Check for three consecutive identical values
#'
#' This function checks whether placing a value at position (i, j) causes a violation
#' of the rule that forbids three identical values consecutively in a row or column.
#'
#' @param grid Character matrix representing the current Takuzu grid.
#' @param i Row index (0-based).
#' @param j Column index (0-based).
#' @return TRUE if the rule is violated, FALSE otherwise.
#' @export
check_rule_1 <- function(grid, i, j) {
  checkRule1(grid, i, j)
}

#' Rule 2: Check for imbalance of 0s and 1s in row/column
#'
#' This function verifies if a full row or column contains unequal numbers of "0" and "1".
#' Only fully filled rows or columns are checked.
#'
#' @param grid Character matrix representing the current Takuzu grid.
#' @param i Row index (0-based).
#' @param j Column index (0-based).
#' @return TRUE if there is an imbalance in a full row or column, FALSE otherwise.
#' @export
check_rule_2 <- function(grid, i, j) {
  checkRule2(grid, i, j)
}

#' Rule 3: Check for duplicate rows or columns
#'
#' This function checks whether any two fully filled rows or columns are identical,
#' which violates the uniqueness rule of the Takuzu game.
#'
#' @param grid Character matrix representing the current Takuzu grid.
#' @return TRUE if any full rows or columns are duplicates, FALSE otherwise.
#' @export
check_rule_3 <- function(grid) {
  checkRule3(grid)
}
