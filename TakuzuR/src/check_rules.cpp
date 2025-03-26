#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
bool checkRule1(CharacterMatrix grid, int i, int j) {
  int size = grid.nrow();
  std::string value = Rcpp::as<std::string>(grid(i, j));

  if (value == "") return false;

  if (j > 1 && grid(i, j-1) == value && grid(i, j-2) == value) return true;
  if (j < size - 2 && grid(i, j+1) == value && grid(i, j+2) == value) return true;
  if (j > 0 && j < size - 1 && grid(i, j-1) == value && grid(i, j+1) == value) return true;

  if (i > 1 && grid(i-1, j) == value && grid(i-2, j) == value) return true;
  if (i < size - 2 && grid(i+1, j) == value && grid(i+2, j) == value) return true;
  if (i > 0 && i < size - 1 && grid(i-1, j) == value && grid(i+1, j) == value) return true;

  return false;
}

// [[Rcpp::export]]
bool checkRule2(CharacterMatrix grid, int i, int j) {
  int size = grid.nrow();
  int count0_row = 0, count1_row = 0;
  int count0_col = 0, count1_col = 0;
  bool row_full = true, col_full = true;

  for (int col = 0; col < size; col++) {
    std::string val = Rcpp::as<std::string>(grid(i, col));
    if (val == "") row_full = false;
    else if (val == "0") count0_row++;
    else if (val == "1") count1_row++;
  }
  if (row_full && count0_row != count1_row) return true;

  for (int row = 0; row < size; row++) {
    std::string val = Rcpp::as<std::string>(grid(row, j));
    if (val == "") col_full = false;
    else if (val == "0") count0_col++;
    else if (val == "1") count1_col++;
  }
  if (col_full && count0_col != count1_col) return true;

  return false;
}
