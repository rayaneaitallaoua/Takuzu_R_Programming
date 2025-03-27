#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
bool check_rule_1(CharacterMatrix grid, int i, int j) {
    int size = grid.nrow();
    std::string value = Rcpp::as<std::string>(grid(i, j));

    if (value == "") return false;

    if (j > 1 && grid(i, j-1) == value && grid(i, j-2) == value) return true;
    if (j < size - 2 && grid(i, j+1) == value && grid(i, j+2) == value) return true;
    if (j > 0 && j < size - 1 && grid(i, j-1) == value && grid(i+1, j) == value) return true;

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

// [[Rcpp::export]]
std::vector<int> check_rule_3(CharacterMatrix grid) {
  int size = grid.nrow();
  std::vector<int> violations; // Will store 1 for row violation, -1 for column violation, 0 for none
  
  bool identical_rows = false;
  bool identical_cols = false;
  
  // Check for identical rows
  for (int i = 0; i < size - 1; ++i) {
    for (int j = i + 1; j < size; ++j) {
      bool identical = true;
      bool all_filled = true;
      for (int k = 0; k < size; ++k) {
        if (grid(i, k) != grid(j, k) || grid(i, k) == "") {
          identical = false;
          break;
        }
        if (grid(i, k) == "") {
          all_filled = false;
          break;
        }
      }
      if (identical && all_filled) {
        identical_rows = true;
        break;
      }
    }
    if (identical_rows) break; // Only need to find one pair
  }
  
  // Check for identical columns
  for (int i = 0; i < size - 1; ++i) {
    for (int j = i + 1; j < size; ++j) {
      bool identical = true;
      bool all_filled = true;
      for (int k = 0; k < size; ++k) {
        if (grid(k, i) != grid(k, j) || grid(k, i) == "") {
          identical = false;
          break;
        }
        if (grid(k, i) == "") {
          all_filled = false;
          break;
        }
      }
      if (identical && all_filled) {
        identical_cols = true;
        break;
      }
    }
    if (identical_cols) break; // Only need to find one pair
  }
  
  if (identical_rows) violations.push_back(1);
  if (identical_cols) violations.push_back(-1);
  
  return violations;
}