TakuzuR: Takuzu Puzzle Game in R
================

# TakuzuR

**TakuzuR** is an R package that provides the core logic and an
interactive Shiny app to play the logic puzzle game *Takuzu* (also known
as *Binairo*). The game challenges users to fill a grid with 0s and 1s
while following strict rules of adjacency, balance, and uniqueness.

## 🔧 Installation

To install the development version from your local project or from
GitHub:

``` r
devtools::install_github("rayaneaitallaoua/Takuzu_R_Programming")
```

## 🚀 Launch the Shiny App

Run the Takuzu game:

``` r
shiny::runApp(system.file("shiny_app", package = "TakuzuR"))
```

## 🎮 Game Rules

- Each row and column must contain the **same number of 0s and 1s**.
- No more than **two consecutive identical numbers** (0 or 1) are
  allowed.
- All rows and columns must be **unique** (no duplicates).
- Click on a cell to toggle between **empty → 0 → 1 → empty**.

The app shows color-coded feedback and error messages when rules are
broken.

## 📦 Package Functions

The core logic is implemented in C++ and exposed in R:

``` r
TakuzuR::generate_takuzu_grid(size = 8)
TakuzuR::check_rule_1(grid, i, j)
TakuzuR::check_rule_2(grid, i, j)
TakuzuR::check_rule_3(grid)
```

## ✍️ Authors

- Rayane Ayoub AIT ALLAOUA
- Najat Ibrahim Amoukou

## 📜 License

This package is released into the public domain.

------------------------------------------------------------------------

Happy puzzling! 🎉
