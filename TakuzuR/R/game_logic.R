#' @export
generate_takuzu_grid <- function(size = 8) {
  matrix(sample(c(0, 1), size * size, replace = TRUE), nrow = size)
}
