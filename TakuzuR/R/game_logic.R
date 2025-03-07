#' @export
generate_takuzu_grid <- function(size = 8) {
  matrix(as.integer(sample(c(0, 1), size^2, replace = TRUE)), nrow = size, ncol = size)
}
