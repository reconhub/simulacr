#' Genrate random labels
#'
#' This function is a simple wrapper for `stri_rand_strings` from the `stringi`
#' package to generate random alphanumeric labels.
#'
#' @export
#'
#' @param n the number of labels to generate; defaults to 1
#'
#' @param size the number of characters of the labels; defaults to 6
#'
#' @examples
#'
#' draw_labels()
#' draw_labels(10, 3)

draw_labels <- function(n = 1, size = 6) {
  stringi::stri_rand_strings(n = n, length = size, pattern = "[A-Za-z0-9]")
}
