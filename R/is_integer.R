
#' Internal function. Checks if a number is a whole number.
#' @noRd

is_integer <- function(x, tol = .Machine$double.eps ^ 0.5)  {
  all(abs(x - round(x)) < tol)
}
