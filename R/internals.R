
assert_R <- function(x) {
  if (!is.numeric(x)) stop("R is not numeric")
  if (!all(is.finite(x))) stop("R is not a finite value")
  if (any(x < 0)) stop(sprintf("R < 0 (value: %.2f)", x[x<0]))
}



assert_n_contacts <- function(x) {
  if (!is.numeric(x)) stop("n_contacts is not numeric")
  if (!all(is.finite(x))) stop("n_contacts is not a finite value")
  if (any(x < 0)) stop(sprintf("n_contacts < 0 (value: %.2f)", x[x<0]))
  if (!all(is_integer(x))) stop("n_contacts contains decimal values")
}



## A fix for the nonesensical behaviour of `sample` when first argument is of
## length 1.

sample_ <- function(x, ...) {
  x[sample.int(length(x), ...)]
}


