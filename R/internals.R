
assert_R <- function(x) {
  if (!is.numeric(x)) stop("R is not numeric")
  if (!all(is.finite(x))) stop("R is not a finite value")
  if (any(x < 0)) stop(sprintf("R < 0 (value: %.2f)", x[x<0]))
}



## A fix for the nonesensical behaviour of `sample` when first argument is of
## length 1.

sample_ <- function(x, ...) {
  x[sample.int(length(x), ...)]
}


