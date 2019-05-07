#' Turn a distribution into a random number generator
#'
#' Internal function. Used to make a random number generator from a distcrete
#' object, or a set of numbers taken to be the PMF on `0:(length(x) - 1)`
#' 
#' @param x either a `function` generating random numbers, in which case it
#'   should have a single argument `n`, or a vector of positive numbers taken as
#'   a probability mass function (pmf) on 0, 1, 2, ..., `(length(x) - 1)`, in which
#'   case it is used to draw numbers from this pmf.
#' 
#' @author Thibaut Jombart
#' 
#' @noRd

make_number_generator <- function(x, n) {

  ## handle `x` input as distcrete object

  if (inherits(x, "distcrete")) {
    return(x$r)
  }
  

  ## handle `x` input as a pmf
  ## we check that the input is numeric, has only finite and positive values
  
  if (!is.numeric(x)) {
    msg <- sprintf(
        "`x` is neither a function nor a numeric vector (but a `%s`)",
        class(x)[1])           
    stop(msg)
  }

  if (any(!is.finite(x))) {
    msg <- "some values in 'x' are not finite"
    stop(msg)
  }

  if (any(x < 0)) {
    msg <- "some values in 'x' are not negative"
    stop(msg)
  }

  pool <- as.integer(seq_along(x) - 1L)
  function(n = 1) sample(pool, size = n, replace = TRUE, prob = x)
}
