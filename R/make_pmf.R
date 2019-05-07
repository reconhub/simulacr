#' Extract the PMF of a distribution
#'
#' Internal function. Used to extract proabability mass function (PMF) from a distcrete
#' object, or a set of numbers taken to be the PMF on `0:(length(x) - 1)`
#' 
#' @param x either a `distcrete` object or a vector of positive numbers taken as
#'   a probability mass function (pmf) on 0, 1, 2, ..., `(length(x) - 1)`, in which
#'   case it is used to draw numbers from this pmf.
#' 
#' @author Thibaut Jombart
#' 
#' @noRd

make_pmf <- function(x, n) {

  ## handle `x` input as distcrete object

  if (inherits(x, "distcrete")) {
    return(x$d)
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

  ## standardise the values and generate a function
  pmf <- x / sum(x)
  pool <- as.integer(seq_along(pmf) - 1L)
  
  function(x) {
    x <- as.integer(round(x))
    out <- double(length(x))
    
    p_not_zero <- (x >= 0) & (x <= max(pool))
    out[p_not_zero] <- pmf[x[p_not_zero] + 1]
    out
  }
}
