#' Extract the PMF of a distribution
#'
#' Internal function. Used to extract proabability mass function (PMF) from a distcrete
#' object, a function, or a set of numbers taken to be the PMF on `0:(length(x) - 1)`
#' 
#' @param x can be any of three types: i) a `distcrete` object ii) a `function`
#'   generating random numbers, in which case it should have a single argument
#'   `n` iii) a vector of positive numbers taken as a probability mass function
#'   (pmf) on 0, 1, 2, ..., `(length(x) - 1)`, in which case it is standardised
#'   to sum to 1.
#' 
#' @author Thibaut Jombart
#' 
#' @noRd

make_pmf <- function(x) {

  ## handle `x` input as distcrete object

  if (inherits(x, "distcrete")) {
    return(x$d)
  }
  


  ## handle `x` input as function generating random positive integers we check
  ## that the function has as single argument, and also produces only positive,
  ## finite numbers on a test input
  
  if (is.function(x)) {
    arguments <- as.list(args(x))
    arguments <- arguments[!vapply(arguments, is.null, logical(1))] # need to remove the last `NULL`

    if (length(arguments) > 1) {
      msg <- "if `x` is a function, it should accept a single argument"
      stop(msg)
    }

    test_values <- 0:100
    test_results <- x(test_values)
    
    if (length(test_results) != length(test_values)) {
      msg <- sprintf(
          "test code `x(0:100)` produces output with wrong length (expected: %d found: %d)",
          length(test_values),
          length(test_results))
      stop(msg)
    }
    
    if (any(test_results < 0)) {
      msg <- "test code `x(0:100)` produces some negative numbers"
      stop(msg)
    }
    
    if (any(!is.finite(test_results))) {
      msg <- "test code `x(0:100)` produces some non-finite numbers"          
      stop(msg)
    }

    return(x)
  }



  ## handle `x` input as a numeric values
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
