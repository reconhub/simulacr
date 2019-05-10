#' Turn a distribution into a random number generator
#'
#' Internal function. Used to make a random number generator from a distcrete
#' object, a function, or a set of numbers taken to be the PMF on
#' `0:(length(x) - 1)`
#' 
#' @param x can be any of three types: i) a `distcrete` object ii) a `function`
#'   generating random numbers, in which case it should have a single argument
#'   `n` iii) a vector of positive numbers taken as a probability mass function
#'   (pmf) on 0, 1, 2, ..., `(length(x) - 1)`, in which case it is used to draw
#'   numbers from this pmf.
#' 
#' @author Thibaut Jombart
#' 
#' @noRd

make_number_generator <- function(x) {

  ## handle `x` input as distcrete object

  if (inherits(x, "distcrete")) {
    return(x$r)
  }


  ## handle `x` input as function generating random positive integers
  ## we check that the function has as single argument, and also produces only
  ## positive, finite integers as output
  
  if (is.function(x)) {
    arguments <- as.list(args(x))
    arguments <- arguments[!vapply(arguments, is.null, logical(1))] # need to remove the last `NULL`

    if (length(arguments) > 1) {
      msg <- "if `x` is a function, it should accept a single argument `n`"
      stop(msg)
    }

    test_n <- 100
    test_results <- x(test_n)
    if (!is_integer(test_results)) {
      msg <- sprintf(
          "test code `x(%d)` produces decimal numbers (should be integers)",
          test_n)
      stop(msg)
    }
    
    if (length(test_results) != test_n) {
      msg <- sprintf(
          "test code `x(%d)` produces output with wrong length (expected: %d found: %d)",
          test_n,
          test_n,
          length(test_results))
      stop(msg)
    }
    
    if (any(test_results < 0)) {
      msg <- sprintf(
          "test code `x(%d)` produces some negative numbers",
          test_n)
      stop(msg)
    }
    
    if (any(!is.finite(test_results))) {
      msg <- sprintf(
          "test code `x(%d)` produces some non-finite numbers",
          test_n)          
      stop(msg)
    }

    return(x)
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
