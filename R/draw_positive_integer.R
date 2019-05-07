#' Draw positive integer
#'
#' Internal function. Used to draw positive integers from a user-specified
#' distribution, with some basic controls.
#'
#' @param x either a `function` generating random numbers, in which case it
#'   should have a single argument `n`, or a vector of positive numbers taken as
#'   a probability mass function (pmf) on 0, 1, 2, ..., `(length(x) - 1)`, in which
#'   case it is used to draw numbers from this pmf.
#' 
#' @author Thibaut Jombart
#' 
#' @noRd

draw_positive_integer <- function(x, n) {

  ## handle `x` input as function generating random positive integers
  ## we check that the function has as single argument, and also produces only
  ## positive integers as output
  
  if (is.function(x)) {
    arguments <- as.list(args(x))
    arguments <- arguments[!vapply(arguments, is.null, logical(1))] # need to remove the last `NULL`

    if (length(arguments) > 1) {
      msg <- "if `x` is a function, it should accept a single argument `n`"
      stop(msg)
    }

    out <- x(n)
    if (!is_integer(out)) {
      msg <- "produced numbers are not whole numbers" 
      stop(msg)
    }
    
    if (length(out) != n) {
      msg <- sprintf(
          "produced output has the wrong length (expected: %d found: %d)",
          n,
          length(out))
      stop(msg)
    }
    
    if (any(out < 0)) {
      msg <- "some numbers are negative"
      stop(msg)
    }

    return(out)
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
  out <- sample(pool, replace = TRUE, prob = x)

  out
  
}
