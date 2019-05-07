#' Test that a 'spread' function is correct
#'
#' Internal. Checks that the input is a function, and takes a single argument.
#'
#' @noRd
#' 
#' @author Thibaut Jombart

assert_spread <- function(x) {
  ## check function type first
  if (!is.function(x)) {
    msg <- sprintf(
        "`x` is not a function but a `%s`)",
        class(x)[1])      
    stop(msg)
    
  }
  

  ## check that it takes a single argument
  arguments <- as.list(args(x))
  arguments <- arguments[!vapply(arguments, is.null, logical(1))] # need to remove the last `NULL`

  if (length(arguments) > 1) {
    msg <- "if `x` is a function, it should accept a single argument `n`"
    stop(msg)
  }

  invisible(NULL)
}

