#' Make a discretized gamma distribution
#' 
#' This function is a simple wrapper around the packages *epitrix* and
#' *distcrete* allowing to create a discretised Gamma distribution with given
#' mean and standard deviration, returning a `distcrete` object.
#'
#' @export
#' 
#' @param mean the mean of the distribution
#'
#' @param sd the standard deviation of the distribution
#'
#' @author Thibaut Jombart
#'
#' @examples
#'
#' serial_interval <- make_disc_gamma(15, 9)
#'
#'
#' ## check PMF
#' plot(0:30, serial_interval$d(0:30), type = "h",
#'      lwd = 3, col = "navy",
#'      xlab = "Number of days", ylab = "probability",
#'      main = "Probability mass function")
#'
#' ## simulate random numbers
#' serial_interval$d(20)
#' 

make_disc_gamma <- function(mean, sd) {
  params <- epitrix::gamma_mucv2shapescale(mean, sd / mean)
  distcrete::distcrete("gamma",
                       interval = 1,
                       w = 0,
                       shape = params$shape,
                       scale = params$scale)
}
