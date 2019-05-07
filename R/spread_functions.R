#' Functions for spreading infections
#'
#' Internal. These functions are used for creating new items (e.g. date of infection,
#' onset, reporting) of a secondary case based on its infector. All of these
#' generate closures with encapsulated parameters.
#'
#' @author Thibaut Jombart
#' 
#' @noRd
#'


#' Make a discretized gamma distribution
#' @noRd

make_disc_gamma <- function(mean, sd) {
  params <- epitrix::gamma_mucv2shapescale(mean, sd / mean)
  out <- distcrete::distcrete("gamma",
                              interval = 1,
                              w = 0,
                              shape = params$shape,
                              scale = params$scale)
  out$r
}




#' Make a Poisson distribution to draw R
#' @noRd

make_R_poisson <- function(mean) {
  function(n) stats::rpois(n, lambda = mean)
}



#' Make random labels of a given size
#' @noRd

draw_labels <- function(n = 1, size = 6) {
  stringi::stri_rand_strings(n = n, length = size, pattern = "[A-Za-z0-9]")
}
