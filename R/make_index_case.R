#' Function to simulate a first case
#'
#' @nrRd
#' 

make_index_case <- function(...) {


  out <- list(...)
  if (length(out) == 1L && is.list(out[[1]])) {
    out <- out[[1]]
  }

  ## SET DEFAULTS
  defaults <- list(id = NULL,
                   infector = NA,
                   date_infection = 0,
                   date_onset = NULL,
                   date_report = NULL,
                   R = 0)

  ## MODIFY OUT WITH ARGUMENTS ##
  out <- modify_defaults(defaults, out)
  out
}
