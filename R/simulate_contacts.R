#' Simulate contacts of cases
#'
#' Under development. Do not use. This simulator adds contacts to the cases of
#' an `outbreak` object, using user-defined distributions for numbers and
#' duration of contacts. It returns an `outbreaks` object with cases and
#' contacts, identifying entries in the linelist as cases or contacts, and links
#' as exposures or transmissions.
#'
#' @param x an `outbreak` object as returned by `simulate_outbreak`
#'
#' @param n_contacts a vector of values to be used as average number of
#'   contacts; values will be drawn at random from this vector to determine the
#'   expected number of contacts for each new case
#'
#' @param dist_time_to_contact the distribution of the delay to new exposures,
#'   in days after the onset of symptoms
#'
#' @param dist_duration the distribution of duration of the exposure period
#'   after the first day, i.e. the time interval between the start and the end
#'   of the exposure window
#'
#' @seealso `simulate_outbreak`
#'
#' @export
#' 
#' @author Thibaut Jombart \email{thibautjombart@@gmail.com}
#'
#' @examples
#' 
#' ## first simulate an outbreak
#' ## (see example in ?simulate_outbreak)
#' incubation <- c(0, 1, 1, 1, 1) # numbers = unscaled PMF
#' infectious_period <- make_disc_gamma(10, 7) # distcrete object
#' reporting <- function(x) dpois(x, 5) # PMF function
#' set.seed(1)
#' x <- simulate_outbreak(R = runif(100, 1, 3), # random values on [1;3]
#'                        dist_incubation= incubation,
#'                        dist_infectious_period = infectious_period,
#'                        dist_reporting = reporting)
#'
#' ## simulate contacts: inputs
#'
#' ## exposure starts 0-2 days post onset
#' time_to_contact = c(1, 1, 1)
#' 
#' ## geom dist for duration of exposure
#' duration <- function(x) dgeom(x, prob = .9)
#' 
#' x_with_contacts <- simulate_contacts(
#'     x[1:10, ],
#'     n_contacts = 1:10, # 1 to 10 contacts
#'     dist_time_to_contact = time_to_contact,
#'     dist_duration = duration)
#'
#' ## check output
#' class(x_with_contacts)
#' dim(x_with_contacts)
#' head(x_with_contacts)
#' plot(x_with_contacts)
#' 

simulate_contacts <- function(x,
                              n_contacts, # average number of contacts
                              dist_time_to_contact, # time onset -> contact start
                              dist_duration # contact start -> end
                              ) {

  if (!inherits(x, "outbreak")) {
    msg <- sprintf("`x` is not an `outbreak` object but a %s",
                   class(x)[1])
    stop(msg)
  }
  if (nrow(x) < 1) {
    msg <- "`x` must have at least one raw (case)"
    stop(msg)
  }
  
  ## General strategy

  ## We simulate contacts for each case of the transmission tree. For each case,
  ## we:
  ## 
  ## - draw the number of contacts
  ## - draw the contacts's ids
  ## - define the starting date of the exposure based on input distribution
  ## - define the end date of the exposure based on input distribution
  ## - pool contacts and cases into a single `epicontacts` object
  
  ## For the handling of distributional inputs: see 'simulate_outbreak.R'

  
  ## make random distribution from inputs, from pmf to random numbers
  x_values <- 0:1000 
  pmf_time_to_contact <- make_pmf(dist_time_to_contact)
  pmf_duration <- make_pmf(dist_duration)
  r_time_to_contact <- make_number_generator(pmf_time_to_contact(x_values))
  r_duration <- make_number_generator(pmf_duration(x_values))

  
  ## random number generator for n_contacts
  assert_n_contacts(n_contacts)
  r_n_contacts <- function(n = 1) sample_(n_contacts, n, replace = TRUE)


  ## generate contacts for each case (list of data.frames)
  n_cases <- nrow(x)
  contacts <- vector(n_cases, mode = "list")
  all_n_contacts <- r_n_contacts(n_cases)
  
  for (i in seq_len(n_cases)) {
    current_id <- x$id[i]
    current_onset <- x$date_onset[i]
    current_n_contacts <- all_n_contacts[i]
    current_contacts <- data.frame(
        id = draw_labels(current_n_contacts),
        source = rep(current_id, current_n_contacts),
        date_exposure_start = x$date_onset[i] +
          r_time_to_contact(current_n_contacts)
    )
    current_contacts$date_exposure_end <-  current_contacts$date_exposure_start +
      r_duration(current_n_contacts)
    contacts[[i]] <- current_contacts
  }
  

  ## pull all contacts together
  contacts <- do.call(rbind.data.frame, contacts)
  contacts$id <- as.character(contacts$id)
  contacts$source <- as.character(contacts$source)


  ## merge original case data with contacts; this involves adding exposure dates
  ## for cases, and keeping track of what is a case, or a contact
  ## 
  ## TODO: for now the start of the exposure is always the date of infection; we
  ## need to be able to change this whilst respecting the constraints on the
  ## date of infection and duration of exposure period
  
  contacts$type <- "non_case"
  x$type <- "case"
  x$date_exposure_start <- x$date_infection
  x$date_exposure_end <- x$date_infection + r_duration(n_cases)

  out <- suppressMessages(dplyr::full_join(x, contacts))

  new_order <- order(out$date_exposure_start)
  out <- out[new_order, ]
  
  class(out) <- class(x)
  attr(out, "has_contacts") <- TRUE
  out
}

