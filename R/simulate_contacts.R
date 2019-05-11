#' Simulate contacts of cases
#'
#' Under development. Do not use. This simulator adds contacts to the cases of
#' an `outbreak` object, using user-defined distributions for numbers and
#' duration of contacts. It returns an `epicontacts` object with cases and
#' contacts, identifying entries in the linelist as cases or contacts, and links
#' as exposures or transmissions.
#'
#' @param n_contacts a vector of values to be used as reproduction number; values
#'   will be drawn at random from this vector to determine the expected numbero
#'   of secondary cases for each new case
#'
#' @param dist_duration the distribution of the incubation period, i.e. the
#'   time interval between infection and onset of symtpoms
#'
#' @export
#' 
#' @author Thibaut Jombart \email{thibautjombart@@gmail.com}
#'
#' @examples
#' 
#' ## make toy distributions (showing the 3 possible input types)
#' incubation <- c(0, 1, 1, 1, 1) # numbers = unscaled PMF
#' infectious_period <- make_disc_gamma(10, 7) # distcrete object
#' reporting <- function(x) dpois(x, 5) # PMF function
#' set.seed(1)
#' x <- simulate_outbreak(R = runif(100, 1, 3), # random values on [1;3]
#'                        dist_duration= incubation,
#'                        dist_infectious_period = infectious_period,
#'                        dist_reporting = reporting)
#' dim(x)
#' head(x)
#' tail(x)
#' if (require(epicontacts)) {
#'   plot(x)
#' }

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

  browser()
  
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
        id = draw_labels(case_n_contacts),
        infector = rep(current_id, current_n_contacts),
        date_exposure_start = x$date_onset[i] +
          r_time_to_contact(current_n_contacts)
    )
    current_contacts$date_exposure_end <-  current_contacts$date_exposure_start +
      r_duration(current_n_contacts)
    contacts[[current_id]] <- current_contacts
  }
  

  ## pull all contacts together
  contacts <- do.call(rbind.data.frame, contacts)

  
  ## add info to a new tree
  ids_cases <- unique(as.character(all_contacts$from))
  ids_contacts <- unique(as.character(all_contacts$to))
  ids <- c(ids_cases, ids_contacts)
  type <- rep(c("case", "contact"),
              c(length(ids_cases), length(ids_contacts)))
  nodes <- data.frame(id = ids, type = type)
  edges <- rbind(x$contacts, all_contacts)

  make_epicontacts(nodes, edges, directed = TRUE)


  
}

