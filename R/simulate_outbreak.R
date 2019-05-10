#' Simulate outbreaks
#'
#' Under development. Do not use. This simulator uses a branching process to
#' determine the infectiousness of individuals over time. It determines the
#' dates of events based on user-provided distributions. Distributions can be
#' provided either as `distcrete` objects, or as numbers in which case they are
#' taken to be probability mass functions (PMF) for values on 0, 1, ...,
#' `length(input) - 1`
#'
#' @param duration the number of days to run the simulation for
#'
#' @param dist_incubation the distribution of the incubation period, i.e. the
#'   time interval between infection and onset of symtpoms
#'
#' @param dist_infectious_period the distribution of the infectious period,
#'   i.e. the time interval between the moment a case starts showing symptoms
#'   (onset) and the moment they infect new secondary cases
#'
#' @param dist_reporting the distribution of the reporting delay, i.e. the time
#'   interval between symptom onset and the date at which the case is notified
#'
#' @param dist_R the distribution of the reproduction number, i.e. the expected
#'   number of secondary cases per primary case
#'
#' @author Thibaut Jombart \email{thibautjombart@@gmail.com}


simulate_outbreak <- function(duration = 100, # duration of the simulation
                              population_size = 100,
                              dist_incubation, # infection -> onset
                              dist_infectious_period, # onset -> new infections
                              dist_reporting, # onset -> reporting
                              R_values # average secondary cases
                              ) {


  ## make random distribution from inputs
  r_incubation <- make_number_generator(dist_incubation)
  r_infectious_period <- make_number_generator(dist_infectious_period)
  r_reporting <- make_number_generator(dist_reporting)
  assert_R(R_values)
  r_R <- function(n = 1) sample_(R_values, n, replace = TRUE)

  ## extract PMF of the infectious period
  pmf_infectious_period <- make_pmf(dist_infectious_period)

  ## make index case
  index_case <- make_index_case(id = draw_labels(1),
                                date_infection = 0,
                                date_onset = r_incubation(),
                                R = r_R())
  index_case$date_report <- index_case$date_onset + r_reporting()

  n_susceptibles <- population_size - 1
  

  ## make vectors for the outputs
  out <- index_case
  
  for (t in seq_len(duration)) {

    ## TODO: DESCRIPTION TO BE REVISED!
    
    ## Individual infectiousness at time 't' is defined as the infectious period
    ## pmf multiplied by the individual R; the global force of infection is a
    ## rate defined as the sum of individual infectiousnesses. Divided by the
    ## number of susceptibles, this translates into a per-capita rate of
    ## infection. We derive individual probabilities of infection from
    ## this. Because per-capita rates cannot be calculated when there are no
    ## susceptible, we test for this condition and only proceed to computations
    ## if the are susceptibles left.
    
    if (n_susceptibles > 0) {
      individual_infectiousness <- pmf_infectious_period(t - out$date_onset) * out$R
      rate_infection <- sum(individual_infectiousness) * (n_susceptibles / population_size)
      n_new_cases <- stats::rpois(1, lambda = rate_infection)
      n_new_cases <- min(n_susceptibles, n_new_cases)
      ## proba_infection <- 1 - exp(-rate_infection)
      ## n_new_cases <- rbinom(1, size = n_susceptibles, prob = proba_infection)
    } else {
      n_new_cases <- 0
    }


    ## handle new cases
    if(n_new_cases > 0){

      ## remove susceptibles
      n_susceptibles <- n_susceptibles - n_new_cases

      
      ## build new cases: id, different dates, etc.

      ## infectors are picked from infectious cases, weighted by their
      ## respective infectiousness
      new_infectors <- sample(out$id,
                              size = n_new_cases,
                              replace = TRUE,
                              prob = individual_infectiousness)
      out$infector <- c(out$infector,
                        new_infectors)

      ## id
      out$id <- c(out$id,
                  draw_labels(n_new_cases))
      
      ## new dates of new infections are the current day
      new_date_infection <- rep(t, n_new_cases)
      out$date_infection <- c(out$date_infection,
                              new_date_infection)

      ## new dates of onset picked using the incubation period
      new_date_onset <- t + r_incubation(n_new_cases)
      out$date_onset <- c(out$date_onset,
                          new_date_onset)

      ## new dates of report picked using the delay to reporting
      new_date_report <- new_date_onset + r_reporting(n_new_cases)
      out$date_report <- c(out$date_report,
                           new_date_report)

      ## new R values are drawn at random
      new_R <- r_R(n_new_cases)
      out$R <- c(out$R,
                 new_R)
      
    }
  }


  ## output will be a data.frame with a special type for e.g. printing and
  ## conversion
  
  out <- data.frame(out)
  class(out) <- c("outbreak", class(out))
  out
}

