#' Basic methods for outbreaks objects
#'
#' These functions implement basic methods for outbreaks objects. They include:
#' * `as_epicontacts`: conversion to `epicontacts` object
#' * `as.epicontacts`: alias for `as_epicontacts`
#'
#' @rdname outbreak_methods
#'
#' @aliases outbreak outbreak_class as_epicontacts as.epicontacts
#'   as_epicontacts.outbreaks as.epicontacts.outbreaks
#'
#' @export
#'

as_epicontacts <- function(...) {
  UseMethod("as_epicontacts")
}





#' @rdname outbreak_methods
#' @export

as.epicontacts <- as_epicontacts





#' @rdname outbreak_methods
#' @export

as_epicontacts.outbreak <- function(x) {
  ## isolate columns for nodes, then edges, then make the epicontact object
  ## using the constructor
  
  to_keep  <- c("id", "date_infection", "date_onset", "date_report")
  nodes <- x[to_keep]

  to_keep <- c("infector", "id")
  edges <- x[to_keep]

  out <- epicontacts::make_epicontacts(
                          nodes,
                          edges,
                          directed = TRUE
                      )
  out
}

