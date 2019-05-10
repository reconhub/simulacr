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
#' @param x an object to be converted to `epicontacts`
#'

as_epicontacts <- function(...) {
  UseMethod("as_epicontacts")
}





#' @rdname outbreak_methods
#' @export

as.epicontacts <- as_epicontacts





#' @rdname outbreak_methods
#' @export

as_epicontacts.outbreak <- function(x, ...) {
  ## isolate columns for nodes, then edges, then make the epicontact object
  ## using the constructor; as some columns may be optional depending on
  ## simulations input (e.g. date_report) we make sure to retain only columns
  ## present in 'x' (e.g. using `intersect`)
  
  to_keep <- intersect(
      c("id", "date_infection", "date_onset", "date_report"),
      names(x))
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







#' @rdname outbreak_methods
#' @export
#' @importFrom graphics plot
#' @param ... for `plot`, arguments passed to `plot.epicontacts` in the
#'   `epicontacts` package.

plot.outbreak <- function(x, ...) {
  plot(as.epicontacts(x), ...)
}
