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
      c("id", "date_infection", "date_onset", "date_report",
        "type", "date_exposure_start", "date_exposure_end"),
      names(x))
  nodes <- x[to_keep]

  to_keep <- c("source", "id")
  edges <- x[to_keep]

  ## add type of link if there are contacts, i.e. not all links are transmission
  ## events but also exposures which did not lead to transmission
  if (attr(x, "has_contacts")) {
    type <- x$type
    is_case <- type == "case"
    type[is_case] <- "transmission"
    type[!is_case] <- "contact"
    edges$type <- type
  }

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
  net <- as.epicontacts(x)
  
  if (attr(x, "has_contacts")) {
    type_pal <- grDevices::colorRampPalette(c("#ff8080", "#c2d6d6"))
    out <- plot(net, node_color = "type", col_pal = type_pal)
  } else {
    out <- plot(net, ...)
  }

  out
}
