# Functions dealing with named graphs

#' List all named graphs in the database
#'
#' @param stardog Stardog object
#' @returns Vector of named graphs
#'
#' @export
list_graphs <- function(stardog) {
  output <- query(stardog, q = "select ?g {graph ?g {}}")
  unlist(output)
}
