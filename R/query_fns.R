#' Query Stardog
#'
#' Issues a query to Stardog and return the results in a dataframe.
#'
#' @param stardog A stardog object for the connect, containing a database
#' @param q The query expressed as a string
#' @param namedGraph Run the query against this named graph
#' @param ... Options for the query. See Details
#' @export
#' @details
#' Additional parameters:
#'
#' - limit (integer) number of results to return
#'
#' - reasoning (boolean) turn on reasoning. Not needed if a schema is included.
#'
#' - schema (string) name of the reasoning schema
#'
#' - offset (integer) number of results to skip before returning rows.
#'
#'
#'
query <- function(stardog, q = 'select (count(*) as ?n) {?s ?p ?o .}', namedGraph = NA, ...) {
  q_options <- list(...)
  if (!is.na(namedGraph)) {
    q_options <- c(q_options, `default-graph-uri` = namedGraph)
  }
  if (length(options) == 0) {
    query_list <- list(query = q)
  } else if (all(names(q_options) %in% c("limit", "reasoning", "useNamespaces", "schema", "offset", "default-graph-uri"))) {
    query_list <- c(query = q, q_options)
  } else {
    stop(
"Only the following options are allowed:
  - limit (integer)
  - reasoning (boolean)
  - offset (integer)
  - useNamespaces (boolean)
  - schema (string)
  - namedGraph (string) [note: use this instead of default-graph-uri]
"
    )
  }
  query_url <- paste(stardog$endpoint, stardog$database, "query", sep = "/")
  r <- GET(query_url, authenticate(stardog$username, stardog$password), accept("application/sparql-results+json,application/trig"),
           query = query_list)
  return(fix_results(r))
}


