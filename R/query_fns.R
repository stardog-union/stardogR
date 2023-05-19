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

query <- function(stardog, q = "select (count(*) as ?n) {?s ?p ?o .}", namedGraph = NA, ...) {
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


#' Extract results of a GET query from the returned value
#'
#' This function is used internally by the query function
#' @param r the response value from a Get.
#' @returns Dataframe containing the results of the query.
#'
#' @details
#' R utility `type.convert` is used to convert the columns to the most likely datatypes
#' Typically, it will do a good job on integers, decimals and booleans. Dates are
#' returned as character strings
#'
#' @importFrom purrr map
#'
fix_results <- function(r) {
  if(isTRUE(r$headers$`content-type` == "application/trig")){x = r$content %>% rawToChar() ; return(x)} ## handle construct and describe queries
  x = r$content %>% rawToChar() %>% fromJSON()
  if(isTRUE(all(names(x) == c("head", "boolean")))){return(x$boolean)} ## handle ask queries
  df = x$results$bindings %>% map_df(function(col){col=col$value}) ## only keep the values
  dfNames = x$head$vars %>% unlist()
  df[ , dfNames[dfNames %notin% names(df)]] = NA ## keep variable columns even if empty  ## uses fn: `%notin%` <- Negate(`%in%`)
  df = df %>% mutate_all(as.character) %>%  select(all_of(dfNames))  # preserve column datatype and requested query variable order
  return(df)
}

