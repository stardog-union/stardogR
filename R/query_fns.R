#' Select query against Stardog
#'
#' Issues a select query to Stardog and return the results in a dataframe.
#'
#' @param stardog A stardog object for the connect, containing a database
#' @param q The query expressed as a string
#' @param namedGraph Run the query against this named graph
#' @param ... Options for the query. See Details
#' @returns a dataframe containing the results.
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
select <- function(stardog, q = 'select (count(*) as ?n) {?s ?p ?o .}', namedGraph = NA, ...) {
  # check if this is a select query
  if (length(grep("select", q, ignore.case = TRUE, fixed = FALSE)) == 0) {
    simpleError(message = "The query must contain the keyword 'select' ")
  }
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

#' Issues an ask query to Stardog and return the result as a boolean
#'
#' @param stardog A stardog object for the connect, containing a database
#' @param q The query expressed as a string. Must be an ask query
#' @param namedGraph Run the query against this named graph
#' @param ... Options for the query. See Details
#' @returns boolean 
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
ask <- function(stardog, q = NA, namedGraph = NA, ...) {
  # check if this is an ask query
  if (length(grep("ask", q, ignore.case = TRUE, fixed = FALSE)) == 0) {
    simpleError(message = "The query must contain the keyword 'ask' ")
  }
  q_options <- list(...)
  if (!is.na(namedGraph)) {
    q_options <- c(q_options, `default-graph-uri` = namedGraph)
  }
  if (length(options) == 0) {
    query_list <- list(query = q)
  } else if (all(names(q_options) %in% c("reasoning", "useNamespaces", "schema", "offset", "default-graph-uri"))) {
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
  fix_results_ask(r)
}

#' Issues an ask query to Stardog and return the result as a boolean
#'
#' @param stardog A stardog object for the connect, containing a database
#' @param q The query expressed as a string. Must be a construct query
#' @param namedGraph Run the query against this named graph
#' @param ... Options for the query. See Details
#' @returns the constructed RDF 
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
construct <- function(stardog, q = NA, namedGraph = NA, ...) {
  # check if this is a construct query
  if (length(grep("construct", q, ignore.case = TRUE, fixed = FALSE)) == 0) {
    simpleError(message = "The query must contain the keyword 'ask' ")
  }
  q_options <- list(...)
  if (!is.na(namedGraph)) {
    q_options <- c(q_options, `default-graph-uri` = namedGraph)
  }
  if (length(options) == 0) {
    query_list <- list(query = q)
  } else if (all(names(q_options) %in% c("reasoning", "useNamespaces", "schema", "offset", "default-graph-uri", "limit"))) {
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
  r <- GET(query_url, authenticate(stardog$username, stardog$password), accept("text/turtle"),
           query = query_list)
  content(r, encoding = "UTF-8")
}


#' Wrapper for specific query functions ask, construct and select
#' 
#' Detects the type of query and runs the appropriate function
#' @param sg Stardog object
#' @param q An ask, construct or select query
#' @param namedGraph runs the query against the specified named graph
#' @param ... Options for the query. See functions ask, construct and select
#' @returns the results of the query
#' @export
query <- function(sg, q = "select (count(*) as ?n) {?s ?p ?o .}", namedGraph = NA, ...) {
  if (length(grep("construct", q, ignore.case = TRUE)) > 0) {
    output <- construct(sg, q = q, namedGraph = namedGraph, ...)
  } else if (length(grep("ask", q, ignore.case = TRUE)) > 0) {
      output <- ask(sg, q = q, namedGraph = namedGraph, ...)
  } else if (length(grep("select", q, ignore.case = TRUE)) > 0) {
      output <- select(sg, q = q, namedGraph = namedGraph, ...)
  } else {
    simpleError("Query must be of the form ASK, Construct or Select")
  }
  return(output)
}
