#' Select query against Stardog
#'
#' Issues a select query to Stardog and return the results in a dataframe.
#'
#' @param stardog A stardog object for the connect, containing a database
#' @param q The query expressed as a string
#' @param graph Run the query against this named graph.
#' @param pretty description TRUE to replace the base iri with prefixes, where possible.
#' @param ... Options for the query. See Details
#' @returns a dataframe containing the results.
#' @details
#' The graph parameter needs to be written out as a full IRI, say
#' "http://example.com/myGraph" rather than ex:myGraph.
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
select <- function(stardog, q = 'select (count(*) as ?n) {?s ?p ?o .}', graph = NA, pretty = TRUE, ...) {
  # check if this is a select query
  if (length(grep("select", q, ignore.case = TRUE, fixed = FALSE)) == 0) {
    simpleError(message = "The query must contain the keyword 'select' ")
  }
  options <- list(...)
  if (!is.na(graph)) {
    options <- c(options, `default-graph-uri` = graph)
  }
  if (length(options) == 0) {
    query_list <- list(query = q)
  } else if (all(names(options) %in% c("limit", "reasoning", "useNamespaces", "schema", "offset", "default-graph-uri"))) {
    query_list <- c(query = q, options)
  } else {
    stop(
"Only the following options are allowed:
  - limit (integer)
  - reasoning (boolean)
  - offset (integer)
  - useNamespaces (boolean)
  - schema (string)
  - graph (string) [note: use this instead of default-graph-uri]
"
    )
  }
  query_url <- paste(stardog$endpoint, stardog$database, "query", sep = "/")
  r <- GET(query_url, authenticate(stardog$username, stardog$password),
           accept("application/sparql-results+json,application/trig"),
           query = query_list)
  output <- fix_results(r)
  if (pretty && nrow(output) > 1) {
    # Adjust for pretty output with prefixes
    output <- apply(output, 2, iri_to_prefix, stardog = stardog)
    output <- as.data.frame(output)
  }
  output
}

#' Issues an ask query to Stardog and return the result as a boolean
#'
#' @param stardog A stardog object for the connect, containing a database
#' @param q The query expressed as a string. Must be an ask query
#' @param graph Run the query against this named graph
#' @param ... Options for the query. See Details
#' @returns boolean
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
ask <- function(stardog, q = NA, graph = NA, ...) {
  # check if this is an ask query
  if (length(grep("ask", q, ignore.case = TRUE, fixed = FALSE)) == 0) {
    simpleError(message = "The query must contain the keyword 'ask' ")
  }
  q_options <- list(...)
  if (!is.na(graph)) {
    q_options <- c(q_options, `default-graph-uri` = graph)
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
  - graph (string) [note: use this instead of default-graph-uri]
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
#' @param graph Run the query against this named graph
#' @param ... Options for the query. See Details
#' @returns the constructed RDF
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
construct <- function(stardog, q = NA, graph = NA, ...) {
  # check if this is a construct query
  if (length(grep("construct", q, ignore.case = TRUE, fixed = FALSE)) == 0) {
    simpleError(message = "The query must contain the keyword 'ask' ")
  }
  q_options <- list(...)
  if (!is.na(graph)) {
    q_options <- c(q_options, `default-graph-uri` = graph)
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
  - graph (string) [note: use this instead of default-graph-uri]
"
    )
  }
  query_url <- paste(stardog$endpoint, stardog$database, "query", sep = "/")
  r <- GET(query_url, authenticate(stardog$username, stardog$password), accept("text/turtle"),
           query = query_list)
  content(r, encoding = "UTF-8")
}

#' Run an update query
#' @param stardog Stardog object
#' @param q The update query
#' @param graph Default graph for the "where" portion of the query
#' @param ... Additional parameters for the query
#' @returns Success message for the request
#' @importFrom httr POST
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom httr authenticate

update_query <- function(stardog, q, graph = NA, ...) {
  options <- list(...)
  if (!is.na(graph)) {
    options <- c(options, `default-graph-uri` = graph)
  }
  if (length(options) == 0) {
    body_list <- list(query = q)
  } else if (all(names(options) %in% c("limit",
                                       "reasoning",
                                       "schema",
                                       "offset",
                                       "default-graph-uri"
                                       ))) {
    body_list <- c(query = q, options)
  } else {
    stop("Only the following options are allowed:
              - limit (integer)
              - reasoning (boolean)
              - offset (integer)
              - useNamespaces (boolean)
              - schema (string)
              - `default-graph-uri` (NOTE THE BACKTICKS. THESE ARE IMPORTANT)
              "
    )
  }
  query_url <- paste(stardog$endpoint, stardog$database, "update", sep = "/")
  r <- POST(query_url, authenticate(stardog$username, stardog$password),
            body = body_list, encode = "form"
  )
  if (r$status_code == 200) {
    return("Update successful")
  } else if (r$status_code == 400 || r$status_code == 404) {
    return(content(r, encoding = stardog$encoding)$message)
  } else {
    return(r)
  }
}

#' Wrapper for specific query functions ask, construct and select
#'
#' Detects the type of query and runs the appropriate function
#' @param stardog Stardog object
#' @param q An ask, construct, update or select query
#' @param graph runs the query against the specified named graph
#' @param pretty True when we want the select query to adjust the output with prefixes.
#' @param ... Options for the query. See functions ask, construct and select
#' @details
#' The graph parameter must be written out in full. "http://example.com/myGraph", say,
#' instead of "ex:myGraph". Stardog does not parse the prefix in this context.
#'
#' @returns the results of the query
#' @export
query <- function(stardog, q = "select (count(*) as ?n) {?s ?p ?o .}", graph = NA, pretty = TRUE, ...) {
  if (!is.na(graph)) {
    graph <- prefix_to_iri(graph, stardog)
  }
  if (length(grep("construct", q, ignore.case = TRUE)) > 0) {
    output <- construct(stardog, q = q, graph = graph, ...)
  } else if (length(grep("ask", q, ignore.case = TRUE)) > 0) {
      output <- ask(stardog, q = q, graph = graph, ...)
  } else if (length(grep("insert|delete", q,  ignore.case = TRUE)) > 0) {
    output <- update_query(stardog, q, graph = graph, ...)
  } else if (length(grep("select", q, ignore.case = TRUE)) > 0) {
      output <- select(stardog, q = q, graph = graph, pretty = pretty, ...)
  } else {
    simpleError("Query must be of the form ASK, construct, insert, delete or select")
  }
  output
}
