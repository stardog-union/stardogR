# Functions to add data

#'Add a string of turtle
#'
#' Supply RDF in turtle format as a (possibly) multi-line string
#'
#' @param stardog Stardog object
#' @param ttl Some lines of turtle in a string.
#' @param graph optional. name of the named graph to receive the new data
#' @param path boolean. If true, then ttl refers to the path to the ttl file.
#' @returns The status code. A message is issued to indicate success or failure. Upon failure
#' it returns the status code
#'
#' @export
add_ttl <- function(stardog, ttl = NULL, graph = NULL, path = TRUE) {
  if (length(stardog$database) == 0) {
    cat("Stardog object must include a database")
    return()
  }
  if (path) {
    ttl <- readr::read_file(ttl)
  }
  stardog <- begin_tx(stardog)
  tx_id <- stardog$transaction

  post_url <- paste(stardog$endpoint, stardog$database, tx_id, "add", sep = "/")
  if (length(graph) > 0) {
    post_url <- paste(post_url, "?graph-uri=", graph, sep = "")
  }

  r <- POST(post_url, authenticate(stardog$username, stardog$password),
            content_type('text/turtle'),
            body = ttl)
  stardog <- commit_tx(stardog)
  status <- r$status_code
  #if (status == 200) {
  #  message("Data added successfully!")
  #} else {
  #  message("The request failed: ", status)
  #}
  status
}

#' Materialize a dataframe to Stardog
#'
#' @description
#' Serializes the data frame and posts to Stardog using the supplied mapping string.
#' The data can be sent to a named graph. The parallel option allows use of readr function
#' format_delim. This function calls a multi-threaded C routine which appears to cause random
#' crashes in RStudio. Under the default (parallel = FALSE), the dataframe is serialized using
#' a single threaded R function.
#'
#' @param stardog Stardog object
#' @param df Dataframe with the data to be imported to Stardog
#' @param mapping Mapping function expressed as a string
#' @param graph optional named of named graph to receive the data
#' @param verbose if TRUE, get more details about the request
#' @param parallel If True use multi-threaded
#' @returns status of the post operation
#' @importFrom httr verbose
#' @export
add_dataframe <- function(stardog, df, mapping, na = "", graph = NULL,
                          verbose = FALSE, parallel = TRUE) {
  stopifnot(is.data.frame(df))
  if (parallel) {
    df_io <- readr::format_delim(df, delim = ',', na = na)
  } else {
    df_io <- format_delim_single_thread(df, delim = ',', na = na)
  }
  input_file_type <- 'DELIMITED'

  import_url <- paste(stardog$endpoint, "admin", "virtual_graphs", "import", sep = "/")
  post_body <- list(database = stardog$database,
                    mappings = mapping,
                    options = "{}",
                    input_file_type = input_file_type,
                    input_file = df_io
  )

  if (!is.null(graph)) {
    post_body <- list(database = stardog$database,
                      mappings = mapping,
                      options = "{}",
                      graph = graph,
                      input_file_type = input_file_type,
                      input_file = df_io
    )
  }

  if (verbose) {
    r <- POST(import_url, authenticate(stardog$username, stardog$password),
              body = post_body,
              encode = 'multipart',
              verbose()
    )
  } else {
  r <- POST(import_url, authenticate(stardog$username, stardog$password),
            body = post_body,
            encode = 'multipart'
  )
  }
  r$status
}



#' Run an update query
#' @param stardog Stardog object
#' @param q The update query
#' @param ... Additional parameters for the query
#' @returns Success message for the request
#' @export
#' @importFrom httr POST
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom httr authenticate

update_query <- function(stardog, q, ...) {
  options <- list(...)
  if (length(options) == 0) {
    body_list <- list(query = q)
  } else if (all(names(options) %in% c("limit",
                                       "reasoning",
                                       "schema",
                                       "offset",
                                       "default-graph-uri"))) {
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
