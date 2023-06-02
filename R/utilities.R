

#' check if the database field is empty
#'
#' If the database field is empty, returns a message (on silent = FALSE) to suggest running \code{use_database} to set the database name.
#' Returns TRUE or FALSE according as the name field holds a name or not. Does not check for the actual existence of the database.
#'
#'@param stardog Stardog object
#'@param silent if TRUE, suppress output.
#'@returns Returns a Boolean.
#'

check_db <- function(stardog, silent = TRUE) {
  if (is.na(stardog$database)) {
    if (!silent) {
      message("Run use_database to assign a database name to stardog")
    }
    check <- FALSE
  } else {
    check <- TRUE
  }
  return(check)
}

#' Checks if a database exists
#'
#' @param stardog  A stardog object
#' @param dbName Name of the database being checked (string)
#'
#' @details
#' This function checks if the database in the stardog object actually
#' exists on the Stardog incidence. Compare to check_db(), which only
#' checks if the name of the database is stored in the Stardog object.
#'
#'
#' @returns Boolean

check_database <- function(stardog, dbName) {
  dbList <- list_databases(stardog)
  if (dbName %in% dbList) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Start a transaction
#'
#'
#' @param stardog Stardog object
#' @returns Stardog object with the tx id in the transaction field.
#' @details
#' Some operations on the Stardog database require that a transaction be initiated. A transaction
#' id is generated, which can be used to roll back the transaction should a problem arise.
#'
begin_tx <- function(stardog) {
  tx_url <- paste(stardog$endpoint, stardog$database, "transaction", "begin", sep = "/")
  r <- POST(tx_url, authenticate(stardog$username, stardog$password),
            body = list(db = stardog$database),
            accept('text/plain'))
  stardog$transaction = content(r, encoding = "UTF-8")
  stardog
}

#' Roll back a transaction
#'
#' @param stardog Stardog object
#' @returns Stardog object with the transaction id set to NA. Rolls back the transaction

rollback_tx <- function(stardog) {
  tx_id <- stardog$transaction
  tx_url <- paste(stardog$endpoint, stardog$database, "transaction", "rollback", tx_id, sep = "/")
  r <- POST(tx_url, authenticate(stardog$username, stardog$password),
            body = list(db = stardog$database, txid = tx_id))
  stardog$transaction = NA
  stardog
}

#'Commit a transaction
#'
#' @param stardog Stardog object
#' @returns stardog object, with transaction set to NA.
#'
#' @details
#' This function is part of the transaction sequence. It issues a request to commit
#' the transaction whose id is in the Stardog object. If successful, the transaction ID
#' is set to NA
#'
commit_tx <- function(stardog) {
  tx_id <- stardog$transaction
  tx_url <- paste(stardog$endpoint, stardog$database, "transaction", "commit", tx_id, sep = "/")
  r <- POST(tx_url, authenticate(stardog$username, stardog$password),
            body = list(db = stardog$database, txid = tx_id))
  stardog$transaction <- NA
  stardog
}

#' Update options
#'
#' Options for update queries
#'
#' @details
#' This is a wheeze for providing constants to the rest of stardogR. See the
#' documentation for query_options()
#'
#' @returns Vector of parameters
#' @export
#'
update_options <- function(){
  c("reasoning", "schema", "timeout", "graph-uri")
}

#' Query options
#'
#' Options for informational queries
#'
#' @details
#' This is a wheeze for providing some constants to the rest of stardogR. These are the options
#' allowed by certain functions. The output of query_options can be used to check if the
#' supplied parameters fall into the allowed list.
#'
#' It is available for export so that users can see the list of currently implemented
#' parameter options. There should be no other reason for invoking it.
#'
#' @returns Vector of parameters: reasoning, schema, timeout, graph-uri, offset and limit.
#' @export
#'
query_options <- function(){
  c("reasoning", "schema", "timeout", "graph-uri", "offset", "limit")
}

#' Parse optional parameters
#'
#' @param params Named list of optional parameters
#' @param optionFun Function to supply the list of allowed parameters
#' @returns Boolean. TRUE if the parameter names are all in the allowed list.
#'
parse_options <- function(params, optionFun) {
  if (all(names(params) %in% optionFun())) {
    output <- TRUE
  } else {
    output <- FALSE
  }
  output
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
  output <- content(r, type = "application/json", encoding = "UTF8")
  dfNames <- unlist(output$head$vars)
  results <- output$results[[1]]
  df <- as.data.frame(do.call(cbind, map(dfNames, function(x)unlist(map(map(results, x), "value")))),
                      stringsAsFactors = FALSE)
  if (nrow(df) > 0) {
    names(df) <- dfNames
    df <- utils::type.convert(df, as.is = TRUE, numerals = "allow.loss")
  }
  df
}

#' Extract results of an ASK query from the returned content
#'
#' This function is used internally by the query function
#' @param r the response value from a Get.
#' @returns Boolean containing the results of the ASK query
#'
#'
#'
fix_results_ask <- function(r) {
  output <- content(r, type = "application/json", encoding = "UTF8")
  results <- output$boolean
  results
}

