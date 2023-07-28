

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
#' Typically, it will do a good job on integers, decimals and Booleans. Dates are
#' returned as character strings
#'
#' @importFrom purrr map
#' @export
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

#' Convert a IRI with prefix to the full IRI
#' 
#' If the string contains a single :, the functions checks if the first component
#' is a namespace prefix. If it is, then it concerts the string to complete format.
#' For example, if ex is the prefix for http://example.com/, it will convert
#' ex:myGraph to http://example.com/myGraph.
#' 
#' @param stardog The stardog object
#' @param s the string to be converted
#' @returns the converted string, or simply s itself if no prefix was found in the namespace
#' @details
#' This is intended to parse named graphs entered as parameters. The function is not vectorized
#' and adjusts only one string in each call.
#' 
prefix_to_iri <- function(stardog, s){
  check <- unlist(strsplit(s, split = ":", fixed = TRUE))
  if (length(check) != 2) {
    # Not in format prefix:field
    output <- s
  } else {
    pref <- check[1] # Must be the prefix
    field <- check[2] # we need this bit.
    namespaces <- get_namespaces(stardog, raw = FALSE)
    prefix_row <- match(pref, namespaces$prefix)
    if (is.na(prefix_row)) {
      output <- s # Couldn't find the prefix
    } else {
      output <- paste(namespaces$name[prefix_row], field, sep = "")
    }
  }
  output
}

#' changes iri nodes with complete IRI bases to prefix: format
#' 
#' This function is intended to adjust query results to use the namespace
#' prefixes instead of complete IRI's. Typically, this function will be applied
#' to a vector of results. We can't assume that every item in the vector will have
#' the same base IRI and prefix, given the optional keyboard, but typically,
#' the prefix will be the same.
#' 
#' @param x a vector of results from a query, potentially containing iri bases
#' @param stardog the stardog object. 
#' @returns Returns a transformed vector if all of the values have partial matches
#' in the namespace file. If some cannot be matched because, say, they are literals, then
#' it returns the vector unchanged
#' 
#' @details
#' Note that this is the only function here in which the stardog object is in second place. That
#' is because the primary application of this function is as applied to the dataframe output
#' of a select query, and column vector has to be in first position.
#' 
#'
iri_to_prefix <- function(x, stardog) {
  # This is code golf. Anyway, matches is a Boolean array with exactly one true value in each row
  # the column with TRUE corresponds to the matching row in the DF of namespaces
  # The matrix apply basically extracts the right prefix for each row.
  # We also extract the iri base corresponding to each result
  
  if (inherits(x, "character")) return(x) # Clearly not an IRI value
  namespaces <- get_namespaces(stardog, raw = FALSE)
  matches <- outer(x, namespaces$name, startsWith)
  check <- all(apply(matches, 1, any))
  if (check) {
    # Check is true if every row of x matches an IRI base somewhere.
    # If there's something wrong, return the original value unchanged.
    tempFun <- function(y, evaluator) {
      evaluator[y]
    }
    prefixes <- apply(matches, 1, tempFun, evaluator = namespaces$prefix)
    iri_bases <- apply(matches, 1, tempFun, evaluator = namespaces$name)
    # Split x into a blank character and what remains after the iri_base is stripped
    # We then replace the blank with the prefix and collapse
    output <- do.call(rbind, strsplit(x, iri_bases, fixed = TRUE))
    output[,1] <- prefixes
    output <- apply(output, 1, paste, collapse = ":")
    } else {
      # Yeah. We'nre not going to mess with this.
      output <- x
    }
  return(output)
}