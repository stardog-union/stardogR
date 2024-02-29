#
# Copyright (c) Catherine Dalzell
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

# Stored queries and models

#' List all stored queries
#'
#' Lists all stored queries, or only those available to the current database
#'
#' @param stardog Stardog object
#' @param allDB Boolean. Under FALSE (default), returns the queries for the currenct database
#'
#' @details
#'  If allDB is TRUE, then the function returns a database with the query names and their respective databases
#'
#'  If alDB is FALSE, it returns a vector of the query names for the current database
#'
#'  If the current database is NA, it returns all stored queries and sends a message.
#'
#' @export

list_stored <- function(stardog, allDB = FALSE) {
  query_url <- paste(stardog$endpoint, "admin/queries/stored", sep = "/")
  r <- GET(query_url, authenticate(stardog$username, stardog$password),
           accept("application/json"))
  queries <- content(r)$queries
  output <- as.data.frame(do.call(cbind, map(c('name', 'database'), function(x)unlist(map(queries, x)))))
  names(output) <- c('query', 'database')
  if (allDB) {
    return(output)
  } else {
      if (!is.na(stardog$database)) {
        return(output[output$database == stardog$database, "query" ])
      } else {
          message("Stardog object does not specify the database")
          return(output)
      }
  }
}


#' Store a named query
#'
#' @param stardog Stardog object
#' @param q Query to be stored
#' @param queryName Name for the stored query
#' @param reasoning Boolean. If TRUE, reasoning is used in the query.
#' @returns Returns a success message or the status code of the call
#' @export
#' @importFrom httr content_type
#' @importFrom httr DELETE
#' @importFrom httr PUT
#'
add_stored <- function(stardog, q = "select (count(*) as ?n) {?s ?p ?o .}",
                       queryName = "CountEm",
                       reasoning = FALSE) {
  if (check_db(stardog)){
    db <- stardog$database
  } else {
    db <- ""
  }
  query_url <- paste(stardog$endpoint, "admin/queries/stored", sep = "/")
  query_iri <- paste("system:Query", queryName, sep = "")

  if (reasoning) {
    body_ttl <- paste("
      @prefix system: <http://system.stardog.com/> .

      ", query_iri, " a system:StoredQuery, system:SharedQuery , system:ReasoningQuery ;
       system:queryName ", "'", queryName, "'", " ;
       system:queryString ", "'", q, "'", " ;
       system:queryCreator ", "'", stardog$username, "'"," ;
       system:queryDatabase ", "'", db, "'", " .",
                     sep = ""
    )
  } else {
    body_ttl <- paste("
      @prefix system: <http://system.stardog.com/> .

      ", query_iri, " a system:StoredQuery, system:SharedQuery  ;
       system:queryName ", "'", queryName, "'", " ;
       system:queryString ", "'", q, "'", " ;
       system:queryCreator ", "'", stardog$username, "'"," ;
       system:queryDatabase ", "'", db, "'", " .",
                     sep = ""
    )
  }

  r <- POST(query_url,
            authenticate(stardog$username, stardog$password),
            content_type('text/turtle'),
            body = body_ttl,
            encoding = "raw")

  status <- r$status_code
  if (status == 204) {
    print("Query stored successfully")
  } else if (status == 422) {
    print("Query already stored")
  } else {
    print(status)
  }

}

#' Delete a stored query
#'
#' @param stardog Stardog object
#' @param storedQuery Name of the stored query to be removed
#' @param all If True, delete all stored queries.
#' @details
#' Either set all=TRUE, or supply the name of a query.
#'
#' @export
#'
delete_stored <- function(stardog, storedQuery = NULL, all = FALSE) {
  if (length(storedQuery > 0)) {
    query_url <- paste(stardog$endpoint, "admin/queries/stored", storedQuery, sep = "/")
    r <- DELETE(query_url,
                authenticate(stardog$username, stardog$password))
    if (r$status_code == 204) {
      print("stored query deleted")
    } else if (r$status_code == 404 ) {
      print("stored query does not exist")
    } else {
      print(r)
    }
  } else if (all) {
    query_url <- paste(stardog$endpoint, "admin/queries/stored", sep = "/")
    r <- DELETE(query_url,
                authenticate(stardog$username, stardog$password))
    if (r$status_code == 204) {
      print("All queries deleted")
    } else {
      print(r$status_cde)
    }
  } else {
    print("Must enter the name of a stored query or set all = TRUE")
  }

}

#' Update a stored query
#'
#' @param stardog Stardog object
#' @param storedQuery The name of the query to be updated
#' @param q The new query to be stored.
#' @details
#' This function basically replaces the stored query with new code.
#'
#' @export
#'
update_stored <- function(stardog, storedQuery = "CountEm",
                          q = "select (count(distinct(?p)) as ?n) {?s ?p ?o .}"
                          ) {
  if (check_db(stardog)) {
    db <- stardog$database
  } else {
    db <- ""
  }
  query_url <- paste(stardog$endpoint, "admin/queries/stored", sep = "/")
  query_iri <- paste("system:Query", storedQuery, sep = "")
  body_ttl = paste("
  @prefix system: <http://system.stardog.com/> .

  ", query_iri, " a system:StoredQuery, system:SharedQuery , system:ReasoningQuery ;
   system:queryName ", "'", storedQuery, "'", " ;
   system:queryString ", "'", q, "'", " ;
   system:queryCreator ", "'", stardog$username, "'"," ;
   system:queryDatabase ", "'", db, "'", " .",
                   sep = "")

  r <- PUT(query_url,
           authenticate(stardog$username, stardog$password),
           content_type('text/turtle'),
           body = body_ttl,
           encoding = "raw")

  status <- r$status_code
  if (status == 204) {
    print("Query stored successfully")
  } else {
    print(status)
  }
}

#' Rename a stored query
#'
#' @param stardog Stardog object
#' @param oldName current name of stored query
#' @param newName rename the query to newName
#' @returns Success message
#' @export
#' @importFrom httr content_type_json
rename_stored <- function(stardog, oldName = "CountEm", newName = "foo") {
  query_url <- paste(stardog$endpoint, "admin/queries/stored", oldName, sep = "/")
  body_list <- list(name = newName)
  r <- POST(query_url,
            authenticate(stardog$username, stardog$password),
            body = toJSON(body_list),
            content_type_json()
  )
  status <- r$status_code
  if (status == 204) {
    print("Name changed successfully")
  } else if (status == 404) {
    print("Stored query does not exist")
  } else {
    r
  }
}


#' Returns the stored query
#'
#' Returns a string containing the script of the stored query
#'
#' @param stardog Stardog object
#' @param queryName Name of the stored query
#' @param encoding Defaults to UTF-8
#' @returns The content of the stored query.
#' @details
#' This allows you to inspect a stored query and read the sparql.
#'
#' @export
#'
get_stored <- function(stardog, queryName = "CountEm", encoding = 'UTF-8') {
  query_url <- paste(stardog$endpoint, "admin/queries/stored", queryName, sep = "/")
  r <- GET(query_url,
           authenticate(stardog$username, stardog$password),
           accept("text/turtle")
  )
  cat(content(r, encoding = encoding))
}


