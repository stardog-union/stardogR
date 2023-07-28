#' List the databases available on the Stardog instance
#'
#' @param stardog Stardog object
#' @returns Returns a vector of database names
#' @export
#' @importFrom httr accept
list_databases <- function(stardog) {
  query_url <- paste(stardog$endpoint, "admin/databases", sep = "/")
  r <- GET(query_url, authenticate(stardog$username, stardog$password), accept("application/json"))
  unlist(content(r)[[1]])
}



#' Creates a new database
#'
#' Checks to see if the named database already exists. If it exists already.
#' If it does not, then creates a new database. See replace_database to replace
#' an existing database.
#'
#' @param stardog Stardog object
#' @param dbName Name of the new database (string)
#'
#'
#' @returns The stardog object with dbName added to the database slot.
#' @export
new_database <- function(stardog, dbName) {
  if (check_database(stardog, dbName)) {
    message("This database already exists. Run reset_database if you want to replace it.")
  } else {
    post_url <- paste(stardog$endpoint, "admin/databases", sep = "/")
    root_string <- paste('{"dbname": "', dbName, '", "options": {}, "files": []}', sep = "")
    body <- list(root = root_string)
    r <- POST(post_url, authenticate(stardog$username, stardog$password),
              body = body,
              encode = "multipart"
    )
    if (r$status_code == 201) {
      stardog$database <- dbName
    } else {
      message("Creation of database failed with error code ", r$status_code)
    }
  }
  return(stardog)
}

#' Drops the named database
#'
#'@param stardog A stardog object
#'@param silent (boolean) If TRUE, suppress output
#' @returns Stardog object with NA in database slot.
#'
#' @description
#' Drops the database contained in the stardog object. If silent = FALSE, gives messages of
#' failure or success.
#'
#'
#'@export
drop_database <- function(stardog, silent = FALSE) {
    post_url <- paste(stardog$endpoint, "admin/databases", stardog$database, sep = "/")
    if (is.na(stardog$database)) {
      message("Stardog object does not contain the name of a database to delete")
      return(stardog)
    }
    body <- list(db = stardog$database)
    r <- DELETE(post_url, authenticate(stardog$username, stardog$password),
                body = body
    )
    if (r$status_code == 200)  {
      stardog$database <- NA
    }
    if (!silent) {
      if (r$status_code == 20) {
          message(stardog$database, " deleted successfully")
      } else if (r$status_code == 400) {
          message(stardog$database, " does not exist")
      } else {
          message(r$status_code)
        }
      }
    stardog
}

#'Replace a database
#'
#'Drops a database and creates a fresh one with the same name. All data
#' will be deleted.
#'
#'@param stardog A stardog object, including the name of the database to be reset
#'
#'@returns Returns the stardog object, having recreated the database
#'
#'@export

reset_database <- function(stardog) {
  db <- stardog$database
  # Upon missing database
  if (length(db) == 0) {
    message("The Stardog object does not have a database.")
    return(stardog)
  }
  # upon failure to drop
  stardog <- drop_database(stardog, silent = TRUE)
  if (!is.na(stardog$database)) {
    message("Unable to drop database ", db)
    return(stardog)
  }
  # Go ahead and recreate the database
  stardog <- new_database(stardog, db)
  return(stardog)

}

#' Add a database name to the Stardog object
#'
#' Many stardogR functions operate on a particular database. Supply the name
#' of that database to the Stardog object.
#'
#' @param stardog A stardog object
#' @param dbName The database name (string)
#' @param reset (Boolean) \code{reset = TRUE} rebuild the database, deleting all data from the current version
#' @param create Boolean (FALSE).
#' @param set_to_NA Boolean. If TRUE, set database to NA, regardless of whether it exists in the endpoint.
#' @returns Returns the new Stardog object.
#'
#' @details
#' This function adds the database name to the stardog object. If the database
#' does not exist, it will be created upon create = TRUE.
#'
#'
#'@export
#'
use_database <- function(stardog, dbName = NA, create = FALSE, reset = FALSE, set_to_NA = FALSE) {
  if (set_to_NA) {
    stardog$database <- NA
  } else {
    if (length(dbName) == 0) {
      message("Must enter a database name")
    } else {
      check <- check_database(stardog, dbName)
      if (check) {
        stardog$database <- dbName
        if (reset) {
          stardog <- reset_database(stardog)
        }
        if (create) {
          message("This database already exists. To reset, use reset=TRUE")
        }
      } else if (create) {
        stardog <- new_database(stardog, dbName)
      } else {
        message("Database dbName does not exist, and I have no instructions on what to do next.")
      }
    }
  }
  return(stardog)
}
