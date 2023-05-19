#' Create a stardog class (S3)
#'
#' @description This is the connection object for using the Stardog API. It
#' requires a Stardog endpoint and basic authentiation (username and password).
#' The database can be supplied as an option. There is a function that allows
#' the creation of a database and it will fill in the database slot.
#' An empty transaction slot is created for holding transaction id's.
#'
#' @param endpoint The Stardog endpoint: a URL with the connection port (5820)
#' @param username Username for connecting to Stardog
#' @param password The Password
#' @param encoding Encoding to be used to parse responses. Defaults to UTF-8.
#' @returns The name of the S3 Stardog class. All functions in this package have this object as an argument.
#' @export
#' @examples
#' Stardog("http://localhost:5820", "admin", "admin")
#'

Stardog <- function(endpoint = "http://localhost:5820", username = "admin",
                    password = "admin", encoding = "UTF-8") {
  conn <- list(endpoint = endpoint,
               username = username,
               password = password,
               database = NA,
               encoding = encoding,
               transaction = NA,
               logging = NA)
  class(conn) <- "stardog"
  conn
}

#' A fluff function to check the connection.
#'
#' @param stardog a Stardog object
#' @returns "Woof!" if the connection is active; otherwise returns the HTTP response.
#' @export
#'
goodBoy <- function(stardog) {
  url <- paste(stardog$endpoint, "/admin/alive", sep = "")
  r <- GET(stardog$endpoint, authenticate(stardog$username, stardog$password))
  if (r$status_code == 200) {
    return("Woof!")
  } else {
    return(r$status_code)
  }
}

#' print method for the Stardog object
#'
#' Only prints out the endpoint and the database, if there is one
#' Does not reveal username, password or variables used by stardogR
#' to keep track of things.
#' @param x Stardog object to be printed out
#' @param ... not actually used
#'
#' @export

print.stardog <- function(x, ...) {
  cat(paste("Stardog endpoint:\n\t", x$endpoint, "\n\n", sep = ""))
  if (!is.na(x$database)) {
    cat(paste("Using database:\n\t", x$database, "\n", sep = ""))
  } else {
    cat("No database has been specified. To enable a database, use function use_database. \n")
  }
}
