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

# Functions about options and namespaces

#' List database options
#' @export
#' @param stardog stardog connection object
#' @returns Returns the options associated with the stardog database.
get_options <- function(stardog) {
  check_db(stardog)
  opt_url <- paste(stardog$endpoint, "admin/databases", stardog$database, "options", sep = "/")
  r <- GET(opt_url, authenticate(stardog$username, stardog$password))
  content(r)
}

#' Get a named option
#'
#' @export
#' @param stardog Stardog connection object
#' @param option The name of the option. Defaults to search.version
#'
#' @returns Returns the value of the named option.
get_option <- function(stardog, option = "search.version") {
  # not suitable for namespaces
  # use get_namespaces instead
  opt_url <- paste(stardog$endpoint, "admin/databases", stardog$database, "options", sep = "/")
  r <- GET(opt_url, authenticate(stardog$username, stardog$password))
  content(r)[option]
}

#' Internal function to replace urn's with prefix's
#'
#' @param x Content to be fixed
#' @returns A data frame with the prefix and URN's of the namespaces
#' @details
#' An internal utility function to massage the output from the API and
#' return a nice dataframe of prefixes and URN's.
#'
fix_namespace <- function(x) {
  temp <- unlist(x, recursive = FALSE)
  out_prefix <- unlist(temp[names(temp) == "prefix"])
  out_name <- unlist(temp[names(temp) == "name"])
  as.data.frame(list(prefix = out_prefix, name = out_name))
}

#' list namespaces used by the database
#' @param stardog Stardog connection object
#' @param raw Boolean
#' @details
#' When raw is FALSE, the namespaces are returns with prefixes used instead of full urn's
#'
#' @export
#' @returns a list of namespaces
#'
get_namespaces <- function(stardog, raw = FALSE) {
  if (length(stardog$database) == 0) {
    cat("Missing database")
    return()
  }
  get_url <- paste(stardog$endpoint, stardog$database, "namespaces", sep = "/")
  r <- GET(get_url, authenticate(stardog$username, stardog$password) )
  if (raw) {
    return(content(r))
  } else {
    output <- content(r)$namespaces
    return(fix_namespace(output))
  }
}

#' Add a namespace to the database
#'
#' @param stardog stardog connection object
#' @param uri Full uri of the namespace
#' @param prefix The prefix for that uri
#' @details
#' Checks to see if the namespace already exists.
#' @returns Does nothing if the namespace already exists. If not, adds the namespace/prefix pair to the existing namespace
#' @export
#'
add_namespace <- function(stardog, uri = "http://iris.com/", prefix = "iris") {
  if (length(stardog$database) == 0) {
    cat("Missing database")
    return()
  }
  opt <- get_options(stardog)
  ns <- opt$database.namespaces
  # Check if namespace is already there
  check <- grep(prefix, unlist(ns))
  if (length(check) > 0) {
    cat("This namespace already exists\n")
    return()
  }
  newBody <- NULL
  newBody$database.namespaces <- unlist(append(ns, paste(prefix, uri, sep = "=")))
  post_url <- paste(stardog$endpoint, "admin/databases", stardog$database, "options", sep = "/")
  r <- POST(post_url, authenticate(stardog$username, stardog$password),
            body = toJSON(newBody)
  )

  r

}

#' Removes a namespace
#'
#' Removes a namespace/prefix pair upon supplying a prefix
#' @param stardog Stardog connection object
#' @param prefix The prefix of the namespace
#' @returns Returns the return value of the post operation
#' @export
remove_namespace <- function(stardog, prefix) {
  if (length(stardog$database) == 0) {
    cat("Missing database")
    return()
  }
  opt <- content(get_options(stardog))
  ns <- opt$database.namespaces
  # Check if namespace is already there
  check <- grep(prefix, unlist(ns))
  if (length(check) == 0) {
    cat("Namespace not found\n")
    return()
  } else {
    newBody <- NULL
    newBody$database.namespaces <- unlist(ns[-check])
    post_url <- paste(stardog$endpoint, "admin/databases", stardog$database, "options", sep = "/")
    r <- POST(post_url, authenticate(stardog$username, stardog$password),
              body = toJSON(newBody)
    )

    r
  }
}

