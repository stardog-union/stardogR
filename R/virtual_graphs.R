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

#' List data sources
#'
#' @param stardog Stardog object
#' @returns a list of data sources
#' @export
#'
list_data_sources <- function(stardog){
  get_url <- paste(stardog$endpoint, "/admin/data_sources", sep = "")
  r <- GET(get_url, authenticate(stardog$username, stardog$password))
  content(r, encoding = "UTF8")

}

#' Register a data source on databricks
#'
#' Registers the Hive tables from /user/hive/warehouse on Databricks
#' Allows connecting virtual graphs to these tables
#'
#' @param stardog a stardog object
#' @param source_name a name for the data source
#' @param jdbc.url connection url
#' @param unique.key.sets The unique keys. See details
#' @param update boolean. Set to TRUE to update an existing data source
#' @returns The success code of the operation.
#'
#' @details
#' There are instructions on Azure Databricks for forming jdbc.url. It
#' contains the address of the warehouse as well as an authentication token
#' for the user. Consequently, we don't need to supply username or password.
#'
#' Databricks need to know which fields in the tables are "unique keys", that is,
#' which can be used as primary keys for joins. The value is a string, of the form
#' "(table1.key1),(table2.key2),..." to indicate that field key1 of table1 is
#' a unique key. For compound keys, do something of the form
#' (table1.key1, table1.key2) within parentheses.
#'
#'@export
register_databricks <- function(stardog,
                                source_name,
                                jdbc.url= "",
                                unique.key.sets = "",
                                update = FALSE) {

  if (update) {
    post_url <- paste(stardog$endpoint, "admin/data_sources", source_name, sep = "/")
    post_body <- list(name = source_name,
                      options = list(
                        jdbc.url = jdbc.url,
                        jdbc.driver = "com.databricks.client.jdbc.Driver",
                        testOnBorrow=TRUE,
                        validationQuery="Select 1",
                        unique.key.sets = unique.key.sets
                      ),
                      force = TRUE
    )
    r <- PUT(post_url,
             authenticate(stardog$username, stardog$password),
             body = post_body,
             encode = "json")
  } else {
    post_url <- paste(stardog$endpoint, "/admin/data_sources", sep = "")
    post_body <- list(name = source_name,
          options = list(
            jdbc.url = jdbc.url,
            jdbc.driver = "com.databricks.client.jdbc.Driver",
            testOnBorrow=TRUE,
            validationQuery="Select 1",
            unique.key.sets = unique.key.sets
          )
      )

    r <- POST(post_url,
              authenticate(stardog$username, stardog$password),
              body = post_body,
              encode = "json")
    }
  r
}

#' Create a virtual graph
#'
#' Set up a virtual graph against a registered data source. Supply
#' the mappings as a character string.
#' @param stardog Stardog object
#' @param vg_name Name of the virtual graph
#' @param source_name name of the data source
#' @param mappings SMS mappings as a character string
#' @param update boolean, if TRUE then update an existing VG graph
#' @returns success code of the Post
#'
#' @export
add_virtual_graph <- function(stardog, vg_name, source_name,
                              mappings = "", update = FALSE) {
  if (!update) {
  post_url <- paste(stardog$endpoint, "/admin/virtual_graphs", sep = "")
  post_body <- list(name = vg_name,
                    data_source = source_name,
                    db = stardog$database,
                    mappings = mappings,
                    options = list(mappings.syntax = "SMS2")
                    )
  post_json <- toJSON(post_body, auto_unbox = TRUE)
  r <- POST(post_url,
            authenticate(stardog$username, stardog$password),
            body = post_json,
            encode = "raw"
            )
  } else {
    put_url <- paste(stardog$endpoint, "admin/virtual_graphs", vg_name, sep = "/")
    post_body <- list(
                      data_source = source_name,
                      db = stardog$database,
                      mappings = mappings,
                      options = list(mappings.syntax = "SMS2")
    )
    post_json <- toJSON(post_body, auto_unbox = TRUE)
    r <- PUT(put_url,
              authenticate(stardog$username, stardog$password),
              body = post_json,
              encode = "raw"
    )
  }
  r$status_code
}

#' refresh the metadata
#'
#' If the tables of interest in a data source change, we need
#' to refresh the metadata, or Stardog will not be able to find
#' the new tables.
#'
#' @param stardog stardog object
#' @param sourceName name of the data source to be refreshed
#' @returns the status code of the post operation
#'
#' @export
#'
refresh_metadata <- function(stardog, sourceName) {
  post_url <- paste(stardog$endpoint, "admin/data_sources", sourceName, "refresh_metadata", sep = "/")
  #post_body <- list(name = sourceName)
  #post_json <- toJSON(post_body, auto_unbox = TRUE)
  r <- POST(post_url,
            authenticate(stardog$username, stardog$password),
            body = '{}',
            encode = "raw"
            )
  r$status_code
}

#' Delete a registered data source
#'
#' @param stardog stardog object
#' @param sourceName the name of the source
#'
#' @returns 204 if successful, 404 if the source does not exist
#' @export
delete_source <- function(stardog, sourceName) {
  post_url <- paste(stardog$endpoint, "admin/data_sources", sourceName, sep = "/")
  r <- DELETE(post_url,
              authenticate(stardog$username, stardog$password))
  r$status_code

}

#' list virtual graphs
#'
#' List the virtual graphs from the instance. It does not appear to allow you
#' to specify the database.
#'
#' @param stardog stardog object
#' @returns the results
#'
#' @export
#'
#'
list_virtual_graphs <- function(stardog) {
  get_url <- paste(stardog$endpoint, "admin/virtual_graphs", sep = "/")
  r <- GET(get_url,
           authenticate(stardog$username, stardog$password))
  if (r$status_code == 200) {
    output <- unlist(content(r)[[1]])
  } else {
    output <- r$status_code
  }
  output
}

#' delete a virtual graph
#'
#' @param stardog stardog object
#' @param vgName name of the virtual graph to be deleted
#' @returns Status code of the deletion
#' @export
#'
delete_virtual_graph <- function(stardog, vgName) {
  del_url <- paste(stardog$endpoint, "admin/virtual_graphs", vgName, sep = "/")
  r <- DELETE(del_url,
              authenticate(stardog$username, stardog$password))
  r$status_code
}

#' does a virtual graph exist
#'
#' Checks if a particular graph name corresponds to an existing virtual
#' graph.
#'
#' @param stardog stardog object
#' @param vgName name of possibly existing virtual graph
#' @returns Boolean. True if there is a virtual graph with that namedd
#' @export
#'

exists_virtual_graph <- function(stardog, vgName) {
  temp <- list_virtual_graphs(stardog)
  vgName <- paste("virtual://", vgName, sep = "")
  if (vgName %in% temp) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


#' Does a data source exist
#'
#' Checks if there is a data source with the supplied name
#'
#' @param stardog Stardog object
#' @param data_source Name of the potential data source
#' @returns Boolean. True if there is a data source with that name
#' @export
#'
exists_data_source <- function(stardog, data_source) {
  temp <- unlist(list_data_sources(stardog))
  if (data_source %in% temp) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#'
