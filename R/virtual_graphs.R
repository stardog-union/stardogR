

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
#' @returns The success code of the operation.
#'
#' @details
#' There are instructions on Azure Databricks for forming jdbc.url. It
#' contains the address of the warehouse as well as an authentication token
#' for the user. Consequently, we don't need to supply username or password.
#'
#'@export
register_databricks <- function(stardog,
                                source_name,
                                jdbc.url= "") {
  post_url <- paste(stardog$endpoint, "/admin/data_sources", sep = "")
  post_body <- list(name = source_name,
                    options = list(
                      jdbc.url = jdbc.url,
                      jdbc.driver = "com.databricks.client.jdbc.Driver",
                      testOnBorrow=TRUE,
                      validationQuery="Select 1"

                    )
      )

  r <- POST(post_url,
            authenticate(stardog$username, stardog$password),
            body = post_body,
            encode = "json")
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
#' @returns success code of the Post
#'
#' @export
add_virtual_graph <- function(stardog, vg_name, source_name,
                              mappings = "") {
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
