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

## Managing models

#' Generate a model from the graph data
#'
#' Not really sure what this does and how it is relevant
#'
#' @param stardog Stardog object
#' @param reasoning Boolean
#' @param output output format. Choose from the list.
#' @param encoding Supply encoding for the response. Defaults to UTF-8
#' @returns Status code

generate_model <- function(stardog, reasoning = FALSE,
                           output=c('text', 'owl', 'sql', 'graphql', 'shacl'),
                           encoding = 'UTF-8'
) {
  query_url <- paste(stardog$endpoint, stardog$database, "model", sep = "/")
  if (!(output %in% c('text', 'owl', 'sql', 'graphql', 'shacl'))) {
    message("Output format must be one of text, owl, sql, graphql or shacl")
    return()
  }
  r <- GET(query_url, authenticate(stardog$username, stardog$password),
           query = list(reasoning = reasoning, output = output)
  )
  if (r$status_code == 200) {
    return(content(r, encoding = encoding))
  } else if (r$status_code == 404) {
    message(content(r)$message)
    return()
  } else {
    return(r$status_code)
  }
}

#' Get the models used by the database
#'
#' @param stardog Stardog object
#' @returns Vector of model names associated with this database.
#'
#' @export
get_model_list <- function(stardog) {
  # not suitable for namespaces
  # use get_namespaces instead
  opt_url <- paste(stardog$endpoint, "admin/databases", stardog$database, "options", sep = "/")
  r <- GET(opt_url, authenticate(stardog$username, stardog$password))
  unlist(content(r)[['reasoning.schemas']])
}

#' Internal function to prepare a list of models
#'
#' Prepare a list of model names with associated graphs to be sent as
#' part of the body in a request for setting or adding to the list of models
#'
#' @param model_list Named list
#' @param old_models Named list
#' @returns list with one element called `reasoning.names`. Its contents describe one or more models. Each model is named by
#' the name it will have in the stardog database. The contents of the model are a vector of graph uri's. These graphs defined
#' the model.
build_model_list <- function(model_list, old_models = NULL){
  tempNames <- names(model_list)
  if (length(old_models) > 0) {
    output <- old_models
  } else {
    output <- c()
  }
  for (name in tempNames) {
    temp <- paste(name, model_list[[name]], sep = "=")
    output = c(output, temp)
  }
  list(reasoning.schemas = output)
}

#' Add models to the database
#'
#' This function deletes all previous models and replaces them by the models
#' in the new list.
#' @param stardog Stardog object
#' @param model_list Named list containing the model information.
#' @returns Success message from the response.
#' @importFrom jsonlite toJSON
#'
#' @details
#' Each model is defined by one or more graphs that contain reasoning ontologies.
#' The names of the model list are passed to Stardog as the names of the reasoning schemas.
#' The graphs URI's must be given in full format without prefixes.
#'
#' @examples
#' \dontrun{
#' set_models(stardog, list(model1=c("http://example.com/graph1a", "http://example.com/graph1b"),
#'   model2="http://example.com/graph2"))
#'   }
#'
#'
#' @export
set_models <- function(stardog, model_list){
  query_url <- paste(stardog$endpoint, "admin/databases", stardog$database, "options", sep = "/")
  body_list <- build_model_list(model_list)
  r <- POST(query_url,
            authenticate(stardog$username, stardog$password),
            content_type_json(),
            body = toJSON(body_list)
  )
  return(content(r, encoding = stardog$encoding)$message)
}

#' @describeIn set_models Clears all models from the database
#'
#'
#'
#' @export
#'
clear_models <- function(stardog){
  query_url <- paste(stardog$endpoint, "admin/databases", stardog$database, "options", sep = "/")
  body_list <- list(reasoning.schemas = c(""))
  r <- POST(query_url,
            authenticate(stardog$username, stardog$password),
            content_type_json(),
            body = toJSON(body_list)
  )
  content(r, encoding = stardog$encoding)$message
}

#' @describeIn set_models Remove listed models from the database
#'
#'
#' @export

remove_model <- function(stardog, model_list) {
  query_url <- paste(stardog$endpoint, "admin/databases", stardog$database, "options", sep = "/")
  old_models <- get_model_list(stardog)
  remove_models <- unlist(build_model_list(model_list))
  new_models <- old_models[!(old_models %in% remove_models)]
  body_list <- list(reasoning.schemas = new_models)
  r <- POST(query_url,
            authenticate(stardog$username, stardog$password),
            content_type_json(),
            body = toJSON(body_list)
  )
  content(r, encoding = stardog$encoding)$message
}

#' @describeIn set_models add new models to database without removing existing models
#' @export
add_models <- function(stardog, model_list){
  query_url <- paste(stardog$endpoint, "admin/databases", stardog$database, "options", sep = "/")
  old_models <- get_model_list(stardog)
  body_list <- build_model_list(model_list, old_models)
  r <- POST(query_url,
            authenticate(stardog$username, stardog$password),
            content_type_json(),
            body = toJSON(body_list)
  )
  content(r, encoding = stardog$encoding)$message
}

