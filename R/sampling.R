#' obtain a "random" sample from the graph
#'
#' Supply a simple pattern and pull out a random sample.
#' The entities thus selected may be saved to a named graph
#' With logging, the sample is connected to a sample node.
#'
#' @param stardog a stardog object
#' @param triple a RDF triple pattern that forms the basis of the sample
#' @param size The number of triples in the random sample
#' @param graph the named graph where the sample is saved.
#' @param ... The usual query parameters. For example, reasoning can be used.
#' @returns a data frame of the sampled triples.
#' @details
#' The sample is only saved if graph is present. Logging adds some additional structure
#' to the graph where the sample is saved.
#'
#' This function invokes the Stardog sampling service. Read the Stardog documentation to learn more about the
#' probabilistic properties of this service. Note that sampling cannot be done on complex queries, but only
#' against simple triples. Pattern \code{?s a owl:someThing} would draw a sample of nodes of class someThing.
#'
#' @export
sampleGraph <- function(stardog, triple, size = 100, graph = NA, ...) {
  # if (logging && is.na(graph)) stop(simpleError("Must supply a graph if logging is turned on"))
  tokens <- unlist(strsplit(triple, split = " "))
  temp <- sapply(tokens, function(x) substring(x, 1, 1) == '?')
  vars <- paste(tokens[temp], collapse = " ")
  service <- 'prefix smp: <tag:stardog:api:sample:>'
  qs <- paste(service, '\n\n', 'select ', vars, ' where {\n',
             '\tservice <tag:stardog:api:sample> {\n',
             '\t\t', triple, ' .\n\t\t',
               '[] smp:size ', size, '\n\t\t}\n\t}', sep = "")
  df <- query(stardog, qs, graph = graph, ...)
  df

}
