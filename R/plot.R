# Plot the ontology or the meta-data

#' plot the ontology based on a named graph
#'
#' Plots the ontology, based solely on classes, or classes and
#' datatypes. Requires the ontology to be in a named graph.
#' TODO: allow used of the default graph.
#' 
#'
#' @param stardog Stardog object
#' @param onto_graph URI of the named graph
#' @param datatypes Boolean: if TRUE, include datatype properties.
#' @param plot Boolean: plot the graph on TRUE
#' @param datatypeWt Large values will pull the datatype properties closer to their parent node.
#' @param classColour Colour for the class nodes
#' @param dataColour Colour of the datatype nodes
#' @returns the igraph object if PLOT=FALSE. Otherwise, plots the schema
#'
#' @export
#'


plotSchema <- function(stardog, onto_graph, datatypes = FALSE, plot = TRUE,
                       datatypeWt = 3,
                       classColour = "skyblue",
                       dataColour = "lightgreen") {
  # nodeGraph <- make_empty_graph()
  # add the class nodes
  q <- paste('select ?node ?n {graph ', onto_graph, ' {
      ?n a owl:Class ;
        rdfs:label ?node .
    }
  }
  ', sep = "")
  temp <- query(stardog, q)
  dfClass <- data.frame(node = temp$node, color = rep(classColour, nrow(temp)))

  # Add the object properties
  q <- paste('select ?node1 ?node2 {graph ', onto_graph, ' {
      ?n1 a owl:Class ;
        rdfs:label ?node1 .
      ?n2 a owl:Class ;
        rdfs:label ?node2 .
      ?edge a owl:ObjectProperty ;
        rdfs:domain ?n1 ;
        rdfs:range ?n2
    }
  }
  ', sep = "")
  dfEdges <- query(stardog, q)
  if (nrow(dfEdges) == 0) stop(simpleError("You need at least one edge for a graph"))
  names(dfEdges) <- c("from", "to")
  dfEdges$color <- "grey"
  dfEdges$wt <- 1
  if (!datatypes) {
    nodeGraph <- igraph::graph_from_data_frame(dfEdges, directed = TRUE, vertices = dfClass)
  } else {

    q <- paste('select ?node ?value {graph ', onto_graph, ' {
      ?n a owl:Class ;
      rdfs:label ?node .
      ?v a owl:DatatypeProperty ;
        rdfs:label ?value ;
        rdfs:domain ?n .
        }
    }
  ', sep = "")
    temp <- query(stardog, q)
    tempNodes <- data.frame(node = temp$value, color = rep(dataColour, nrow(temp))) # Getting the datatype nodes
    dfClass <- rbind(dfClass, tempNodes)
    names(temp) <- c("from", "to")
    temp$color <- "bisque"
    temp$wt <- datatypeWt
    dfEdges <- rbind(dfEdges, temp) # Adding to the edge list
    nodeGraph <- igraph::graph_from_data_frame(dfEdges, directed = TRUE, vertices = dfClass)
    }

  if (plot) {
    plot(nodeGraph, layout = igraph::layout_with_fr(nodeGraph, weights = dfEdges$wt))
  } else {
    nodeGraph
  }
}
