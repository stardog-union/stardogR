
#' Create a SMS mapping and a simple ontology
#'
#' The terms of the formula must be column names of the data.frame data
#' The mappings and ontology are output as a list of two strings.
#'
#' @param formulae A vector of strings. Each string is a simple starmap formula
#' @param data The data frame to be mapped
#' @param prefix A sparql prefix
#' @param urn The prefix to each iri node written in full
#'
#' @returns A list containing the ontology and the SMS mapping as strings
#' @details
#' The formulae are based on the R formula language.
#' \code{x ~ y} maps x and y as iri nodes. there is an object property from x to y
#' \code{x | v1 + v2} maps x as an iri node, with datatype properties v1 and v2.
#' The types of v1 and v2 are deduced from their datatypes in the dataframe.
#' \code{x | v1:v2} maps all columns from v1 through v2
#' \code{x | 1:4} maps columns 1 through 4.
#' Formula fragments can be combined, as in \code{x | v1 + v2 ~ y | w1 + w2 }
#'
#' @export
#'
starmap <- function(formulae, data, prefix = "", urn = "http://stardog.com/") {
  prefix_line <- paste("prefix ", prefix,  ": <" , urn, ">  \n\n", sep = "" )
  prefix_line_onto <- paste("@prefix ", prefix,  ": <" , urn, "> . \n\n", sep = "" )
  sparql <- ""
  bindings <- ""
  nodeList <- c() # keep track of distinct iri nodes to avoid duplication in the mapping

  onto <- paste("@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .",
                "@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> ." ,
                "@prefix xsd: <http://www.w3.org/2001/XMLSchema#> ." ,
                "@prefix owl: <http://www.w3.org/2002/07/owl#> ." ,
                "@prefix stardog: <tag:stardog:api:> . ",
                "@prefix so: <https://schema.org/> . " ,
                prefix_line_onto,
                sep = "\n")

  sms <- paste("prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> ",
               "prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> " ,
               "prefix xsd: <http://www.w3.org/2001/XMLSchema#> " ,
               "prefix owl: <http://www.w3.org/2002/07/owl#> " ,
               "prefix stardog: <tag:stardog:api:>  ",
               prefix_line,
               sep = "\n")
  theToBit <- paste("MAPPING", "FROM CSV {", "}", "TO {\n", sep = "\n")
  theWhereBit <- "\nWHERE {\n"

  holding <- list(sparql = sparql, bindings = bindings, onto = onto, nodeList = nodeList,
                  prefix = prefix, urn = urn, node_iri = NA, node_class = NA)

  for (formula in formulae) {
    holding <- buildText(formula, data, holding)
  }

  sms <- paste(sms, theToBit, holding$sparql, "} ", theWhereBit, holding$bindings, "}", sep = "")
  onto <- holding$onto
  mapping <- list(sms = sms, onto = onto)
  mapping
}

#' build the sparql and onto text for one formula
#'
#' @param formula single formula (string)
#' @param data The dataframe to be parsed
#' @param holding A list of text fragments thus far
#' @returns An updated version of the holding list
#'
buildText <- function(formula, data, holding) {
  dfNames <- names(data)
  urn <- holding$urn
  prefix <- holding$prefix

  terms <- processFormula(formula, dfNames)
  lhs <- buildTextSide(terms$lhs, data, holding)
  if (!is.na(terms$rhs$lhs)) {
     rhs <- buildTextSide(terms$rhs, data, lhs)
     # Now set an object property between the lhs and the rhs. Add the ontology. No binding necessary for this piece.
     objectProperty <- paste(lhs$node_iri, " ", prefix, ":has", rhs$node_class, " ", rhs$node_iri, " .\n", sep = ""  )
     objectOntology <- paste("<" , urn , "has", rhs$node_class , "> " , "a owl:ObjectProperty ; \n " ,
                                   "\t" , " rdfs:label " , "'" , "has", rhs$node_class, "' " ,  "; \n " ,
                                   "\t" , " so:domainIncludes ", "<" , urn , lhs$node_class , "> ; \n" ,
                                   "\t" , " so:rangeIncludes " , "<" , urn, rhs$node_class , "> . \n ", sep = "")
     holding <- rhs
     holding$sparql <- paste(holding$sparql, objectProperty, sep = "")
     holding$onto <- paste(holding$onto, objectOntology, sep = "")
  } else {
    # No right hand side on this formula, so return the holdings of the lhs.
    holding <- lhs
  }

  holding
}

#' build the sparql and onto text for a side of a formula
#'
#' @param data the dataframe to be parsed
#' @param side the formula fragment (side) being parsed here
#' @param holding A list containing all the interim calculations
#' @returns a list of text to be added to the sms and onto files and the nodeList
#' @details
#' updates the nodeList with new nodes. This allows the same field to appear in different
#' formulae without getting extra BIND statements in the where clause, or extra definitions
#' in the ontology file. Updates the text for the sparql clause, the bindings and the ontology
#' All of this is held in the holding list.
#'
#' The side parameter is a list of two elements: lhs and rhs. lhs is always a single token. rhs could be a single token
#' or a vector of tokens.
#'
#' This function does not build on mapping fragments already constructed. It builds its own contribution, which are combined
#' by the calling functions to build the complete mapping and ontology files for all of formulae being parsed.
#'
buildTextSide <- function(side, data,  holding) {
  node = side$lhs
  prefix <- holding$prefix
  urn <- holding$urn
  nodeList <- holding$nodeList

  # check if we are dealing with a field from the data or a row number variable.
  if (substring(node, 1, 1) == '#') {
    node <- substring(node, 2) # strip the hashtag
    node_binding = '_ROW_NUMBER_'
    side$lhs <- node # the hashtag is gone
  } else {
    node_binding <- node
  }
  node_iri <- paste("?", node, "_iri", sep = "")
  node_class = snakePascal(node)

  if (!(node %in% nodeList)) {
    holding$nodeList <- c(holding$nodeList, node)
    holding$sparql <- paste(holding$sparql, node_iri , ' a ',  prefix, ':' , node_class , ' ; \n',
                             '\t', 'rdfs:label ?', node, ' . \n', sep = "" )
    holding$bindings <- paste(holding$bindings, 'BIND(TEMPLATE("', urn, node, '_{', node_binding, '}") as ', node_iri, ')',
                              '\n', sep = "")
    holding$onto <- paste(holding$onto,
                           '<', urn, node_class, '>', ' ', 'a owl:Class ; \n',
                           '\t', 'rdfs:label', ' ', "'", node_class, "'", ' . \n', sep = "")
  }
  holding$node_iri <- node_iri
  holding$node_class <- node_class
  # now deal with the datatype variables, if there are any.
  datatypes <- side$rhs
  if (!any(is.na(datatypes))) {
    output <- processDatatypes(datatypes, data, holding)
    holding$sparql <- paste(holding$sparql, output$sparql, sep = "")
    holding$bindings <- paste(holding$bindings, output$bindings, sep = "")
    holding$onto <- paste(holding$onto, output$onto, sep = "")
  } else {
    holding$sparql <- paste(holding$sparql, "\n", sep = "")
  }

  holding
}

#' the heavy lifting
#'
#' build all the sparql, bindings and ontologies for the datatype properties
#'
#' @param datatypes vector of datatypes
#' @param data the dataframe
#' @param holding a place to hold interim results and useful meta-data
#' @returns a list with the updated buildings, onto and sparql
#'
processDatatypes <- function(datatypes, data, holding) {
  node_iri <- holding$node_iri
  node_class <- holding$node_class
  prefix <- holding$prefix
  urn <- holding$urn

  sparql <- paste(node_iri, "\n")
  onto <- ""
  bindings <- ""
  for (dt in datatypes) {
    dataTransform <- getDataType(dt, data)
    datatypeName = paste("?", dt, "_tr", sep = "")
    propertyName <- snakeCamel(dt)

    sparql <- paste(sparql, "\t", prefix, ":", propertyName, " ", datatypeName, " ;\n", sep = "")
    bindings <- paste(bindings, "BIND(", dataTransform, "(?", dt, ") as ", datatypeName, ") \n", sep = "")
    onto <- paste(onto, "<" , urn , propertyName , "> " , "a owl:DatatypeProperty ; \n",
                  "\t", "rdfs:label " , "'" , propertyName , "' " ,  "; \n",
                  "\t", "so:domainIncludes ", "<" , urn , node_class , "> ; \n",
                  "\t", "so:rangeIncludes " , dataTransform , " .\n", sep = "")

  }
  sparql <- paste(sparql, " .\n", sep = "")
  output <- list(sparql = sparql, bindings = bindings, onto = onto)

}

#' process a formula
#'
#' Processes an individual formula and convert to a list of lists
#' @param f a formula string
#' @param dfNames Names of the fields in the dataframe to be mapped
#' @returns formula parsed as a list of lists.
#'
processFormula <- function(f, dfNames) {
  output <- list()
  partials <- splitFormula(f, operator = "~")
  output$lhs <- processPartial(partials$lhs, dfNames)
  if (!is.na(partials$rhs)) {
    output$rhs <- processPartial(partials$rhs, dfNames)
  } else {
    output$rhs <- list(lhs = NA, rhs = NA)
  }
  output
}

#' Process the left or right side of the formula.
#'
#' @param snippet string containing the right or left side of the formula
#' @param dfNames The data frame being mapped
#' @returns A list containing the left and right sides of the snippet.
#'
#' @details
#' We're looking at snippets such as "x | y" or "x | y + z + 3:5"
#'
#'
processPartial <- function(snippet, dfNames) {
  output <- list()
  terms <- splitFormula(snippet, operator = "|")
  # process the left hand side, which should be a field name or a $varName.
  check <- checkField(terms$lhs, dfNames)
  if (check) {
    output$lhs <- terms$lhs
  } else {
    stop(simpleError("Error parsing ", terms$lhs))
  }
  if (is.na(terms$rhs)) {
    output$rhs <- NA
  } else {
    output$rhs <- processPlus(terms$rhs, dfNames)
  }

  output
}

#' Capitalize the first letter of a string, make the rest lower case
#'
#' Used to convert variable names to camel case or pascal case.
#'
#' @param var a string
#' @returns The string in title case
#'
capitalize <- function(var) {
  if (is.na(var)) {
    stop(simpleError("Trying to parse a missing value where a variable name is expected"))
  } else if (nchar(var) == 0) {
    stop(simpleError("Trying to title case a null string"))
  } else if (nchar(var) == 1) {
    output <- toupper(var)
  } else {
    output <- paste(toupper(substring(var, 1, 1)), tolower(substring(var, 2)), sep = "")
  }
  output
}

#' Make the first letter of the string lower case
#'
#' @param var a string
#' @returns the string with the first character in lower case, leaving the rest as is
#'
lowerCaseFirst <- function(var) {
  if (is.na(var)) {
    stop(simpleError("Trying to parse a missing value where a variable name is expected"))
  } else if (nchar(var) == 0) {
    stop(simpleError("Trying to title case a null string"))
  } else if (nchar(var) == 1) {
    output <- tolower(var)
  } else {
    output <- paste(tolower(substring(var, 1, 1)), substring(var, 2), sep = "")
  }
  output
}

#' Make the first letter of the string upper case
#'
#' @param var a string
#' @returns the string with the first character in upper case, leaving the rest as is
#'
upperCaseFirst <- function(var) {
  if (is.na(var)) {
    stop(simpleError("Trying to parse a missing value where a variable name is expected"))
  } else if (nchar(var) == 0) {
    stop(simpleError("Trying to title case a null string"))
  } else if (nchar(var) == 1) {
    output <- toupper(var)
  } else {
    output <- paste(toupper(substring(var, 1, 1)), substring(var, 2), sep = "")
  }
  output
}
#' Take a field name and convert to snake case
#'
#' @param var the string to convert to came case
#' @returns the string in camel case
#'
snakeCamel <- function(var) {
  var <- unlist(strsplit(var, "_|\\.", fixed = FALSE))
  if (length(var) == 1) {
    output <- trimws(var)
    output <- lowerCaseFirst(var)
  } else {
    output <- ""
    for (v in var) {
      v <- trimws(v)
      output <- paste(output, capitalize(v), sep = "")
    }
    output <- lowerCaseFirst(output)
  }
  output
}

#' Take a field name and convert to snake case
#'
#' @param var the string to convert to pascal case
#' @returns the string in pascal case
#'
snakePascal <- function(var) {
  var <- unlist(strsplit(var, "_|\\.", fixed = FALSE))
  if (length(var) == 1) {
    output <- trimws(var)
    output <- upperCaseFirst(var)
  } else {
    output <- ""
    for (v in var) {
      v <- trimws(v)
      output <- paste(output, capitalize(v), sep = "")
    }
  }
  output
}


#' Split a formula on ~ or |
#'
#' @param f formula
#' @param operator Either | or ~, the operators of the formula
#'
#' @returns list containing lhs and rhs, the terms of the formula
splitFormula <- function(f, operator = c("|", "~")) {
  terms <- trimws(unlist(strsplit(f, split = operator, fixed = TRUE)))
  if (length(terms) == 1) {
    lhs <- terms
    rhs <- NA
  } else if (length(terms) == 2) {
    lhs <- terms[1]
    rhs <- terms[2]
  } else {
    stop(simpleError(paste("A formula can only have one", operator, "operator.", sep = " ")))
  }
  list(lhs = lhs, rhs = rhs)
}

getDataType <- function(var, data) {
  types <- lapply(data, class)
  if ("numeric" %in% types[[var]]) {
    output <- "xsd:numeric"
  } else if ("character" %in% types[[var]]) {
    output <- "xsd:string"
  } else if ("integer" %in% types[[var]]) {
    output <- "xsd:integer"
  } else if ("factor" %in% types[[var]]) {
    output <- "xsd:string"
  } else if ("POSIXct" %in% types[[var]]) {
    output <- "xsd:dateTime"
  } else if ("Date" %in% types[[var]]) {
    output <- "xsd:date"
  } else if ("logical" %in% types[[var]]) {
    output <- "xsd:boolean"
  } else {
    stop(simpleError(paste("I can't figure out the dataype of", var, sep = " ")))
  }
}

#' Test if a string is a field name of the database
#'
#' Simple function to save some typing
#' @param field The test string
#' @param dfNames The field names of the database being mapped
#' @returns Boolean. TRUE if the field is a name of the database
#' @details
#' Legitimate fields are either names of the dataframe, or \code{#var}, where
#' we want a rownumber to be added to the mapping with name var.
#'
#'
checkField <- function(field, dfNames) {
  if (field %in% dfNames) {
    output <- TRUE
  } else if (substring(field, 1, 1) == "#") {
    output <- TRUE
  } else {
    output <- FALSE
  }
  output
}

#' Return the token
#'
#' If the token is a single field, that field is returned. If the token is
#' a range of integers or strings, it returns a list of field names within
#' that range
#' It checks if the range is legal and if the field names exist in the
#' database.
#'
#' \code{1:4} returns fields 1 through 4 inclusive.
#' \code{var2} returns field var2
#' \code{var2:var4} returns fields var2, var3, var4.
#' \code{0:4} gives a warning and indexes from 1
#' If the upper bound of the range exceeds the number of fields, the function
#' issues a warning and takes everything from the lower limit on up. This is a compromise with
#' current R practice for slicing, which does not throw an out of bounds error, but slices to the
#' end of the vector and then fills in the extra space with NA.
#'
#' @param token The token (String)
#' @param dfNames The names of the data frame being mapped
#' @returns A vector of field names corresponding to the token
#'
#'
#'
getTokenRange <- function(token, dfNames) {
  n <- length(dfNames)
  check <- trimws(unlist(strsplit(token, split = ":", fixed = TRUE)))
  if (length(check) == 1) {
    if (checkField(check, dfNames)) {
      output <- check
    } else {
      stop(simpleError("Term is not a field in the dataframe or #varName."))
    }
  } else if (length(check) > 2) {
    stop(simpleError("Tokens of the form foo:bar:baz are not legal."))
  } else {
    if (checkField(check[1], dfNames)) {
      lower <- which(check[1] == dfNames)
    } else {
      lower <- as.numeric(check[1])
      if (is.na(lower)) {
        stop(simpleError("Range token must be a field name or an integer"))
      }
    }
    if (checkField(check[2], dfNames)) {
      upper <- which(check[2] == dfNames)
    } else {
      upper <- as.numeric(check[2])
      if (is.na(upper)) {
        stop(simpleError("Range token must be a field name or an integer"))
      }
    }
    if (upper > n) {
      warning("The range is out of bounds. Changing upper bound to", n)
      upper <- n
    }
    if (lower < 1) {
      warning("The range is out of bounds. Changing lower bound to 1")
      lower <- 1
    }
    if (upper < lower) stop(simpleError("I can't figure out this range."))
    output <- dfNames[lower:upper]
  }
  output
}

#' Process a formula with + in it
#'
#' Process formula sippets like \code{var1 + var2 + 4:6}
#' Calls getTokenRange for each term.
#'
#' @param snippet The formula snippet
#' @param dfNames The names of the data frame being mapped
#' @returns A list of field names from the dataframe
#'
processPlus <- function(snippet, dfNames) {
  tokens <- trimws(unlist(strsplit(snippet, split = "+", fixed = TRUE)))
  output <- c()
  for (token in tokens) {
    output <- c(output, getTokenRange(token, dfNames))
  }
  output

}


