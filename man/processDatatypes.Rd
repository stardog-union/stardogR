% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/starmap.R
\name{processDatatypes}
\alias{processDatatypes}
\title{the heavy lifting}
\usage{
processDatatypes(datatypes, data, holding)
}
\arguments{
\item{datatypes}{vector of datatypes}

\item{data}{the dataframe}

\item{holding}{a place to hold interim results and useful meta-data}
}
\value{
a list with the updated buildings, onto and sparql
}
\description{
build all the sparql, bindings and ontologies for the datatype properties
}
