% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/virtual_graphs.R
\name{delete_source}
\alias{delete_source}
\title{Delete a registered data source}
\usage{
delete_source(stardog, sourceName)
}
\arguments{
\item{stardog}{stardog object}

\item{sourceName}{the name of the source}
}
\value{
204 if successful, 404 if the source does not exist
}
\description{
Delete a registered data source
}
