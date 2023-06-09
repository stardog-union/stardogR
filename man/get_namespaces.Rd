% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_db_metadata_fns.R
\name{get_namespaces}
\alias{get_namespaces}
\title{list namespaces used by the database}
\usage{
get_namespaces(stardog, raw = FALSE)
}
\arguments{
\item{stardog}{Stardog connection object}

\item{raw}{Boolean}
}
\value{
a list of namespaces
}
\description{
list namespaces used by the database
}
\details{
When raw is FALSE, the namespaces are returns with prefixes used instead of full urn's
}
