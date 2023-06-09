% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/database_management_fns.R
\name{use_database}
\alias{use_database}
\title{Add a database name to the Stardog object}
\usage{
use_database(
  stardog,
  dbName = NA,
  create = FALSE,
  reset = FALSE,
  set_to_NA = FALSE
)
}
\arguments{
\item{stardog}{A stardog object}

\item{dbName}{The database name (string)}

\item{create}{Boolean (FALSE).}

\item{reset}{(Boolean) \code{reset = TRUE} rebuild the database, deleting all data from the current version}

\item{set_to_NA}{Boolean. If TRUE, set database to NA, regardless of whether it exists in the endpoint.}
}
\value{
Returns the new Stardog object.
}
\description{
Many stardogR functions operate on a particular database. Supply the name
of that database to the Stardog object.
}
\details{
This function adds the database name to the stardog object. If the database
does not exist, it will be created upon create = TRUE.
}
