% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{check_database}
\alias{check_database}
\title{Checks if a database exists}
\usage{
check_database(stardog, dbName)
}
\arguments{
\item{stardog}{A stardog object}

\item{dbName}{Name of the database being checked (string)}
}
\value{
Boolean
}
\description{
Checks if a database exists
}
\details{
This function checks if the database in the stardog object actually
exists on the Stardog incidence. Compare to check_db(), which only
checks if the name of the database is stored in the Stardog object.
}
