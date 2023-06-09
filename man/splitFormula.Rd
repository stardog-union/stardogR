% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/starmap.R
\name{splitFormula}
\alias{splitFormula}
\title{Split a formula on ~ or |}
\usage{
splitFormula(f, operator = c("|", "~"))
}
\arguments{
\item{f}{formula}

\item{operator}{Either | or ~, the operators of the formula}
}
\value{
list containing lhs and rhs, the terms of the formula
}
\description{
Split a formula on ~ or |
}
