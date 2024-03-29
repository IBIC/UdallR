% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getClosestACVisit.R
\name{getClosestACVisit}
\alias{getClosestACVisit}
\title{Get the closest analytic site visits.}
\usage{
getClosestACVisit(dat, multivis.df = NULL, multivis.file = NULL,
  file.name = NULL)
}
\arguments{
\item{dat}{Cleaned REDCap data}

\item{multivis.file}{CSV file to read multivis information from, if you need
data other than the default}

\item{file.name}{CSV file to save output to. Defaults to NULL. If none is
given, returns data frame.}

\item{mulitivis.df}{Data frame with multivis information (e.g. the
lazy-loaded panuc_multivis_*_* datasets). Mutually exclusive with
multivis.file}
}
\value{
closest.visit A data table with the information from the analytic
center visit nearest to scan date.
}
\description{
Takes a cleaned data frame downloaded from REDCap and a file to save the
output to, if wanted, and outputs a data frame containing the analytical
core visit that was nearest to the scan date (preferably within 6 months).
}
\examples{
getClosestACVisit(visit1, multivis.df = panuc_multivis_07_07)

}
