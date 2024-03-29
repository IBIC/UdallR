% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/udallCheckDataWide.R
\name{udallCheckDataWide}
\alias{udallCheckDataWide}
\title{Check the data that have been processed by udallCleanREDCapDataWide
This function contains arbitrary sanity checks to make sure that there
are no obvious errors in the data. It will issue warnings, but currently
will not stop if there is a problem.}
\usage{
udallCheckDataWide(dat, error.file)
}
\arguments{
\item{dat}{Data frame that has been processed and cleaned missing codes
replaced with NA}

\item{error.file}{A string that has the location of a file to save error
descriptions too.}
}
\description{
Check the data that have been processed by udallCleanREDCapDataWide
This function contains arbitrary sanity checks to make sure that there
are no obvious errors in the data. It will issue warnings, but currently
will not stop if there is a problem.
}
