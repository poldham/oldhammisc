% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/orcid_rename.R
\name{orcid_rename}
\alias{orcid_rename}
\title{Rename orcid json fields with underscores}
\usage{
orcid_rename(data)
}
\arguments{
\item{data}{A data.frame containing orcid results.}
}
\value{
A data.frame
}
\description{
This converts hyphens and stops in column names for results from
  rorcid into R friendly underscores. This is a wrapper for dplyr rename for
  rorcid. Note that for CRAN use it will probably generate notes on bindings
  and a method to fix that (or an alternative approach) may be desirable.
}
\examples{
\dontrun{orcid_rename(data)}
}
