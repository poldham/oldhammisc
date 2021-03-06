% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/earliest_priority.R
\name{earliest_priority}
\alias{earliest_priority}
\title{Identify the earliest priority number in derwent innovation documents}
\usage{
earliest_priority(x, id = NULL, col = NULL, date = NULL)
}
\arguments{
\item{x}{a data.frame}

\item{id}{A column containing ids}

\item{col}{A list column containing the date field}

\item{date}{A date as ymd. Character fields will be converted to date}
}
\value{
data.frame
}
\description{
An input data frame from derwent innovation containing a column with priority numbers and dates. The function identifies the earliest dates.
}
\examples{
\dontrun{resultssofar \%>\% unnest(epodoc_priority) \%>\% split(.$id)}
}
