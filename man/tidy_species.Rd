% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidy_species.R
\name{tidy_species}
\alias{tidy_species}
\title{Tidy up species names}
\usage{
tidy_species(file, col)
}
\arguments{
\item{file}{A csv file containing species names in a column called Species}
}
\value{
a data frame
}
\description{
Read in a list of species and then tidy up by converting to lower case and then capitalizing the first letter before using to search with rgibif or other packages. Presently assumes a column called Species exists with capital S and needs updating to make it generic.
}
\examples{
\dontrun{df <- tidy_species(file)}
}
