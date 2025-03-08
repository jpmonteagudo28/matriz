% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_record.R
\name{determine_position}
\alias{determine_position}
\title{Determine Row Insertion Position}
\usage{
determine_position(.data, record, .before = NULL, .after = NULL)
}
\arguments{
\item{.data}{Original data frame}

\item{record}{New row to be inserted}

\item{.before}{Optional. Row number before which to insert}

\item{.after}{Optional. Row number after which to insert}
}
\value{
List containing the potentially modified data frame and insertion position
}
\description{
Calculates the position for inserting a new row in a data frame
}
\keyword{internal}
