% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import.R
\name{validate_columns}
\alias{validate_columns}
\title{Validate and Clean Imported Data Matrix}
\usage{
validate_columns(
  data,
  extra_columns = NULL,
  drop_extra = FALSE,
  silent = FALSE
)
}
\arguments{
\item{data}{A data frame containing the imported matrix.}

\item{extra_columns}{A character vector of allowed additional columns beyond the required ones. Defaults to NULL.}

\item{drop_extra}{A logical value indicating whether to remove extra columns that are not in `extra_columns`. Defaults to FALSE.}

\item{silent}{A logical value indicating whether to suppress messages. Defaults to FALSE.}
}
\value{
A cleaned data frame with required columns intact and, optionally, extra columns removed.
}
\description{
This function ensures that the imported data contains all required columns,
optionally removes unwanted extra columns, and provides informative messages
about the dataset's structure.
}
\details{
The function checks whether all required columns are present in the data. If any required columns are
missing, it stops execution and informs the user.

It also identifies extra columns beyond the required set and compares them against the allowed `extra_columns`.
If `drop_extra = TRUE`, it removes any extra columns not listed in `extra_columns`.
If `drop_extra = FALSE`, it retains the extra columns but issues a message unless `silent = TRUE`.
}
\note{
The function assumes that column names in `data` are correctly formatted and case-sensitive.
}
