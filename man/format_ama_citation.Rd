% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/citation.R
\name{format_ama_citation}
\alias{format_ama_citation}
\title{Format BibTeX Entry to AMA Citation Style}
\usage{
format_ama_citation(bibtex_entry)
}
\arguments{
\item{bibtex_entry}{A character string containing a single BibTeX entry}
}
\value{
An object of class c("bibentry", "character", "citation") containing:
  \item{string}{The original BibTeX entry}
  \item{year}{The publication year as numeric}
  \item{citation}{The formatted AMA citation string}
  \item{keywords}{A vector of keywords}
}
\description{
Converts a BibTeX entry into AMA (American Medical Association) citation format.
Handles article, book, and miscellaneous entry types.
}
\examples{
bibtex <- "@article{key,
  author = {Smith J and Jones K},
  title = {Example Title},
  journal = {Journal Name},
  year = {2024},
  volume = {1},
  pages = {1-10}
}"
citation <- format_ama_citation(bibtex)

}
\keyword{internal}
