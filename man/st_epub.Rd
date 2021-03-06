% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/books.R
\name{st_epub}
\alias{st_epub}
\title{Read Star Trek epub files}
\usage{
st_epub(file, fields = NULL, chapter_pattern = NULL,
  add_pattern = NULL, cleaner = NULL, drop_sections = NULL,
  fix_date = TRUE, fix_text = TRUE)
}
\arguments{
\item{file}{character.}

\item{fields}{character.}

\item{chapter_pattern}{character.}

\item{add_pattern}{list.}

\item{cleaner}{function.}

\item{drop_sections}{character.}

\item{fix_date}{logical.}

\item{fix_text}{logical.}
}
\value{
a data frame
}
\description{
A wrapper around \code{epubr::epub} for Star Trek epub files.
}
\examples{
\dontrun{st_epub(file)}
}
