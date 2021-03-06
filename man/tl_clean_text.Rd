% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tl.R
\name{tl_clean_text}
\alias{tl_clean_text}
\title{Initial Timeline Text Cleaning}
\usage{
tl_clean_text(x, fix = c(151, 300))
}
\arguments{
\item{x}{character.}

\item{fix}{special fix for two footnote entries, no need to change.}
}
\value{
character
}
\description{
First pass of text cleaning after reading file containing timeline entries.
}
\examples{
\dontrun{tl_clean_text(x)}
}
