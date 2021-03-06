% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/episodes.R
\name{st_script_text_df}
\alias{st_script_text_df}
\title{Curate nested scripts to data frames}
\usage{
st_script_text_df(x, reset_line_numbers = TRUE)
}
\arguments{
\item{x}{a vector of lines of script. See example.}

\item{reset_line_numbers}{logical, adjust line numbers consistently and based on resulting data frame rows containing spoken lines.}
}
\value{
a data frame
}
\description{
Curate nested vectors of script lines to data frames.
}
\details{
This function works well for all scripts, though imperfectly. There are some rare instances of two-column text formatting in original scripts. This edge case is not currently handled.
Data extracted from original scripts is more informative and complete than those for which only caption-based transcriptions are available.
}
\examples{
\dontrun{
x <- st_script_download("data-raw/episode_scripts", TRUE)
system.time(x <- dplyr::mutate(x, text2 = purrr::map(text, st_script_text_df))) # ~10 minutes
}
}
