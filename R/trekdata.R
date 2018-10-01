#' trekdata: Functions for extracting and curating \code{rtrek} package datasets.
#'
#' \code{trekdata} provides functions for extracting and curating \code{rtrek} package datasets from various data sources.
#'
#' The \code{trekdata} package assists with extracting and curating datasets that appear in the \code{rtrek} package.
#' The scripts associated with this preparation from source data would typically appear in the \code{rtrek} repository \code{data-raw} folder.
#' However, since the datasets are derived from varied sources and require a number of custom functions to facilitate all of the preprocessing,
#' it would make for an unwieldy collection of scripts in the \code{data-raw} folder.
#' Instead, all that work is packaged in \code{trekdata} to simplify the code contained in \code{rtrek/data-raw}.
#'
#' @docType package
#' @name trekdata
NULL

#' @importFrom magrittr %>%
NULL
