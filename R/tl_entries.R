tl_entries <- function(x, step = 1){
  x <- tl_filter_lines(x)
  x <- split(x, cumsum(
    (!grepl("§|\\(|;|\"|\\d\\.\\d", x) & grepl("^     (\\d|C\\.\\d)", x)) |
      grepl("^     3\\.5 BILLION|^     2\\.5 BILLION", x) # correction
    ))
  x <- purrr::map(x, ~{
    if(length(grep(" -- ", .x[1]))){
      print(.x)
      return(c(strsplit(.x[1], " -- ")[[1]], .x[-1]))
    } else .x
  })
  purrr::map(x, tl_clean_entry, step = step)
}

.tl_move_footnote <- function(x){
  idx <- grep("^      \\d\\d?\\d?$", x)
  if(length(idx)){
    fnote <- as.numeric(trimws(x[idx]))
    idx <- idx[fnote <= 493]
    if(length(idx)){
      x[idx - 1] <- paste0(x[idx - 1], trimws(x[idx]))
      x <- x[-idx]
    }
  }
  x
}

.tl_move_indented <- function(x){
  idx <- grepl("^      .*", x) & !grepl("^      (SARATOGA|ENTERPRISE).*", x) # correction
  idx <- which(idx == TRUE)
  if(length(idx)){

    x[idx - 1] <- paste(x[idx - 1], "--", trimws(x[idx]))
    x <- x[-idx]
  }
  x
}

.tl_move_dashed <- function(x){
  idx <- grep("^\\s+-.*", x)
  if(length(idx)){
    y <- gsub("^-", "", trimws(x[idx]))
    x[idx - 1] <- paste(x[idx - 1], "--", y)
    x <- x[-idx]
  }
  x
}

.tl_move_linebreak <- function(x){
  idx <- grep("^     [A-Za-z ]+\"-.*", x)
  if(length(idx)){
    y <- gsub("^-", "", trimws(x[idx]))
    x[idx - 1] <- paste0(x[idx - 1], y)
    x <- x[-idx]
  }
  x
}

.tl_primary_entry <- function(x){
  idx <- grepl("see primary entry in", x)
  if(any(idx)) x[idx] <- gsub(".*entry in (\\d+)\\)", "\\1", x[idx])
  x[!idx] <- NA
  x
}

.tl_format <- function(x){
  out <- rep("book", length(x))
  idx <- !grep("[a-z]", x)
  if(length(idx)) idx <- grep("^(\\{.*\\} |)\"[A-Z ]+\"", x)
  if(length(idx)) out[idx] <- "story"
  idx <- grep("^\\([A-Z]+\\) \"", x)
  if(length(idx)) out[idx] <- "episode"
  out
}

.tl_stardate <- function(x){
  pat1 <- "^\\{STARDATE (\\d+\\.?\\d)\\}.*"
  pat2 <- "^\\{STARDATE (\\d+\\.?\\d)\ TO (\\d+\\.?\\d)\\}.*"
  idx1 <- grep(pat1, x)
  idx2 <- grep(pat2, x)
  y <- list(rep(NA, length(x)), rep(NA, length(x)))
  if(length(idx1)) y[[1]][idx1] <- gsub(pat1, "\\1", x[idx1])
  if(length(idx2)){
    y[[1]][idx2] <- gsub(pat2, "\\1", x[idx2])
    y[[2]][idx2] <- gsub(pat2, "\\2", x[idx2])
  }
  y
}

.tl_footnote_number <- function(x){
  idx <- grep("\\)(\\d+)( -- |$)", x)
  y <- rep(NA, length(x))
  if(length(idx)) y[idx] <- gsub(".*\\)(\\d+)( -- .*|$)", "\\1", x[idx])
  as.integer(y)
}

.tl_book_number <- function(x){
  idx <- grep("#\\d+\\)", x)
  y <- rep(NA, length(x))
  if(length(idx)) y[idx] <- gsub(".*#(\\d+)\\).*", "\\1", x[idx])
  as.integer(y)
}

tl_abb <- function(){
  series_abb <- c("CHA", "DS9", "ENT", "NF", "SCE", "SGZ", "ST", "TAS", "TLE", "TNG", "TOS", "VGR")
  series <- c("Challenger", "Deep Space Nine", "Enterprise", "New Frontier", "Starfleet Corps of Engineers",
              "Stargazer", "All-Series/Crossover", "The Animated Series", "The Lost Era",
              "The Next Generation", "The Original Series", "Voyager")
  anth_abb <- c("CON", "DS", "EL", "NL", "PAC", "SNW", "CT", "TLOD", "TNV", "TNV2", "TODW", "WLB", "YA")
  anth <- c(paste(
    c("Constellations", "Distant Shores", "Enterprise Logs", "New Frontier: No Limits", "Deep Space Nine: Prophecy and Change",
      "Strange New Worlds", "Tales from the Captain's Table", "The Lives of Dax", "The New Voyages",
      "The New Voyages 2", "Tales of the Dominion War", "Gateways: What Lay Beyond"), "Anthology"), "Young Adult Book")
  dplyr::data_frame(collection = c(series, anth), abb = c(series_abb, anth_abb),
                    type = rep(c("series", "anthology"), times = c(length(series), length(anth))))
}

.tl_collection <- function(x, type = c("series", "anthology")){
  type <- match.arg(type)
  x0 <- x
  x <- gsub("(.*) -- .*", "\\1", x)
  y <- tl_abb()$abb[tl_abb()$type == type]
  y <- sapply(y, function(i) grepl(paste0("\\(", i, " "), x) | grepl(paste0(i, "\\)"), x) | grepl(paste0(" ", i, " "), x))
  if(!length(y[[1]])){print(x0);print(y)}
  if(!is.matrix(y)) y <- t(as.matrix(y))
  if(any(rowSums(y) > 1)) warning("Multiple entries")
  y <- colnames(y)[apply(y, 1, function(x) if(!any(x)) 999 else which(x == TRUE))]
  if(is.list(y)) y <- sapply(y, paste0, collapse = ";")
  y
}

.tl_is_novelization <- function(x){
  grepl("NOVELIZATION", x)
}

.tl_title <- function(x){
  x <- gsub("\\{STARDATE.*\\} |\\(.*\\)", "", x)
  x <- sapply(strsplit(x, " -- ", x), "[", 1)
  x <- gsub("\"", "", x)
  trimws(x) %>% .tl_title_case()
}

.tl_title_case <- function(x){
  x <- gsub("(^|-|[[:space:]])([[:alpha:]])([[:alpha:]]+)", "\\1\\2\\L\\3", x, perl = TRUE)
  x <- gsub("(')([[:alpha:]])( |$)", "\\1\\L\\2\\3", x, perl = TRUE)
  gsub("( A | The | An | For | Of | In | On )", "\\L\\1", x, perl = TRUE)
}

.tl_setting <- function(x){
  y <- .tl_primary_entry(x)
  sapply(y, function(i) if(is.na(i)) "primary" else "secondary")
}

.tl_section <- function(x){
  x <- gsub("\\{STARDATE.*\\} |\\(.*\\)", "", x)
  x <- strsplit(x, " -- ")
  x <- sapply(x, function(i) if(length(i) == 1) NA else paste0(i[2:length(i)], collapse = "; "))
}

tl_clean_entry <- function(x, step = 1){
  x <- x[!grepl("^(\\s+|)$", x)] %>% .tl_move_footnote() %>% .tl_move_indented() %>%
    .tl_move_dashed() %>% .tl_move_linebreak()
  x <- gsub("  ", " ", trimws(x))
  year <- tolower(x[1])
  x <- x[-1]
  if(!length(x)) return()
  stardate <- .tl_stardate(x)
  d <- dplyr::data_frame(year = year, title = x,
                         series = .tl_collection(x, "series"),
                         anthology = .tl_collection(x, "anthology"),
                         format = .tl_format(x),
                         number = .tl_book_number(x),
                         novelization = .tl_is_novelization(x),
                         setting = .tl_setting(x),
                         stardate_start = stardate[[1]],
                         stardate_end = stardate[[2]],
                         section = .tl_section(x),
                         primary_entry_year = .tl_primary_entry(x),
                         footnote = .tl_footnote_number(x))
  d <- dplyr::mutate(d, title = .tl_title(title))
  #year, title, series, format, number, setting, stardate_start, stardate_end, section, primary_entry_year, footnote
  d
}
