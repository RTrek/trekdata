#' Timeline Entries Data Frame
#'
#' Create the timeline entries data frame.
#'
#' @param x character.
#'
#' @return a data frame
#' @export
#'
#' @examples
#' \dontrun{tl_entries(x)}
tl_entries <- function(x){
  x <- tl_filter_lines(x)
  x <- split(x, cumsum(
    (!grepl("\u00A7|\\(|;|\"|\\d\\.\\d", x) & grepl("^     (\\d|C\\.\\d|C\\. \\d)", x)) |
      grepl("^     3\\.5 BILLION|^     2\\.5 BILLION", x) # correction
    ))
  x <- purrr::map(x, ~{
    if(length(grep(" -- ", .x[1]))){
      return(c(strsplit(.x[1], " -- ")[[1]], .x[-1]))
    } else .x
  })
  purrr::map(x, tl_clean_entry)
}

.tl_move_footnote <- function(x){
  idx <- grep("^      \\d\\d?\\d?$", x)
  if(length(idx)){
    fnote <- as.numeric(trimws(x[idx]))
    idx <- idx[fnote <= 493]
    if(length(idx)){
      x[idx - 1] <- paste0(x[idx - 1], "FN_", trimws(x[idx]))
      x <- x[-idx]
    }
  }
  x <- gsub("AFTERMATH \\(SCE EBOOK #29\\)1", "AFTERMATH \\(SCE EBOOK #29\\)458", x)
  x <- gsub("FIRESTORM \\(TOS #68\\)21", "FIRESTORM \\(TOS #68\\)143", x)
  x
}

.tl_move_indented <- function(x){
  idx <- which(grepl("^      .*", x) & !grepl("^      (SARATOGA|ENTERPRISE).*", x)) # correction
  while(length(idx)){
    x[idx[1] - 1] <- paste(x[idx[1] - 1], "--", trimws(x[idx[1]]))
    x <- x[-idx[1]]
    idx <- which(grepl("^      .*", x) & !grepl("^      (SARATOGA|ENTERPRISE).*", x))
  }
  x
}

.tl_move_dashed <- function(x){
  idx <- grep("^\\s+-.*", x)
  while(length(idx)){
    y <- gsub("^-", "", trimws(x[idx[1]]))
    x[idx[1] - 1] <- paste(x[idx[1] - 1], "--", y)
    x <- x[-idx[1]]
    idx <- grep("^\\s+-.*", x)
  }
  x
}

.tl_move_bullets <- function(x){
  idx <- which(grepl("^\\s+.*-\\[.*\\]", x) & !grepl("^\\s+.*;\\s", x))
  while(length(idx)){
    y <- trimws(x[idx[1]])
    x[idx[1] - 1] <- paste(x[idx[1] - 1], "--", y)
    x <- x[-idx[1]]
    idx <- which(grepl("^\\s+.*-\\[.*\\]", x) & !grepl("^\\s+.*(;\\s|\\s--\\s.*-\\[.*\\])", x))
  }
  x
}

.tl_fix_kobayashi <- function(x){
  idx <- grep(" OBAYASHI", x)
  while(length(idx)){
    x[idx[1] - 1] <- paste0(x[idx[1] - 1], trimws(x[idx[1]]))
    x <- x[-idx[1]]
    idx <- grep(" OBAYASHI", x)
  }
  idx <- grep("(^|[^E]) KOBAYASHI", gsub("\\s+", " ", x))
  while(length(idx)){
    x[idx[1] - 1] <- gsub("E\\s+K", "E K", paste(x[idx[1] - 1], trimws(x[idx[1]])))
    x <- x[-idx[1]]
    idx <- grep("(^|[^E]) KOBAYASHI", gsub("\\s+", " ", x))
  }
  idx <- grep("KOBAYASHI($| $| [^M])", x)
  while(length(idx)){
    y <- trimws(x[idx[1] + 1])
    x[idx[1]] <- gsub("I\\s+M", "I M", paste(x[idx[1]], y))
    x <- x[-(idx[1] + 1)]
    idx <- grep("KOBAYASHI($| $| [^M])", x)
  }
  idx <- grep("KOBAYASHI M($| $|[^A])", x)
  while(length(idx)){
    y <- trimws(x[idx[1] + 1])
    x[idx[1]] <- gsub(" M ", " M", paste0(x[idx[1]], y))
    x <- x[-(idx[1] + 1)]
    idx <- grep("KOBAYASHI M($| $|[^A])", x)
  }
  x
}

.tl_fix_warstories <- function(d){
  idx <- which(grepl("DUOLOGY", d$section))
  d$section[idx] <- gsub("DUOLOGY: 21, 22\\)FN_453; ", "Duology: Book 21, 22; ", d$section[idx])
  d
}

.tl_move_linebreak <- function(x){
  fixes <- c("NOVELIZATION\\)178", "^     \\d+\\.\\d$",
             "^     \\d+-", "^\\s+\\([A-Z0-9 #]+\\)($|FN| -- )", "^     NOVELIZATION)$",
             "^     \\d+\u00A7", "^      THY FATHER \\(TLE\\)$",
             "     \" \\(ST SHORT STORY, SNW 8\\)$")
  idx <- grep(paste0(c("^     ([A-Za-z ]+\"-|\\d+, ).*", fixes), collapse = "|"), x)
  while(length(idx)){
    y <- gsub("^-", "", trimws(x[idx[1]]))
    x0 <- x[idx[1] - 1]
    idx2 <- grep("[A-Za-z]", substr(x0, nchar(x0), nchar(x0)))
    idx3 <- grep("[A-Za-z\\(]", substr(y, 1, 1))
    if(length(idx2) & length(idx3)) y <- paste0(" ", y)
    x[idx[1] - 1] <- paste0(x[idx[1] - 1], y)
    x <- x[-idx[1]]
    idx <- grep(paste0(c("^     ([A-Za-z ]+\"-|\\d+, ).*", fixes), collapse = "|"), x)
  }
  idx <- grep("THE $|THE V$", x)
  while(length(idx)){
    x[idx[1]] <- paste0(x[idx[1]], trimws(x[idx[1] + 1]))
    x <- x[-(idx[1] + 1)]
    idx <- grep("THE $|THE V$", x)
  }
  idx <- grep("^\\s+\\(ENT\\) \"E$", x)
  if(length(idx)){
    x[idx[1]] <- paste0(x[idx[1]], trimws(x[idx[1] + 1]))
    x <- x[-(idx[1] + 1)]
  }
  idx <- grep("From The Star Trek Chronology", x)
  if(length(idx)){
    x[idx - 1] <- paste(x[idx - 1], "--", paste0(trimws(x[idx + 0:3]), collapse = " "))
    x <- x[-c(idx + 0:3)]
  }
  idx <- grep(": $", x)
  while(length(idx)){
    x[idx[1]] <- paste0(x[idx[1]], trimws(x[idx[1] + 1]), collapse = "")
    x <- x[-c(idx[1] + 1)]
    idx <- grep(": $", x)
  }
  x <- gsub(":  -- ", ": ", x)
  x
}

.tl_primary_entry <- function(x){
  idx <- grepl("see primary entry in", x)
  if(any(idx)) x[idx] <- gsub(".*entry in (\\d+).*", "\\1", x[idx])
  x[!idx] <- NA
  as.integer(x)
}

.tl_format <- function(x){
  out <- rep(NA, length(x))
  x <- sapply(strsplit(x, " -- "), "[", 1)
  idx <- grep("[^a-z]", x)
  if(length(idx)){
    out[idx] <- "book"
    idx <- grep("^(\\{.*\\} |)\"[A-Z ]+\"", x)
  }
  if(length(idx)) out[idx] <- "story"
  idx <- grep("^\\([A-Z]+\\) \"", x)
  if(length(idx)) out[idx] <- "episode"
  out
}

.tl_stardate <- function(x){
  x <- gsub("FN_\\d+", "", x)
  x <- gsub("(STARDATE \\d+\\.) (\\d+)", "\\1\\2", x)
  pat1 <- "^\\{STARDATE (\\d+\\.?\\d+?)\\}.*"
  pat2 <- "^\\{STARDATE (\\d+\\.?\\d+?)\ (TO|THROUGH) (\\d+\\.?\\d+?)\\}.*"
  pat3 <- "^\\([A-Z]+\\) \".*\"-Stardate (\\d+\\.?\\d+?)( .*|)"
  pat4 <- "^\\([A-Z]+\\) \".*\"-Stardate (\\d+\\.?\\d+?) (to|through) (\\d+\\.?\\d+?)(,.*|$)"
  pat5 <- ".*STARDATES (\\d+\\.\\d) TO (\\d+\\.\\d).*"
  idx1 <- grep(pat1, x)
  idx2 <- grep(pat2, x)
  idx3 <- grep(pat3, x)
  idx4 <- grep(pat4, x)
  idx5 <- grep(pat5, x)
  y <- list(rep(NA, length(x)), rep(NA, length(x)))
  if(length(idx1)) y[[1]][idx1] <- gsub(pat1, "\\1", x[idx1])
  if(length(idx2)){
    y[[1]][idx2] <- gsub(pat2, "\\1", x[idx2])
    y[[2]][idx2] <- gsub(pat2, "\\3", x[idx2])
  }
  if(length(idx3)) y[[1]][idx3] <- gsub(pat3, "\\1", x[idx3])
  if(length(idx4)){
    y[[1]][idx4] <- gsub(pat4, "\\1", x[idx4])
    y[[2]][idx4] <- gsub(pat4, "\\3", x[idx4])
  }
  if(length(idx5)){
    y[[1]][idx5] <- gsub(pat5, "\\1", x[idx5])
    y[[2]][idx5] <- gsub(pat5, "\\2", x[idx5])
  }
  lapply(y, as.numeric)
}

.tl_detailed_date <- function(x, year){
  idx <- grepl("^\\{(.*)\\} .*", x)
  stardate_idx <- grepl("^\\{STARDATE.*\\} .*", x)
  idx <- which(idx & !stardate_idx)
  y <- rep(NA, length(x))
  if(length(idx)) y[idx] <- gsub("^\\{(.*)\\} .*", "\\1", x[idx])
  pat <- paste0(".*([A-Za-z]\"-)(", paste(month.name, collapse = "|"),
                ")(| \\d\\d?| \\d\\d?-\\d\\d?)($|,? )(\\d\\d\\d\\d).*")
  idx <- grep(pat, x)
  if(length(idx)) y[idx] <- gsub(pat, "\\2\\3\\4\\5", x[idx])
  y <- gsub("(.*), STARDATES.*", "\\1", y)
  y <- .tl_title_case(y)
  idx <- which(is.na(y) & !is.na(year))
  if(length(idx)){
    y[idx] <- year
  }
  y
}

.tl_strip_detailed_date <- function(x){
  idx <- grepl("^\\{(.*)\\} .*", x)
  stardate_idx <- grepl("^\\{STARDATE.*\\} .*", x)
  idx <- which(idx & !stardate_idx)
  if(length(idx)) x[idx] <- gsub("^\\{.*\\} (.*)", "\\1", x[idx])
  pat <- paste0("^(.*)-(", paste(month.name, collapse = "|"), ")(| \\d\\d?| \\d\\d?-\\d\\d?)(|,? )(|\\d\\d\\d\\d).*")
  idx <- grep(pat, x)
  if(length(idx)) x[idx] <- gsub(pat, "\\1", x[idx])
  x
}

.tl_footnote_number <- function(x){
  idx <- grep("(\\)|FN_|\"|#2|47457\\.1|\\]|#73\\) )(\\d+)( -- |$)", x)
  y <- rep(NA, length(x))
  if(length(idx)) y[idx] <- gsub(".*(\\)|FN_|\"|#2|47457\\.1|\\]|#73\\) )(\\d+)( -- .*|$)", "\\2", x[idx])
  as.integer(y)
}

.tl_book_number <- function(x){
  idx <- grep("#\\d+\\)", x)
  y <- rep(NA, length(x))
  if(length(idx)) y[idx] <- gsub(".*#(\\d+)\\).*", "\\1", x[idx])
  y <- as.integer(y)
  y[y >= 500] <- NA
  y
}

st_abb <- function(){
  series_abb <- c("AV", "CHA", "DS9", "DSC", "ENT", "KE", "NF", "PRO", "SKR", "SV",
                  "SCE", "SGZ", "ST", "TAS", "TLE", "TNG", "TOS", "TTN", "VAN", "VOY")
  series <- c("Abramsverse", "Challenger", "Deep Space Nine", "Discovery", "Enterprise",
              "Klingon Empire", "New Frontier", "Prometheus", "Seekers", "Shatnerverse",
              "Starfleet Corps of Engineers", "Stargazer", "All-Series/Crossover",
              "The Animated Series", "The Lost Era", "The Next Generation",
              "The Original Series", "Titan", "Vanguard", "Voyager")
  anth_abb <- c("CON", "DS", "EL", "NL", "PAC", "SNW", "CT", "TLOD", "TNV", "TNV2", "TODW", "WLB", "YA")
  anth <- c(paste(
    c("Constellations", "Distant Shores", "Enterprise Logs", "New Frontier: No Limits",
      "Deep Space Nine: Prophecy and Change", "Strange New Worlds", "Tales from the Captain's Table",
      "The Lives of Dax", "The New Voyages", "The New Voyages 2", "Tales of the Dominion War",
      "Gateways: What Lay Beyond"), "Anthology"), "Young Adult Book")
  other_abb <- c("REF")
  other <- c("Reference")
  dplyr::data_frame(collection = c(series, anth, other), abb = c(series_abb, anth_abb, other_abb),
                    type = rep(c("series", "anthology", "other"),
                               times = c(length(series), length(anth), length(other))))
}

.tl_collection <- function(x, type = c("series", "anthology")){
  type <- match.arg(type)
  x <- gsub("(.*) -- .*", "\\1", x)
  y <- st_abb()$abb[st_abb()$type == type]
  y <- sapply(y, function(i) grepl(paste0("\\(", i, "(-| )"), x) | grepl(paste0(i, "\\)"), x) |
                grepl(paste0(" ", i, " "), x) | grepl(paste0("-", i, " "), x))
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
  x <- gsub("FN_\\d+| -- .*", "", x)
  x <- gsub("\\{STARDATE.*\\} |\\(.*\\)|\\)\\d\\d?\\d?$", "", x)
  x <- gsub("(.*)-Stardate \\d+\\.?\\d+?($| (to|through) \\d+\\.?\\d+?(,|$))", "\\1", x)
  x <- gsub("(.*)( |\\.\\.\\.)\\d\\d?\\d?($| -- .*)", "\\1\\2\\3", x)
  x <- gsub("\\s+", " ", x)
  x <- gsub("\"", "", x)
  x <- gsub("\\(.*", "", x)
  x <- gsub("'Q'UANDRY", "'Q'uandry", x)
  x <- gsub("Ha'MARA", "Ha'Mara", x)
  trimws(x) %>% .tl_strip_detailed_date() %>% .tl_title_case()
}

.tl_title_case <- function(x){
  x <- gsub("(^|-|\\.\\.\\.|[[:space:]])([[:alpha:]]'?)([[:alpha:]]+)", "\\1\\2\\L\\3", x, perl = TRUE)
  x <- gsub("(')([[:alpha:]])( |$)", "\\1\\L\\2\\3", x, perl = TRUE)
  pat <- "(.*[^:] )(A|The|An|And|As|Is|To|For|From|Of|In|On)( .*)"
  x <- gsub(pat, "\\1\\L\\2\\E\\3", x, perl = TRUE)
  x <- gsub(pat, "\\1\\L\\2\\E\\3", x, perl = TRUE)
  x <- gsub("'TIL", "'til", x)
  x <- gsub("Ar-558", "AR-558", x)
  x <- gsub("4:...Sacrifice", "4: Sacrifice", x)
  x <- gsub("2: Call to Arms...", "2: Call to Arms", x)
  x
}

.tl_setting <- function(x){
  y <- .tl_primary_entry(x)
  sapply(y, function(i) if(is.na(i)) "primary" else "secondary")
}

.tl_section <- function(x){
  x <- gsub("\\{STARDATE.*\\} |[^( -- )]\\(.*\\)", "", x)
  x <- strsplit(x, " -- ")
  x <- sapply(x, function(i) if(length(i) == 1) NA else paste0(i[2:length(i)], collapse = "; "))
  x <- gsub(" \\(see primary.*\\)", "", x)
  x
}

.tl_year <- function(x){
  x <- gsub("^C\\.|,| AD$", "", x)
  if(grepl(" BC$", x)) x <- paste0("-", gsub(" BC", "", x))
  if(grepl("BILLION", x)) x <- paste0("-", gsub("^(\\d|\\d\\.\\d) .*", "\\1", x), "e9")
  as.numeric(x)
}

.tl_year_detailed <- function(x){
  y <- grepl("C\\.|,|BILLION| AD$| BC$", x)
  if(!y) return(NA)
  y <- gsub("^C\\.", "~", x)
  y <- tolower(y)
  y <- gsub("( ad$| bc$)", "\\U\\1", y, perl = TRUE)
  y
}

.tl_unneeded_quotes <- function(x){
  f <- function(x) length(which(strsplit(x, "")[[1]] == "\""))
  f2 <- function(x){
    nc <- nchar(x)
    substr(x, 1, 1) == "\"" & substr(x, nc, nc) == "\""
  }
  n <- sapply(x, f)
  quoted <- sapply(x, f2)
  idx <- which(n == 2 & quoted)
  if(length(idx)){
    x[idx] <- gsub("\"", "", x[idx])
  }
  if(any(n == 1)){
    x[n == 1] <- gsub("\"", "", x[n == 1])
  }
  x
}

.tl_cleanup <- function(x){
  x <- gsub("(CHAPTER|CHAPTERS|Chapter|Chapters|chapter|chapters)", "Ch", x)
  x <- gsub("(.*)( )(:|;|\\.|,)(.*)", "\\1\\3\\4", x)
  x <- gsub(" (P|p)arts? (\\d)", " Part \\2", x)
  x <- gsub(", (Book|Part) ", " \\1 ", x)
  x <- gsub("Book One", "Book 1", x)
  x <- gsub("Book Two", "Book 2", x)
  x <- gsub("Book Three", "Book 3", x)
  x <- gsub("1 and Two", "1 and 2", x)
  x <- gsub("#(\\d)", "\\1", x)
  x <- gsub("Section31", "Section 31", x)
  x <- gsub("\u00A7\u00A7?", " Section ", x)
  x <- gsub("(S|s)ections?( \\d)", " Section \\2", x)
  x <- gsub("U\\.S\\.S\\.", "USS", x)
  x <- gsub("\"Seventy Years Ago,\"", "Seventy Years Ago,", x)
  x <- gsub("\\s+", " ", trimws(x))
  x <- gsub("\\.([A-Za-z])", "\\. \\1", x)
  x <- gsub("([\\d])(ST|ND|TH) ", "\\1\\L\\2\\E ", x, perl = TRUE)
  idx <- grep("^(\\d+|One) Years? Ago$", x)
  if(length(idx)) x[idx] <- NA
  x
}

.tl_section_to_date <- function(x, y){
  idx <- which(grepl("^\"(\\d+|One) Years? Ago\"$", y) & is.na(x))
  if(length(idx)) x[idx] <- tolower(gsub("\"", "", gsub("One", "1", y[idx])))
  x
}

tl_clean_entry <- function(x){
  x <- gsub("; ", ", ", x)
  x <- gsub("\"A &\\s(\"|)", "\"A AND\"", x)
  x <- .tl_fix_kobayashi(x) %>% .tl_move_linebreak()
  x <- x[!grepl("^(\\s+|)$", x)] %>% .tl_move_footnote() %>% .tl_move_bullets() %>%
    .tl_move_indented() %>% .tl_move_dashed() #%>%
  x <- gsub("\\(VGR-YA #2216", "\\(VGR-YA #2\\)216", x) # fix
  x <- gsub("\\s+", " ", trimws(x))
  year <- .tl_year(x[1])
  year_detailed_date <- .tl_year_detailed(x[1])
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
                         detailed_date = .tl_detailed_date(x, year_detailed_date),
                         section = .tl_section(x),
                         primary_entry_year = .tl_primary_entry(x),
                         footnote = .tl_footnote_number(x))
  d <- dplyr::mutate(d, title = .tl_title(.data[["title"]]))
  d <- dplyr::mutate(d, title = .tl_cleanup(.data[["title"]]),
                     detailed_date = .tl_section_to_date(.data[["detailed_date"]], .data[["section"]]),
                     section = .tl_cleanup(.tl_unneeded_quotes(.data[["section"]])))
  d <- .tl_fix_warstories(d)
  idx <- grep("spacedock|Kirk is believed killed|Chronology|television", d$title)
  if(length(idx)) d <- dplyr::slice(d, -idx)
  d
}
