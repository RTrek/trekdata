# nolint start

#' Copy Star Trek books
#'
#' Copy Star Trek epub files, with some changes.
#'
#' @param in_dir character.
#' @param out_dir character.
#'
#' @return returns invisibly
#' @export
#'
#' @examples
#' \dontrun{st_book_copy(in_dir, out_dir)}
st_book_copy <- function(in_dir, out_dir){
  if(!file.exists(out_dir)){ # runs if directory has not been created, does not check files
    files <- list.files(in_dir, pattern = ".epub$", recursive = TRUE, full.names = TRUE)
    pat <- " \\(Retail-PubUpd\\)" # updated edition
    pat2 <- " \\(Retail\\)" # previous edition
    idx_updated <- grep(pat, files) # index of updated novels
    updated <- files[idx_updated] # updated novels
    updated_trimmed <- gsub(pat, "", updated) # updated novels, pattern removed
    idx_replaced <- which(gsub(pat2, "", files) %in% updated_trimmed) # which previous edition novels have an updated edition available
    files <- files[-idx_replaced] # all novels minus the previous editions for which an update is in the list
    out <- file.path(out_dir, gsub(paste0(in_dir, "/"), "", files))
    out <- gsub(paste0(pat, "|", pat2, "| UNFORMATTED"), "", out)
    out <- paste0(dirname(out), ".epub")
    purrr::walk(dirname(out), ~dir.create(.x, showWarnings = FALSE, recursive = TRUE))
    file.copy(files, out)
    cat("Files copied.\n")
  }
  invisible()
}

# nolint end

.st_chapcheck1 <- c("INVINCIBLE", "Have Tech, Will Travel", "SPARTACUS", "WAR DRUMS", "THE ROMULAN STRATAGEM",
                   "ROGUE SAUCER", "I,Q", "GEMWORLD: BOOK ONE OF TWO", "The Genesis Wave Book Three",
                   "Star Trek: The Next Generation: The Stuff of Dreams", "Memory Prime")

.st_chapcheck2 <- c("MARTYR", "No Limits", "Spectre", "SPARTACUS", "WAR DRUMS", "THE ROMULAN STRATAGEM",
                   "ROGUE SAUCER", "I,Q", "GEMWORLD: BOOK ONE OF TWO",
                   "Star Trek: The Next Generation: The Stuff of Dreams", "Memory Prime")

.st_pat <- paste0(
  "(C|c)(H|h)(_|\\d)|^i0\\d\\d|000000|c0\\d\\d|^c(\\d$|\\d\\d$)|",
  "tocref\\d|Chapter|chapter_\\d\\d|C\\d-|p\\d_c\\d|^c_\\d|",
  "^bk\\d|^rule\\d|^prt$|_RWTOC|\\d+text-(\\d|\\d\\d)$", collapse = "")

# Star Trek novel overrides
# Pattern helper function specific to Star Trek novels. A function that takes no arguments and returns a named list of three elements: \code{pattern} is a regular expression pattern string,
# \code{chapter_check} is a character vector giving exact (in-metadata) book titles as a filter to only apply the supplemental \code{pattern} to specific books,
# and \code{chapter_doublecheck} is a vector of titles that applies additional checks that may only be necessary and helpful (and non-harmful) for specific titles.
.st_pat_list <- function(){
  list(pattern = "toc_ref\\d|^con$|^i2000\\d|^ref(\\d$|\\d\\d$)|^dreams-\\d",
       chapter_check = .st_chapcheck1, chapter_doublecheck = .st_chapcheck2)
}

# Star Trek novel section filter
# Regular expression pattern for dropping commonly named non-chapter sections that typically apply to popular Star Trek novels.
.st_sec_drop <- "^(C|c)o(v|n|p)|^(T|t)it|^(A|a)ck|^htl|reg(front|back)|signup|isbn|font|css|style|image|logo"

.st_fields <- c("title", "creator", "date", "identifier", "publisher", "file")

.st_series <- function(x, subseries = FALSE, parent_dir = "novels"){
  x <- strsplit(dirname(x), "/")
  idx <- purrr::map_dbl(x, ~(which(.x == parent_dir) + 1))
  if(subseries) idx <- idx + 1
  x <- purrr::map_chr(seq_along(x), ~x[[.x]][idx[.x]])
  x[x == "All-Series_Crossover"] <- "All-Series/Crossover"
  x
}

st_add_series <- function(d, files){
  dplyr::mutate(d, series = .st_series(files, FALSE), subseries = .st_series(files, TRUE))
}

st_add_dedication <- function(d){
  x <- purrr::map_chr(d[["data"]], ~{
    idx <- grep("^(D|d)ed", substr(.x[["section"]], 1, 3))
    if(length(idx)) .x[["text"]][idx[1]] else as.character(NA)
  })
  dplyr::mutate(d, dedication = x)
}

st_fix_date <- function(x){
  if(!"file" %in% names(x)) return(x)
  y <- stringr::str_extract(x$file, "\\d{8}")
  if(any(is.na(y)) && "subseries" %in% names(x) && !all(is.na(x$subseries))){
    y2 <- stringr::str_extract(x$subseries, "\\d{8}")
    y[is.na(y)] <- y2[is.na(y)]
  }
  if(all(is.na(y))) return(x)
  dplyr::mutate(x, date = ifelse(is.na(y), .data[["date"]],
                                 paste(substr(y, 1, 4), substr(y, 5, 6), substr(y, 7, 8), sep = "-")))
}

st_fix_case <- function(x){
  f <- function(x, authors = FALSE){
    x[!is.na(x)] <- tools::toTitleCase(tolower(x[!is.na(x)]))
    x <- gsub(" & ", " and ", x)
    if(authors){
      x <- gsub("( [a-z]) ", " \\U\\1\\. ", x, perl = TRUE)
      x <- gsub("(^[A-Za-z]) ", "\\U\\1\\. ", x, perl = TRUE)
      x <- gsub("( Ii+)( |$)", "\\U\\1\\2", x, perl = TRUE)
    }
    x <- gsub("\\s+", " ", x)
    x <- gsub("( Vi$| Vii$| Viii$)", "\\U\\1", x, perl = TRUE)
    x <- gsub("( Ii$| Iii$| X$)", "\\U\\1", x, perl = TRUE)
    paste0(toupper(substr(x, 1, 1)), substring(x, 2))
  }
  if("title" %in% names(x)) x <- dplyr::mutate(x, title = f(.data[["title"]]))
  if("creator" %in% names(x)) x <- dplyr::mutate(x, creator = f(.data[["creator"]], authors = TRUE))
  if("publisher" %in% names(x)) x <- dplyr::mutate(x, publisher = f(.data[["publisher"]]))
  x
}

st_fix_bantam <- function(x){
  a <- c("Bantam Episodes", "Bantam Novels") # nolint
  dplyr::mutate(x, subseries = dplyr::case_when(
    grepl(a[1], .data[["subseries"]]) ~ a[1],
    grepl(a[2], .data[["subseries"]]) ~ a[2],
    TRUE ~ .data[["subseries"]]))
}

st_fix_nchap <- function(x){
  dplyr::mutate(x, nchap = ifelse(.data[["nchap"]] == 0 |
                                    grepl("Bantam Episodes", .data[["subseries"]]) |
                                    (grepl("Bantam Novels", .data[["subseries"]]) & .data[["nchap"]] < 10),
                                  as.integer(NA), .data[["nchap"]]))
}

.st_title_from_file <- function(x){
  files <- gsub("\\.epub$", "", x[["file"]])
  x$title <- purrr::map_chr(files, ~paste0(strsplit(.x, " - ")[[1]][-1], collapse = ": "))
  x
}

.st_series_from_file <- function(x){
  x$series_abb <- purrr::map_chr(x[["file"]], ~{
    gsub("\\d+ ([A-Z9]+|[A-Z9]+-[A-Z9]+).*", "\\1", strsplit(.x, " - ")[[1]][1])
  })
  x
}

.st_number_from_file <- function(x){
  x$number <- purrr::map_int(x[["file"]], ~{
    a1 <- strsplit(.x, " - ")[[1]][1]
    a2 <- gsub("^\\d+ ([A-Z9]+|[A-Z9]+-[A-Z9]+) (\\d+)", "\\2", a1)
    if(a1 == a2) a2 <- as.integer(NA)
    as.integer(a2)
  })
  x
}

st_title_update <- function(x, use_filename = TRUE){
  if(use_filename) .st_title_from_file(x) else st_title_sub_specific(st_title_sub(x))
}

st_series_update <- function(x){
  .st_series_from_file(x) %>% .st_number_from_file()
}

st_title_sub <- function(x){
  f <- function(x){
    x <- gsub("^Star Trek: ", "", x)
    x <- gsub("Typhon Pact ", "Typhon Pact: ", x)
    x <- gsub("^Shadow$", "Section 31: Shadow", x)
    x <- gsub("(^[A-Za-z]+): (Section 31)$", "\\2: \\1", x)
    x <- gsub("of\\sTwo", "of 2", x)
    x <- gsub("of\\sThree", "of 3", x)
    x <- gsub("Treck", "Trek", x)
    x <- gsub("(T|t)ng", "TNG", x)
    x <- gsub("(^|\\s)q(\\s|-)", "\\1Q\\2", x)
    x <- gsub("\\s([a-z]$)", " \\U\\1", x, perl = TRUE)
    x <- gsub("book", "Book", x)
    x <- gsub("volume", "Volume", x)
    x <- gsub("(Book|Volume.*)(\\sOne)", "\\1 1", x)
    x <- gsub("(Book|Volume.*)(\\sTwo)", "\\1 2", x)
    x <- gsub("(Book|Volume.*)(\\sThree)", "\\1 3", x)
    x <- gsub(",\\s(B|V)(ook|olume)", " \\1\\2", x)
    x <- gsub("(A-Za-z0-9),(A-Za-z0-9)", "\\1,\\s\\2", x)
    x <- gsub("\U00AE|\U2122|\U00E2|(B|b)ook\\s|#|Star\\sTrek:\\s", "", x)
    x <- gsub("(\\.[a-z]\\.):", "\\U\\1", x, perl = TRUE)
    x <- gsub("(\\.[a-z]\\.)", "\\U\\1", x, perl = TRUE)
    x <- gsub("(\\.[a-z]\\.\\s)", "\\U\\1", x, perl = TRUE)
    x <- gsub("^The Original Series(:\\s|\\s)", "TOS: ", x)
    x <- gsub("^The Next Generation(:\\s|\\s)", "TNG: ", x)
    x <- gsub("^Deep Space Nine(:\\s|\\s)", "DS9: ", x)
    x <- gsub("^Voyager(:\\s|\\s)", "VOY: ", x)
    x <- gsub("^Dti: ", "Department of Temporal Investigations: ", x)
    x <- gsub("Volume", "Vol", x)
    x <- gsub(": - ", " ", x)
    x <- gsub("s - \\d(\\d\\d) - ", "s \\1: ", x)
    x <- romans_sub(x)
    x <- gsub("( V$)", " 5", x)
    x <- gsub("( X$)", " 10", x)
    x <- gsub("Terok nor", "Terok Nor", x)
  }
  if("title" %in% names(x)) x <- dplyr::mutate(x, title = f(.data[["title"]]))
  x[["title"]] <- gsub("^Star Trek: ", "", x[["title"]])
  x
}

st_title_sub_specific <- function(x){
  x <- dplyr::mutate(x, title = dplyr::case_when(
    .data[["title"]] == "To Reign in Hell" &
      .data[["date"]] == "2005-01-04" ~ "To Reign in Hell To Reign in Hell: The Exile of Khan Noonien Singh",
    .data[["title"]] == "Epiphany" & .data[["date"]] == "2007-04-17" ~ "Vulcan's Soul 3: Epiphany",
    .data[["title"]] == "The Fifty-Year Mission" & .data[["date"]] == "2016-08-30" ~
      "The Fifty-Year Mission: The Complete, Uncensored, Unauthorized Oral History of Star Trek Vol 2",
    .data[["title"]] == "The Definitive Star Trek Trivia Book" & .data[["date"]] == "2000-04-01" ~
      "The Definitive Star Trek Trivia Book 1",
    .data[["title"]] == "The Definitive Star Trek Trivia Book" & .data[["date"]] == "2001-04-03" ~
      "The Definitive Star Trek Trivia Book 2",
    .data[["date"]] == "1998-07-01" & grepl("Strange", .data[["title"]]) ~ "Strange New Worlds 1",
    .data[["date"]] == "2001-05-01" & grepl("Strange", .data[["title"]]) ~ "Strange New Worlds 4",
    .data[["date"]] == "1989-10-01" & grepl("Lost", .data[["title"]]) ~ "The Lost Years",
    .data[["date"]] == "1992-01-01" & grepl("Undiscovered", .data[["title"]]) ~ "Star Trek 6: The Undiscovered Country",
    .data[["date"]] == "1997-06-01" & grepl("Mind", .data[["title"]]) ~ "Mind Meld",
    .data[["date"]] == "2008-12-30" & grepl("Sacrifices of War", .data[["title"]]) ~ "Errand of Fury 3: Sacrifices of War",
    .data[["date"]] == "2011-07-26" & grepl("Cast", .data[["title"]]) ~ "Cast No Shadow",
    .data[["date"]] == "1991-11-01" & grepl("Reuinion", .data[["title"]]) ~ "Reunion",
    .data[["date"]] == "1994-12-01" & grepl("Generations", .data[["title"]]) ~ "Star Trek: Generations",
    .data[["date"]] == "1996-12-01" & grepl("Contact", .data[["title"]]) ~ "Star Trek: First Contact",
    .data[["date"]] == "1998-11-01" & grepl("Insurrection", .data[["title"]]) ~ "Star Trek: Insurrection",
    .data[["date"]] == "2011-03-29" & grepl("Generation", .data[["title"]]) ~ "Indistinguishable from Magic",
    .data[["date"]] == "2009-08-25" & grepl("Deep Space Nine", .data[["title"]]) ~ "The Never-Ending Sacrifice",
    .data[["date"]] == "1996-04-01" & grepl("Ghost of a Chance", .data[["title"]]) ~ "Ghost of a Chance",
    .data[["date"]] == "1996-07-01" & grepl("Invasion", .data[["title"]]) ~ "Invasion 4 - The Final Fury",
    .data[["date"]] == "1996-10-01" & grepl("Mosaic", .data[["title"]]) ~ "Mosaic",
    .data[["date"]] == "1997-12-01" & grepl("Voyager", .data[["title"]]) ~ "Marooned",
    .data[["date"]] == "1998-09-01" & grepl("Voyager", .data[["title"]]) ~ "Seven of Nine",
    .data[["date"]] == "1999-10-01 " & grepl("Voyager", .data[["title"]]) ~ "Equinox",
    .data[["date"]] == "2009-09-29" & grepl("Voyager", .data[["title"]]) ~ "Unworthy",
    .data[["date"]] == "2011-10-25" & grepl("Romulan", .data[["title"]]) ~ "The Romulan War: To Brave the Storm",
    .data[["date"]] == "2009-10-27" & grepl("Titan", .data[["title"]]) ~ "Synthesis",
    .data[["date"]] == "1998-02-01" & grepl("Frontier", .data[["title"]]) ~ "New Frontier (Omnibus)",
    .data[["date"]] == "2001-09-26" & grepl("Invincible", .data[["title"]]) ~ "SCE: Invincible 2",
    .data[["date"]] == "2004-09-15" & grepl("44", .data[["title"]]) ~ "SCE 44: Where Time Stands Still",
    .data[["date"]] == "2007-07-01" & grepl("Ghost", .data[["title"]]) ~ "Corps of Engineers: Ghost",
    .data[["date"]] == "2017-11-28" & grepl("Fire", .data[["title"]]) ~ "Prometheus: Fire with Fire",
    .data[["date"]] == "2000-03-01" & grepl("S9", .data[["title"]]) ~ "The Captain's Table 1-6 (Omnibus)",
    .data[["date"]] == "1997" & .data[["title"]] == "Star Trek Day of Honor" ~ "Day of Honor (Omnibus)",
    .data[["date"]] == "2009-12-29" & grepl("Universe", .data[["title"]]) ~ "Mirror Universe: The Sorrows of Empire",
    .data[["date"]] == "2009-01-27" & grepl("Destiny", .data[["title"]]) ~ "Destiny: A Singular Destiny",
    .data[["date"]] == "2013-05-21" & grepl("Darkness", .data[["title"]]) ~ "Star Trek: Into Darkness",
    .data[["date"]] == "1997-09-01" & grepl("Traveler", .data[["title"]]) ~ "Klingon for the Galactic Traveler",
    .data[["date"]] == "2003-01-07" & grepl("Companion", .data[["title"]]) ~ "The Next Generation Companion",
    .data[["date"]] == "2012-04-03" & grepl("CookBook", .data[["title"]]) ~ "Star Trek Cookbook",
    .data[["date"]] == "2012-10-01" & grepl("365", .data[["title"]]) ~ "Star Trek The Next Generation 365",
    .data[["date"]] == "2010-03-30" & grepl("Online", .data[["title"]]) ~ "Star Trek Online: The Needs of the Many",
    .data[["date"]] == "1999-11-01" & .data[["title"]] == "Star Trek" ~ "New Worlds New Civilizations",
    .data[["date"]] == "2000-02-01" & grepl("Hamlet", .data[["title"]]) ~ "The Klingon Hamlet",
    .data[["date"]] == "2010-03-16" & grepl("Seven", .data[["title"]]) ~ "Seven Deadly Sins",
    .data[["date"]] == "1998-10-01" & grepl("^The Dominion War$", .data[["title"]]) ~ "The Dominion War 2: Call to Arms",
    .data[["date"]] == "1998-10-01" & grepl("Behind Enemy Lines", .data[["title"]]) ~ "The Dominion War 1: Behind Enemy Lines",
    .data[["date"]] == "1998-11-01" & grepl("Tunnel", .data[["title"]]) ~ "The Dominion War 3: Tunnel Through the Stars",
    .data[["date"]] == "2008-09-30" & grepl("Destiny", .data[["title"]]) ~ "Destiny 1: Gods of Night",
    .data[["date"]] == "2008-10-28" & grepl("Destiny", .data[["title"]]) ~ "Destiny 2: Mere Mortals",
    .data[["date"]] == "2008-11-25" & grepl("Destiny", .data[["title"]]) ~ "Destiny 3: Lost Souls",
    .data[["date"]] == "1997-09-01" & grepl("Armageddon", .data[["title"]]) ~ "Day of Honor 2: Armageddon Sky",
    .data[["date"]] == "1997-09-01" & grepl("Ancient", .data[["title"]]) ~ "Day of Honor 1: Ancient Blood",
    .data[["date"]] == "1997-10-01" & grepl("Treaty", .data[["title"]]) ~ "Day of Honor 4: Treaty's Law",
    .data[["date"]] == "1997-10-01" & grepl("Klingon", .data[["title"]]) ~ "Day of Honor 3: Her Klingon Soul",
    .data[["date"]] == "2001-01-01" & grepl("^Dark Passions$", .data[["title"]]) ~ "Dark Passions 1",
    .data[["date"]] == "1997" & .data[["title"]] == "Day of Honor" ~ "Day of Honor: Episode Novelization",
    .data[["date"]] == "2011-11-29" & grepl("Lions", .data[["title"]]) ~ "Mirror Universe: Rise Like Lions",
    .data[["date"]] == "1999-03-01" & grepl("Liberated", .data[["title"]]) ~ "Rebels 3: The Liberated",
    .data[["date"]] == "2000-04-01" & grepl("Inferno", .data[["title"]]) ~ "Millennium 3: Inferno",
    .data[["date"]] == "2001-05-01" & .data[["title"]] == "Avatar 1 of 2" ~ "Avatar 1",
    .data[["date"]] == "2001-05-01" & .data[["title"]] == "Avatar: 2 of 2" ~ "Avatar 2",
    .data[["date"]] == "2001-08-28" & grepl("Gateways:demons", .data[["title"]]) ~ "Gateways: Demons of Air and Darkness",
    .data[["date"]] == "2002-01-01" & .data[["title"]] == "Millennium" ~ "Millennium (Omnibus)",
    .data[["date"]] == "2002-09-01" & grepl("Twilight", .data[["title"]]) ~ "Mission Gamma 1: Twilight",
    .data[["date"]] == "2002-09-01" & grepl("Spirit", .data[["title"]]) ~ "Mission Gamma 2: This Gray Spirit",
    .data[["date"]] == "2002-10-01" & grepl("Cathedral", .data[["title"]]) ~ "Mission Gamma 3: Cathedral",
    .data[["date"]] == "2002-10-01" & grepl("Lesser", .data[["title"]]) ~ "Mission Gamma 4: Lesser Evil",
    .data[["date"]] == "2004-05-25" & .data[["title"]] =="Worlds of Star Trek Deep Space Nine" ~ "Worlds of Star Trek Deep Space Nine Vol 1: Cardassia and Andor",
    .data[["date"]] == "2005-01-25" & grepl("Vol 2", .data[["title"]]) ~ "Worlds of Star Trek Deep Space Nine Vol 2: Trill and Bajor",
    .data[["date"]] == "2005-01-25" & grepl("Vol 3", .data[["title"]]) ~ "Worlds of Star Trek Deep Space Nine Vol 3: Ferenginar and The Dominion",
    TRUE ~ .data[["title"]]
  ))
  x
}

st_author_sub <- function(x){
  dplyr::mutate(x, creator = gsub("(\\.[a-z]\\.)", "\\U\\1", .data[["creator"]], perl = TRUE))
}

st_pub_sub <- function(x){
  f <- function(x){
    x[stringr::str_detect(x, "Pocket|Packet")] <- "Pocket Books"
    x[stringr::str_detect(x, "Klingon Language Institute")] <- "Klingon Language Institute"
    x[stringr::str_detect(x, "Elysium")] <- "Elysium"
    x[stringr::str_detect(x, "Schuster|S and s")] <- "Simon and Schuster"
    x[stringr::str_detect(x, "Martin")] <- "St. Martin's Press"
    x[stringr::str_detect(x, "Titan")] <- "Titan Books"
    x[stringr::str_detect(x, "^Bantam$")] <- "Bantam Books"
    x
  }
  if("publisher" %in% names(x)) x <- dplyr::mutate(x, publisher = f(.data[["publisher"]]))
  x
}

clean_book_text <- function(x){
  if(!requireNamespace("tm")){
    message("Package `tm` must be installed to perform additional cleaning steps.")
    return(x)
  }
  tm::stripWhitespace(x) #%>% qdap::replace_ordinal() %>% qdap::replace_symbol()
}

#' Read Star Trek epub files
#'
#' A wrapper around \code{epubr::epub} for Star Trek epub files.
#'
#' @param file character.
#' @param fields character.
#' @param chapter_pattern character.
#' @param add_pattern list.
#' @param cleaner function.
#' @param drop_sections character.
#' @param fix_date logical.
#' @param fix_text logical.
#'
#' @return a data frame
#' @export
#'
#' @examples
#' \dontrun{st_epub(file)}
st_epub <- function(file, fields = NULL, chapter_pattern = NULL, add_pattern = NULL,
                    cleaner = NULL, drop_sections = NULL, fix_date = TRUE, fix_text = TRUE){
  if(is.null(fields)) fields <- .st_fields
  if(is.null(chapter_pattern)) chapter_pattern <- .st_pat
  if(is.null(add_pattern)) add_pattern <- .st_pat_list()
  if(is.null(cleaner)) cleaner <- clean_book_text
  if(is.null(drop_sections)) drop_sections <- .st_sec_drop
  d <- epubr::epub(file, fields = fields, drop_sections = drop_sections, chapter_pattern = chapter_pattern,
                   add_pattern = add_pattern)
  d <- st_add_series(d, file) %>% st_add_dedication() %>% st_series_update()
  if(fix_date) d <- st_fix_date(d)
  d <- st_fix_bantam(d)
  d <- st_fix_nchap(d)
  if(fix_text){
    d <- st_fix_case(d)
    d <- st_title_update(d, use_filename = TRUE)
    d <- st_author_sub(d) %>% st_pub_sub()
    d <- dplyr::mutate_if(d, is.character, trimws)
  }
  if(is.function(cleaner)){
    nested <- names(d$data[[1]])
    d <- tidyr::unnest(d, .data[["data"]])
    d <- dplyr::mutate(d, text = cleaner(.data[["text"]]))
    d <- tidyr::nest(d, !! nested)
  }
  d
}

#' Testing function for Star Trek books
#'
#' A testing function for reading Star Trek epub files with \code{epubr::epub} or \code{st_epub}.
#'
#' @param file character.
#' @param details logical.
#' @param add_tail logical.
#' @param default_reader logical.
#'
#' @return returns invisibly
#' @export
#'
#' @examples
#' \dontrun{st_epub_test(file)}
st_epub_test <- function(file, details = FALSE, add_tail = FALSE, default_reader = FALSE){
  read <- if(default_reader) epubr::epub else st_epub
  x <- read(file)
  if(!all(c("title", "creator") %in% names(x))) warning("`title` and/or `author` missing.")
  if("nchap" %in% names(x) && x$nchap == 0) warning("`nchap` is zero.")
  if(nrow(x$data[[1]]) < 5) warning("Content data frame has fewer than five rows.")
  if(details){
    print(x)
    print(x$data[[1]])
    if(add_tail) print(utils::tail(x$data[[1]]))
  }
  cat("Checks completed. ---- ", x$title, "\n")
  invisible()
}
