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
    out <- file.path(out_dir, gsub(in_dir, "", files))
    out <- gsub(paste0(pat, "|", pat2, "|,"), "", out)
    out <- stringr::str_replace(out, "([\\w|' ]+)/\\1", "\\1")
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
    paste0(toupper(substr(x, 1, 1)), substring(x, 2))
  }
  if("title" %in% names(x)) x <- dplyr::mutate(x, title = f(.data[["title"]]))
  if("creator" %in% names(x)) x <- dplyr::mutate(x, creator = f(.data[["creator"]], authors = TRUE))
  if("publisher" %in% names(x)) x <- dplyr::mutate(x, publisher = f(.data[["publisher"]]))
  x
}

st_fix_bantam <- function(x){
  a <- c("Bantam Episodes", "Bantam Novels")
  d <- dplyr::mutate(x, subseries = dplyr::case_when(
    grepl(a[1], .data[["subseries"]]) ~ a[1],
    grepl(a[2], .data[["subseries"]]) ~ a[2],
    TRUE ~ .data[["subseries"]]
  ))
  d
}

st_title_sub <- function(x, keep_roman = FALSE){
  f <- function(x){
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
    x <- gsub("Volume", "Vol", x)
    x <- gsub(": - ", " ", x)
    x <- gsub("s - \\d(\\d\\d) - ", "s \\1: ", x)
    roman_subs <- purrr::map(1:11, ~{
      paste0("(", paste(strsplit(as.character(as.roman(2:12)), "")[[.x]],
                        strsplit(tolower(as.character(as.roman(2:12))), "")[[.x]], sep = "|"), ")", collapse = "")
    })
    p <- if(keep_roman) rep("\\U\\1\\2", 11) else 2:12
    for(i in seq_along(roman_subs)) x <- gsub(paste0(roman_subs[[i]], "(\\s|:|$)"), p[i], x, perl = TRUE)
    x
  }
  if("title" %in% names(x)) x <- dplyr::mutate(x, title = f(.data[["title"]]))
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
#' @param series logical.
#' @param fix_date logical.
#' @param fix_text logical.
#' @param dedication logical.
#' @param hist_note logical.
#'
#' @return a data frame
#' @export
#'
#' @examples
#' \dontrun{st_epub(file)}
st_epub <- function(file, fields = NULL, chapter_pattern = NULL, add_pattern = NULL,
                    cleaner = NULL, drop_sections = NULL, series = TRUE, fix_date = TRUE,
                    fix_text = TRUE, dedication = TRUE, hist_note = TRUE){
  if(is.null(fields)) fields <- .st_fields
  if(is.null(chapter_pattern)) chapter_pattern <- .st_pat
  if(is.null(add_pattern)) add_pattern <- .st_pat_list()
  if(is.null(cleaner)) cleaner <- clean_book_text
  if(is.null(drop_sections)) drop_sections <- .st_sec_drop
  d <- epubr::epub(file, fields = fields, drop_sections = drop_sections, chapter_pattern = chapter_pattern,
                   add_pattern = add_pattern, series = series, dedication = dedication, hist_note = hist_note)
  if(fix_date) d <- st_fix_date(d)
  d <- st_fix_bantam(d)
  if(fix_text){
    d <- st_fix_case(d)
    d <- st_title_sub(d) %>% st_author_sub() %>% st_pub_sub()
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
