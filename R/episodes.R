#' Episode Script Information
#'
#' Star Trek episode and movie transcript metadata.
#'
#' @return a data frame
#' @export
#'
#' @examples
#' \dontrun{st_script_info()}
st_script_info <- function(){
  url <- "https://scifi.media/star-trek/transcripts/"
  x <- xml2::read_html(url) %>% rvest::html_nodes(".fusion-text p") %>% rvest::html_children()
  urls <- rvest::html_attr(x, "href")
  txt <- rvest::html_text(x)

  .season_sub <- function(x){
    name <- c("One", "Two", "Three", "Four", "Five", "Six", "Seven")
    suppressWarnings(
      as.integer(paste0(sapply(1:7, function(i){
        if(grepl(name[i], x)) gsub(paste0(".*", name[i]), i, x) else ""
      }
      ), collapse = ""))
    )
  }
  season <- ifelse(grepl("^Season .*", txt), sapply(txt, .season_sub), NA)

  number <- as.integer(ifelse(grepl("^Episode \\d.*", txt), gsub("^Episode (\\d+).*", "\\1", txt), NA))

  .rs <- function(x) gsub("(p|P)art I$", "Part 1", romans_sub(x))
  title <- ifelse(grepl("^Episode \\d.*(:|.) ", txt), gsub("^Episode \\d+(:|.) (.*)", "\\2", .rs(txt)),
                  ifelse(grepl("^Star Trek", txt), .rs(txt), NA)) %>% .clean_script_titles()

  fmt <- rep("episode", length(title))
  idx <- 1:(which(is.na(title))[1] - 1)
  fmt[idx] <- "movie"
  number[idx] <- idx
  series <- rep(NA, length(title))
  series_abb <- c("TOS", "TNG", "DS9", "VOY", "ENT")
  series[which(season == 1)] <- series_abb[c(2:5, 1)]
  series[1:6] <- "TOS"
  series[7:10] <- "TNG"
  series <- factor(series, levels = series_abb)

  d <- tibble::tibble(format = fmt, series = series, season = season,
                      number = as.integer(number), title = title, url = urls) %>%
    tidyr::fill(.data[["series"]], .data[["season"]]) %>%
    dplyr::filter(!is.na(.data[["number"]])) %>%
    dplyr::group_by(.data[["format"]], .data[["series"]]) %>%
    tidyr::complete(number = as.integer(tidyr::full_seq(.data[["number"]], 1))) %>% dplyr::ungroup()
  d$title[d$series == "VOY" & d$number == 15] <- "Jetrel"
  d <- dplyr::arrange(d, .data[["format"]], .data[["series"]], .data[["number"]]) %>%
    tidyr::fill(.data[["season"]]) %>%
    dplyr::filter(!(is.na(.data[["url"]]) & .data[["number"]] %in% c(2, 74))) %>%
    dplyr::mutate(series = factor(.data[["series"]], levels = c(series_abb, "TAS"))) %>%
    dplyr::filter(!.data[["title"]] %in%
                    paste0(c("Caretaker", "Dark Frontier", "Flesh and Blood", "Endgame"), ", Part 2"))

  # Second pass for completion
  series_abb2 <- c("StarTrek", "NextGen", "DS9", "Voyager", "Enterprise")
  f <- function(abb, abb2){
    if(abb == "VOY"){
      url <- file.path("http://www.chakoteya.net", abb2, "episode_listing.htm")
      x <- xml2::read_html(url) %>% rvest::html_nodes("div table")
    } else {
      url <- file.path("http://www.chakoteya.net", abb2, "episodes.htm")
      x <- xml2::read_html(url) %>% rvest::html_nodes("td table")
      idx <- which(sapply(x, function(x) length(rvest::html_nodes(x, "tr")) > 10))
      x <- x[idx]
    }
    d <- rvest::html_table(x, TRUE)
    d <- purrr::map2(d, seq_along(d), ~{
      x <- dplyr::mutate(.x, Production = as.character(.data[["Production"]]),
                         series = factor(abb, levels = c(series_abb, "TAS")), season = .y)
      names(x) <- gsub("\r\n", " ", names(x))
      x
    })
    if(abb2 == "StarTrek") d[[4]] <- dplyr::mutate(
      d[[4]], series = factor("TAS", levels = c(series_abb, "TAS")), season = c(rep(1, 16), rep(2, 6)))
    d <- dplyr::bind_rows(d) %>% tibble::as_tibble() %>%
      dplyr::rename(title = .data[["Episode Name"]], production = .data[["Production"]]) %>%
      dplyr::mutate(format = "episode", title = .clean_script_titles(.rs(.data[["title"]])),
                    production = as.integer(gsub("(\\d+).*", "\\1", .data[["production"]])),
                    airdate = ifelse(.data[["Airdate"]] == "unaired",
                                     NA, as.character(lubridate::dmy(
                                       ifelse(grepl("^(\\d+)(, | )(\\d+)", .data[["Airdate"]]),
                                              gsub("^(\\d+)(, | )(\\d+)(.*)", "\\1\\4",
                                                   .data[["Airdate"]]), .data[["Airdate"]]),
                                       quiet = TRUE))))
    for(i in 1:nrow(d)) if(!is.na(d$airdate[i])) next else
      d$airdate[i] <- as.character(lubridate::mdy(d$Airdate[i], quiet = TRUE))
    d$Airdate <- NULL
    urls <- rvest::html_nodes(x, "a") %>% rvest::html_attr("href")
    urls <- file.path(dirname(url), urls)
    dplyr::mutate(d, url2 = urls)
  }

  d2 <- purrr::map2_dfr(series_abb, series_abb2, f)
  d2 <- dplyr::mutate(d2, title = dplyr::case_when(
    .data[["title"]] == "For the World is Hollow..." ~ "For the World is Hollow and I Have Touched the Sky",
    .data[["title"]] == "The Day of the Dove" ~ "Day of the Dove",
    .data[["title"]] == "Operation: Annihilate!" ~ "Operation - Annihilate!",
    TRUE ~ .data[["title"]]
  ))
  d <- dplyr::full_join(d, d2, by = c("format", "series", "season", "title"))
  d$number[d$series == "TAS"] <- 1:22
  dplyr::arrange(d, .data[["format"]], .data[["series"]], .data[["number"]]) %>%
    dplyr::mutate(series = as.character(.data[["series"]]), season = as.integer(.data[["season"]])) %>%
    dplyr::select(c(1, 2, 4, 3, 5, 7, 8, 6, 9))
}

#' Download episode scripts
#'
#' Download episode scripts into a nested tidy data frame.
#'
#' Downloads almost entirely come from \code{scifi.media}, but when unavailable are pulled from \code{chakoteya.net}.
#'
#' @param download_dir download directory.
#' @param keep logical, if \code{FALSE} (default) then downloaded files are removed after processing.
#' @param overwrite logical, if \code{FALSE} (default) then no downloading occurs for any file already present in \code{download_dir}.
#'
#' @return a data frame; may write files to disk.
#' @export
#'
#' @examples
#' \dontrun{x <- st_script_download("data-raw/episode_scripts", TRUE)}
st_script_download <- function(download_dir = tempdir(), keep = FALSE, overwrite = FALSE){
  d <- st_script_info() %>%
    dplyr::mutate(
      file = ifelse(.data[["format"]] == "episode",
                    paste0(.data[["series"]], "-s", .data[["season"]], "e", .data[["number"]], "-",
                           gsub(":|\\?", "", gsub(" ", "_", .data[["title"]])), ".txt"),
                    paste0(.data[["series"]], "-mov", .data[["number"]], "-",
                           gsub(":|\\?", "", gsub(" ", "_", .data[["title"]])), ".txt"))
    )

  get_script <- function(i){
    file <- file.path(download_dir, d$file[i])
    if(!file.exists(file) | overwrite){
      cat("Downloading ", basename(file), "...\n", sep = "")
      z <- tryCatch(utils::download.file(d$url[i], file, quite = TRUE), error = function(e) "fail")
      if(z == "fail"){
        x <- xml2::read_html(d$url2[i]) %>% rvest::html_text()
        x <- gsub("\r\n", "\n", gsub("\r\n\r\n", "\r\n", x))
        x <- gsub("Stardate\\:\n", "\nStardate\\: ", x)
        x <- strsplit(x, "\n")[[1]]
        x <- x[-1]
        x <- trimws(x)
        x <- gsub("^\\[", "\n\\[", x)
        idx <- grep("^Stardate\\: ", x)
        if(length(idx)){
          idx <- idx[1]
          x <- c(paste("Title:", d$title[i]), x[idx:length(x)])
        }
        x <- paste0(paste0(x, collapse = "\n"), "\n")
        x <- gsub("\\]\n", "\\]\n\n", x)
        x <- gsub("([A-Z]+)\\:\n", "\\1: ", x)
        sink(file)
        cat(x)
        sink()
      }
    }
    cat("Reading file: ", file, "...\n", sep = "")
    txt <- readLines(file, warn = FALSE)
    if(!keep) unlink(file, recursive = TRUE, force = TRUE)
    txt
  }

  dplyr::mutate(d, file = NULL, text = purrr::map(1:nrow(d), get_script))
}

.clean_script_titles <- function(x){
  x <- gsub("\r\n", " ", x)
  x <- gsub("\u2010|\u2011|\u2012|\u2013|\u2014|\u2015", "-", x)
  x <- gsub("\u2018|\u2019", "'", x)
  x <- gsub("\u201C|\u201D", "\"", x)
  x <- gsub("\u2026", "...", x)
  x <- gsub("\u00c3|\u00e1", "a", x)
  x <- gsub("\u00e8|\u00e9", "e", x)
  x <- gsub("^\"|\"$", "", x)
  x <- tools::toTitleCase(trimws(x))
  x <- gsub("\\((\\d+)\\)$", ", Part \\1", x)
  x <- gsub("(, |,| )(Part|Pt) (\\d)$", ", Part \\3", x)
  x <- gsub(",,", ",", x)
  x <- gsub(" ,", ",", x)
  x <- gsub("Part One", "Part 1", x)
  x <- gsub("Part Two", "Part 2", x)
  # TNG subs
  x <- gsub("QPid", "Qpid", x)
  x <- gsub("Pre-Emptive", "Preemptive", x)
  x <- gsub("True-Q", "True Q", x)
  x <- gsub("Shades of Grey", "Shades of Gray", x)
  x <- gsub("Who Watches the Watchers$", "Who Watches the Watchers?", x)
  x <- gsub("The Best of Both Worlds", "Best of Both Worlds", x)
  x <- gsub("Honour", "Honor", x)
  x <- gsub("&", "and", x)
  x <- gsub("^Redemption$", "Redemption, Part 1", x)

  # DS9 subs
  x <- gsub("The Sons of Mogh", "Sons of Mogh", x)
  x <- gsub("The Bar Association", "Bar Association", x)
  x <- gsub("Hard Times", "Hard Time", x)
  x <- gsub("Looking for Par'Mach...", "Looking for par'Mach in All the Wrong Places", x)
  x <- gsub("Nor the Battle to the Strong", "...nor the Battle to the Strong", x)
  x <- gsub("Let He Who is Without Sin...", "Let He Who is Without Sin", x)
  x <- gsub("Doctor Bashir, I Presume$", "Doctor Bashir, I Presume?", x)
  x <- gsub("Favour the Bold", "Favor the Bold", x)
  x <- gsub("The Sacrifice of Angels", "Sacrifice of Angels", x)
  x <- gsub("You are Cordially Invited...", "You are Cordially Invited", x)
  x <- gsub("Who Mourns for Morn$", "Who Mourns for Morn?", x)
  x <- gsub("Treachery, Faith and the Great River", "Treachery, Faith, and the Great River", x)
  x <- gsub("AR558", "AR-558", x)
  x <- gsub("Badda-Bing, Badda-Bang", "Badda Bing Badda Bang", x)
  x <- gsub("Till Death Do Us Part", "Til Death Do Us Part", x)

  # VOY subs
  x <- gsub("Caretaker, Part 1", "Caretaker", x)
  x <- gsub("Future's End$", "Future's End, Part 1", x)
  x <- gsub("^Darkling", "The Darkling", x)
  x <- gsub("^The Killing Game \\(I\\)|^The Killing Game$", "The Killing Game, Part 1", x)
  x <- gsub("Year of Hell$", "Year of Hell, Part 1", x)
  x <- gsub("Bride of Chaotica!", "Bride of Chaotica", x)
  x <- gsub("Dark Frontier, Part 1", "Dark Frontier", x)
  x <- gsub("Flesh and Blood, Part 1", "Flesh and Blood", x)
  x <- gsub("Workforce$", "Workforce, Part 1", x)
  x <- gsub("Endgame, Part 1", "Endgame", x)

  # ENT subs
  x <- gsub("Episode 1/2 Broken Bow, Part 1", "Broken Bow", x)
  x <- gsub("Azati Prime, Part 1", "Azati Prime", x)
  x <- gsub("Damage, Part 2", "Damage", x)
  x <- gsub("Borderland, Part 1", "Borderland", x)
  x <- gsub("Cold Station 12, Part 2", "Cold Station 12", x)
  x <- gsub("The Augments, Part 3", "The Augments", x)
  x <- gsub("The Forge, Part 1", "The Forge", x)
  x <- gsub("Awakening, Part 2", "Awakening", x)
  x <- gsub("Kir'Shara, Part 3", "Kir'Shara", x)
  x <- gsub("Babel One, Part 1", "Babel One", x)
  x <- gsub("United, Part 2", "United", x)
  x <- gsub("The Aenar, Part 3", "The Aenar", x)
  x <- gsub("Affliction, Part 1", "Affliction", x)
  x <- gsub("Divergence, Part 2", "Divergence", x)
  x <- gsub("Demons, Part 1", "Demons", x)
  x <- gsub("Terra Prime, Part 2", "Terra Prime", x)
  x <- gsub("Two Day and Two Nights", "Two Days and Two Nights", x)
  x
}

#' Curate nested scripts to data frames
#'
#' Curate nested vectors of script lines to data frames.
#'
#' This currently works well (but imperfectly) for most scripts; it does not yet work for ENT, TAS, or a couple TOS scripts.
#'
#' @param x a vector of lines of script. See example.
#'
#' @return a data frame
#' @export
#'
#' @examples
#' \dontrun{
#' x <- st_script_download("data-raw/episode_scripts", TRUE)
#' system.time(x <- dplyr::mutate(x, text2 = purrr::map(text, st_script_text_df))) # ~6 minutes
#' }
st_script_text_df <- function(x){
  .f <- function(x){
    txt <- strsplit(x, "\n+")[[1]]
    txt <- txt[txt != ""]
    line_num <- gsub("(^\\d+)\\s+.*", "\\1", txt[grepl("^\\d+ ", txt)])
    if(!length(line_num)) line_num <- NA_character_
    prsp <- gsub("^\\d+\\s+", "", txt[grepl("^\\d+ ", txt)])
    if(!length(prsp)) prsp <- NA_character_
    setting <- paste(txt[grepl("^\t[^\t]", txt)], collapse = " ")
    line <- paste(txt[grepl("^\t\t\t[^\t]", txt)], collapse = " ")
    desc <- paste(txt[grepl("^\t\t\t\t[^\t]", txt)], collapse = " ")
    chr <- paste(txt[grepl("^\t\t\t\t\t[^\t]", txt)], collapse = " ")
    tibble::tibble(line_number = line_num, perspective = prsp, setting = setting,
                   line = line, description = desc, character = chr) %>%
      dplyr::mutate_all(list(~trimws(gsub("\t", "", .))))
  }

  .f2 <- function(x){
    txt <- strsplit(x, "\n+")[[1]]
    txt <- txt[!grepl("^Title: |^Stardate: |^Airdate: |Star Trek.*CBS|entertainment purposes only|their respective holders", txt)] # nolint
    prsp_idx <- grep("^\\[.*\\]$", txt)
    desc_idx <- grep("^\\(.*\\)$", txt)
    line_idx <- grep("^[^a-z]+\\: .*", txt)
    txt[prsp_idx] <- paste0("\n_prsp__PRSP_", txt[prsp_idx])
    txt[line_idx] <- paste0("\n_line__LINE_", txt[line_idx])
    txt[desc_idx] <- paste0("\n_DESC_", txt[desc_idx])
    txt <- paste0(txt, collapse = "\n")
    txt <- strsplit(txt, "\n\n")[[1]]
    txt <- gsub("^\n", "", txt)
    txt <- gsub("\n", " ", txt)
    txt <- strsplit(paste0(txt, collapse = ""), "_prsp_|_line_")[[1]]
    txt <- txt[txt != ""]
    prsp_idx <- grep("^_PRSP_\\[", txt)
    txt[prsp_idx] <- paste0(txt[prsp_idx], txt[prsp_idx + 1])
    txt <- txt[-c(prsp_idx + 1)]
    prsp <- ifelse(grepl("^_PRSP_", txt), gsub("_PRSP_\\[|\\]$", "", purrr::map_chr(txt, ~strsplit(.x, "_DESC_|_LINE_")[[1]][1])), NA_character_) # nolint
    line <- sapply(strsplit(txt, "_LINE_"), "[", 2)
    line <- sapply(strsplit(line, "_DESC_"), "[", 1)
    chr <- gsub("(^[^a-z]+)\\: .*", "\\1", line)
    line <- gsub("^[^a-z]+\\: (.*)", "\\1", line)
    desc <- ifelse(grepl("_DESC_", txt), gsub("\\(|\\)$", "", purrr::map_chr(txt, ~strsplit(.x, "_DESC_")[[1]][2])), NA_character_) # nolint
    desc <- gsub("\\)$", "", sapply(strsplit(desc, "_LINE_"), "[", 1))
    tibble::tibble(line_number = seq_along(txt), perspective = prsp, setting = NA_character_,
                   description = desc, character = chr, line = line) %>%
      dplyr::mutate_all(list(~trimws(gsub("\t", "", .)))) %>%
      tidyr::fill(.data[["perspective"]])
  }

  if(grepl("^Title: ", x[1])){
    x <- paste0(x, collapse = "\n")
    .f2(x)
  } else {
    x <- paste0(x, collapse = "\n")
    strsplit(gsub("(\n\\d+)(\\s+)", "\\1__\\1\\2", x), "\n\\d+__")[[1]] %>%
      purrr::map_dfr(~{
        n <- length(strsplit(.x, "\t\t\t\t\t")[[1]])
        if(n <= 2){
          x <- .x
        } else {
          x <- strsplit(gsub("(\t\t\t\t\t)([^\t])", "_\\1_\\1\\2", .x), "_\t\t\t\t\t_")[[1]] %>% unlist()
          x <- c(paste0(x[1], x[2]), x[-c(1:2)])
        }
        purrr::map_df(x, .f) %>% tidyr::fill(.data[["line_number"]], .data[["perspective"]]) %>%
          dplyr::select(c("line_number", "perspective", "setting", "description", "character", "line")) %>%
          dplyr::mutate_all(list(~ifelse(. == "", NA_character_, .))) %>%
          dplyr::filter_at(dplyr::vars(setting, description, character, line), dplyr::any_vars(!is.na(.)))
      })
  }
}
