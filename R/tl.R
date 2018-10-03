tl_clean_text <- function(x, fix = c(151, 300)){
  x <- gsub("“|”", "\"", x)
  x <- gsub("\u2014", "-", x)
  x <- gsub("\u2018|\u2019", "'", x)
  x <- gsub("\u201C|\u201D", "\"", x)
  x <- gsub("P&C", "PAC", x)
  x <- gsub("TFTCT", "CT", x)
  idx <- sapply(fix, function(i) grep(paste0("^      ", i, " Story "), x))
  x[idx] <- gsub("      ", "    ", x[idx])
  x
}

tl_line_range <- function(x, tl_start = "     5 BILLION YEARS AGO",
                      tl_end = "    493 Framing story is set 2, 000 years in an alternate future of TNG."){
  c(which(x == tl_start), which(x == tl_end))
}

tl_line_seq <- function(x){
  r <- tl_line_range(x)
  seq(r[1], r[2])
}

tl_filter_lines <- function(x){
  x[setdiff(tl_line_seq(x), tl_footnote_index(x))]
}

tl_footnote_index <- function(x){
  grep("^    \\d+\\s(\\w|\"|\\.)", x)
}

tl_footnotes <- function(x, ibid = FALSE){
  idx <- tl_footnote_index(x)
  x <- x[idx]
  x <- trimws(x)
  if(!ibid){
    idx <- grep("^\\d+ Ibid", x)
    while(length(idx)){
      i1 <- gsub("(^\\d+)(.*)", "\\1", x[idx])
      i2 <- gsub("(^\\d+)(.*)", "\\2", x[idx - 1])
      x[idx] <- paste0(i1, i2)
      idx <- grep("^\\d+ Ibid", x)
    }
  }
  dplyr::data_frame(id = as.integer(gsub("(^\\d+).*", "\\1", x)), text = gsub("^\\d+ (.*)", "\\1", x)) %>%
    dplyr::distinct()
}
