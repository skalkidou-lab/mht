#!/usr/bin/env Rscript
#
# Make the two 2026-09-22 workbooks from their predecessors by surgical XML
# edit. Run it from the package root:
#
#     Rscript dev/codebooks-20260922/make-codebooks-20260922.R
#
# - product_table_20260922.xlsx is product_table_20260902.xlsx with 5 cells of
#   one row changed. Estradiol Valerate becomes A7 and excludes the person.
# - dataDictionary20260922.xlsx is dataDictionary20260828.xlsx without the 14
#   post_grouping rules that include A7. The 3 rules that exclude A7 lose that
#   one cell.
#
# The script uses no spreadsheet writer. It edits the XML text of the members
# that change. It copies every other zip member byte for byte, with its local
# header and its compressed data. Two runs write identical files.
# tests/testthat/test-v20260922-codebooks.R checks the result.

# ZIP ====

# The text of one zip member, with its bytes unchanged.
read_member <- function(path, member) {
  dir <- tempfile()
  on.exit(unlink(dir, recursive = TRUE))
  utils::unzip(path, files = member, exdir = dir)
  f <- file.path(dir, member)
  return(rawToChar(readBin(f, "raw", file.size(f))))
}

# An unsigned little-endian field of `size` bytes at 0-based offset `at`.
get_field <- function(z, at, size) {
  return(sum(as.numeric(z[at + seq_len(size)]) * 256^(seq_len(size) - 1)))
}
put_field <- function(x, size) {
  return(as.raw((x %/% 256^(seq_len(size) - 1)) %% 256))
}

# Deflate `bytes` for a zip member. R's gzip writer does the work. With its
# flag byte at 0, a gzip file is a 10-byte header, the raw deflate data, the
# CRC-32 and the length.
deflate <- function(bytes) {
  gz <- tempfile(fileext = ".gz")
  on.exit(unlink(gz))
  con <- gzfile(gz, "wb")
  writeBin(bytes, con)
  close(con)
  g <- readBin(gz, "raw", file.size(gz))
  stopifnot(identical(g[1:4], as.raw(c(0x1f, 0x8b, 0x08, 0x00))))
  n <- length(g)
  return(list(data = g[11:(n - 8)], crc = g[(n - 7):(n - 4)]))
}

# Write `to` as a copy of the archive `from`. Each member named in `members`
# gets the text given there. Every other member keeps its local header and its
# compressed data byte for byte. The member order, the time stamps and the
# attributes stay those of `from`. Both predecessors have no archive comment,
# no data descriptor and no zip64 record.
rezip <- function(from, to, members) {
  z <- readBin(from, "raw", file.size(from))
  eocd <- length(z) - 22
  stopifnot(get_field(z, eocd, 4) == 0x06054b50)
  at <- get_field(z, eocd + 16, 4)
  records <- list()
  directory <- list()
  seen <- character(0)
  offset <- 0
  for (i in seq_len(get_field(z, eocd + 10, 2))) {
    name_len <- get_field(z, at + 28, 2)
    entry_len <- 46 +
      name_len +
      get_field(z, at + 30, 2) +
      get_field(z, at + 32, 2)
    entry <- z[at + seq_len(entry_len)]
    name <- rawToChar(entry[46 + seq_len(name_len)])
    local <- get_field(z, at + 42, 4)
    header_len <- 30 +
      get_field(z, local + 26, 2) +
      get_field(z, local + 28, 2)
    header <- z[local + seq_len(header_len)]
    data <- z[local + header_len + seq_len(get_field(z, at + 20, 4))]
    if (name %in% names(members)) {
      bytes <- charToRaw(members[[name]])
      d <- deflate(bytes)
      data <- d$data
      sizes <- c(d$crc, put_field(length(data), 4), put_field(length(bytes), 4))
      header[15:26] <- sizes
      entry[17:28] <- sizes
    }
    entry[43:46] <- put_field(offset, 4)
    records[[i]] <- c(header, data)
    directory[[i]] <- entry
    seen <- c(seen, name)
    offset <- offset + header_len + length(data)
    at <- at + entry_len
  }
  stopifnot(all(names(members) %in% seen))
  directory <- unlist(directory)
  end <- z[eocd + 1:22]
  end[13:16] <- put_field(length(directory), 4)
  end[17:20] <- put_field(offset, 4)
  writeBin(c(unlist(records), directory, end), to)
  return(invisible(to))
}

# XML TEXT ====

# The text of every shared string, in index order. Shared string i of the
# sheet XML is element i + 1.
sst_text <- function(sst) {
  si <- regmatches(
    sst,
    gregexpr("<si>.*?</si>", sst, perl = TRUE, useBytes = TRUE)
  )[[1]]
  return(gsub("<[^>]+>", "", si, useBytes = TRUE))
}

# The 0-based index of the one shared string with this text.
sst_index <- function(text, strings) {
  i <- which(strings == text)
  stopifnot(length(i) == 1L)
  return(i - 1L)
}

# Replace `old` in `x`. Stop unless `old` occurs exactly once.
replace_once <- function(x, old, new) {
  hits <- gregexpr(old, x, fixed = TRUE, useBytes = TRUE)[[1]]
  stopifnot(sum(hits > 0) == 1L)
  return(sub(old, new, x, fixed = TRUE, useBytes = TRUE))
}

# Delete whole rows of a sheet. Every later row, and each cell in it, moves up
# by the number of rows deleted above it. The dimension shrinks to match. The
# kept rows keep their content and order, blank separator rows included.
delete_rows <- function(x, delete) {
  m <- gregexpr("<row [^>]*>.*?</row>", x, perl = TRUE, useBytes = TRUE)
  rows <- regmatches(x, m)[[1]]
  r <- as.integer(sub('^<row r="([0-9]+)".*$', "\\1", rows, useBytes = TRUE))
  gone <- r %in% delete
  to <- r - cumsum(gone)
  for (i in which(!gone & to != r)) {
    rows[i] <- sub(
      sprintf('^<row r="%d"', r[i]),
      sprintf('<row r="%d"', to[i]),
      rows[i],
      useBytes = TRUE
    )
    rows[i] <- gsub(
      sprintf('(<c r="[A-Z]+)%d"', r[i]),
      sprintf('\\1%d"', to[i]),
      rows[i],
      useBytes = TRUE
    )
  }
  rows[gone] <- ""
  regmatches(x, m) <- list(rows)
  return(replace_once(
    x,
    sprintf('<dimension ref="A1:AI%d"/>', max(r)),
    sprintf('<dimension ref="A1:AI%d"/>', max(to))
  ))
}

# PRODUCT TABLE ====

pt_from <- "inst/2023-mht/product_table_20260902.xlsx"
pt_to <- "inst/2023-mht/product_table_20260922.xlsx"

# The new source value is the one new shared string. It goes last, so no index
# that a cell holds today moves. This file writes count equal to uniqueCount,
# and both grow by 1.
sst <- read_member(pt_from, "xl/sharedStrings.xml")
n <- length(sst_text(sst))
sst <- replace_once(
  sst,
  sprintf('count="%d" uniqueCount="%d"', n, n),
  sprintf('count="%d" uniqueCount="%d"', n + 1L, n + 1L)
)
sst <- replace_once(
  sst,
  "</sst>",
  '<si><t xml:space="preserve">decision 2026-09-22</t></si></sst>'
)
strings <- sst_text(sst)
cell <- function(ref, text) {
  i <- sst_index(text, strings)
  return(sprintf('<c r="%s" t="s"><v>%d</v></c>', ref, i))
}

# Sheet G is xl/worksheets/sheet1.xml, and Estradiol Valerate is its row 60.
# Columns F to J are classification, classification_meaning,
# exclude_entire_person, exclusion_reason and source.
g <- read_member(pt_from, "xl/worksheets/sheet1.xml")
stopifnot(grepl(cell("B60", "Estradiol Valerate"), g, fixed = TRUE))
ev_before <- paste0(
  cell("F60", "notmht"),
  cell("G60", "not menopausal hormone therapy"),
  cell("H60", "FALSE"),
  '<c r="I60"/>',
  cell("J60", "decision20260826")
)
ev_after <- paste0(
  cell("F60", "A7"),
  cell("G60", "oestrogens, injection"),
  cell("H60", "TRUE"),
  cell("I60", "possible gender-affirming therapy"),
  cell("J60", "decision 2026-09-22")
)
g <- replace_once(g, ev_before, ev_after)

rezip(
  pt_from,
  pt_to,
  list("xl/sharedStrings.xml" = sst, "xl/worksheets/sheet1.xml" = g)
)

# DICTIONARY ====

dd_from <- "inst/2023-mht/dataDictionary20260828.xlsx"
dd_to <- "inst/2023-mht/dataDictionary20260922.xlsx"

# post_grouping is xl/worksheets/sheet2.xml. Row numbers are spreadsheet rows,
# with the header as row 1. Each row in DELETE is a rule with A7 in includes1.
# Each cell in CLEAR is doesnotinclude4 of a local_or_none_mht rule, and holds
# A7. Shared string A7 stays, because MHT_groups uses it. sharedStrings.xml is
# copied unchanged, so its count attribute still counts the removed cells.
DELETE <- c(8, 38, 47, 61, 65, 69, 73, 77, 81, 85, 89, 93, 99, 103)
CLEAR <- c("I2", "I3", "I4")

a7 <- sst_index("A7", sst_text(read_member(dd_from, "xl/sharedStrings.xml")))
x <- read_member(dd_from, "xl/worksheets/sheet2.xml")

# The rows in DELETE are all the rules with A7 in includes1 or includes2.
a7_includes <- regmatches(
  x,
  gregexpr(
    sprintf('<c r="[CD][0-9]+" s="[0-9]+" t="s"><v>%d</v></c>', a7),
    x,
    useBytes = TRUE
  )
)[[1]]
stopifnot(identical(
  as.integer(sub('^<c r="[CD]([0-9]+)".*$', "\\1", a7_includes)),
  as.integer(DELETE)
))

for (ref in CLEAR) {
  x <- replace_once(
    x,
    sprintf('<c r="%s" s="28" t="s"><v>%d</v></c>', ref, a7),
    sprintf('<c r="%s" s="28"/>', ref)
  )
}
x <- delete_rows(x, DELETE)

rezip(dd_from, dd_to, list("xl/worksheets/sheet2.xml" = x))
