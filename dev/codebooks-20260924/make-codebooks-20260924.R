#!/usr/bin/env Rscript
#
# Make the two 2026-09-24 workbooks from the 2026-09-22 pair by surgical XML
# edit. Run it from the package root:
#
#     Rscript dev/codebooks-20260924/make-codebooks-20260924.R
#
# - product_table_20260924.xlsx is product_table_20260922.xlsx with one note
#   cell reworded. Only xl/sharedStrings.xml changes.
# - dataDictionary20260924.xlsx is dataDictionary20260922.xlsx with the 47
#   approach 4 rules appended to the post_grouping sheet. The rules of
#   approaches 1 to 3 do not change.
#
# The script uses no spreadsheet writer. In each workbook it edits the XML text
# of the one member that changes. It copies every other zip member byte for
# byte, with its local header and its compressed data. The new dictionary cells
# hold inline strings, so the dictionary's xl/sharedStrings.xml does not change.
# Two runs write identical files. tests/testthat/test-v20260924-approach4.R
# checks the result.

# ZIP ====

# The zip functions are copies of those in
# dev/codebooks-20260922/make-codebooks-20260922.R.

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
# attributes stay those of `from`. The predecessor has no archive comment, no
# data descriptor and no zip64 record.
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

# Replace `old` in `x`. Stop unless `old` occurs exactly once.
replace_once <- function(x, old, new) {
  hits <- gregexpr(old, x, fixed = TRUE, useBytes = TRUE)[[1]]
  stopifnot(sum(hits > 0) == 1L)
  return(sub(old, new, x, fixed = TRUE, useBytes = TRUE))
}

# RULES ====

# One approach 4 rule. The categories in `...` fill includes1 and includes2.
# `excludes` fills doesnotinclude1 onward. A rule with no includes declares its
# variable and lights nothing.
rule <- function(variable, ..., excludes = character(0)) {
  includes <- as.character(c(...))
  stopifnot(length(includes) <= 2L, length(excludes) <= 30L)
  return(list(variable = variable, includes = includes, excludes = excludes))
}

# The doesnotinclude cells of each approach 3 estrogen_only rule, in sheet
# order. The script checks them against the old sheet before it writes.
ESTROGEN_ONLY_EXCLUDES <- c(
  "C1", "C2", "C3", "C4", "D1", "D2", "E1",
  "B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B9", "B10", "B11", "B12",
  "D3", "I1", "I2", "C5"
)

# The 47 approach 4 rules, one row each, in the order of the new rows.
RULES <- list(
  rule("oral_estrogen_progesterone", "B4"),
  rule("oral_estrogen_progesterone", "B11"),
  rule("oral_estrogen_progesterone", "A2", "C1"),
  rule("oral_estrogen_progesterone", "A2", "C5"),
  rule("oral_estrogen_progesterone", "A6", "C1"),
  rule("oral_estrogen_progesterone", "A6", "C5"),
  rule("oral_estrogen_other_progestogen", "B2"),
  rule("oral_estrogen_other_progestogen", "B3"),
  rule("oral_estrogen_other_progestogen", "B5"),
  rule("oral_estrogen_other_progestogen", "B6"),
  rule("oral_estrogen_other_progestogen", "B8"),
  rule("oral_estrogen_other_progestogen", "B9"),
  rule("oral_estrogen_other_progestogen", "B10"),
  rule("oral_estrogen_other_progestogen", "B12"),
  rule("oral_estrogen_other_progestogen", "A2", "C3"),
  rule("oral_estrogen_other_progestogen", "A2", "C4"),
  rule("oral_estrogen_other_progestogen", "A2", "D1"),
  rule("oral_estrogen_other_progestogen", "A2", "D2"),
  rule("oral_estrogen_other_progestogen", "A2", "D3"),
  rule("oral_estrogen_other_progestogen", "A2", "E1"),
  rule("oral_estrogen_other_progestogen", "A2", "I1"),
  rule("oral_estrogen_other_progestogen", "A2", "I2"),
  rule("oral_estrogen_other_progestogen", "A6", "C3"),
  rule("oral_estrogen_other_progestogen", "A6", "C4"),
  rule("oral_estrogen_other_progestogen", "A6", "D1"),
  rule("oral_estrogen_other_progestogen", "A6", "D2"),
  rule("oral_estrogen_other_progestogen", "A6", "D3"),
  rule("oral_estrogen_other_progestogen", "A6", "E1"),
  rule("oral_estrogen_other_progestogen", "A6", "I1"),
  rule("oral_estrogen_other_progestogen", "A6", "I2"),
  rule("transdermal_estrogen_progesterone", "A1", "C1"),
  rule("transdermal_estrogen_progesterone", "A1", "C5"),
  rule("transdermal_estrogen_other_progestogen", "B1"),
  rule("transdermal_estrogen_other_progestogen", "B7"),
  rule("transdermal_estrogen_other_progestogen", "A1", "C3"),
  rule("transdermal_estrogen_other_progestogen", "A1", "C4"),
  rule("transdermal_estrogen_other_progestogen", "A1", "D1"),
  rule("transdermal_estrogen_other_progestogen", "A1", "D2"),
  rule("transdermal_estrogen_other_progestogen", "A1", "D3"),
  rule("transdermal_estrogen_other_progestogen", "A1", "E1"),
  rule("transdermal_estrogen_other_progestogen", "A1", "I1"),
  rule("transdermal_estrogen_other_progestogen", "A1", "I2"),
  rule("estrogen_only", "A1", excludes = ESTROGEN_ONLY_EXCLUDES),
  rule("estrogen_only", "A2", excludes = ESTROGEN_ONLY_EXCLUDES),
  rule("estrogen_only", "A6", excludes = ESTROGEN_ONLY_EXCLUDES),
  rule("tibolone", "F1"),
  rule("local_or_none_mht")
)
stopifnot(length(RULES) == 47L)

# SHEET XML ====

# Spreadsheet column letters, A to AZ, by column number.
COLUMNS <- c(LETTERS, paste0("A", LETTERS))

# The style that each column declares in the <cols> element, by column number.
# A new cell takes the style of its column, as a cell typed into the sheet does.
column_styles <- function(x) {
  cols <- regmatches(x, gregexpr("<col [^>]*/>", x, useBytes = TRUE))[[1]]
  stopifnot(length(cols) > 0L, grepl(' style="', cols, fixed = TRUE))
  attribute <- function(name) {
    pattern <- sprintf('^.* %s="([0-9]+)".*$', name)
    return(as.integer(sub(pattern, "\\1", cols, useBytes = TRUE)))
  }
  from <- attribute("min")
  to <- attribute("max")
  style <- attribute("style")
  out <- integer(0)
  for (i in seq_along(cols)) {
    out[from[i]:to[i]] <- style[i]
  }
  return(out)
}

# The XML of one post_grouping row for rule `r` at spreadsheet row `row`.
# Column A holds the variable and column B the approach, as a number like the
# older rows. Columns C and D hold the includes, and column F onward holds the
# doesnotinclude cells. Every text cell is an inline string.
rule_row <- function(row, r, approach, style) {
  col <- c(1L, 2L, 2L + seq_along(r$includes), 5L + seq_along(r$excludes))
  text <- c(r$variable, NA, r$includes, r$excludes)
  stopifnot(grepl("^[A-Za-z0-9_]+$", text[-2]), !is.na(style[col]))
  ref <- paste0(COLUMNS[col], row)
  cells <- sprintf(
    '<c r="%s" s="%d" t="inlineStr"><is><t>%s</t></is></c>',
    ref,
    style[col],
    text
  )
  cells[2] <- sprintf(
    '<c r="%s" s="%d"><v>%d</v></c>',
    ref[2],
    style[col[2]],
    approach
  )
  return(sprintf(
    '<row r="%d" spans="1:%d" x14ac:dyDescent="0.35">%s</row>',
    row,
    max(col),
    paste(cells, collapse = "")
  ))
}

# PRODUCT TABLE ====

pt_from <- "inst/2023-mht/product_table_20260922.xlsx"
pt_to <- "inst/2023-mht/product_table_20260924.xlsx"

# One note cell names the person who raised a question. The new table words it
# as "a question raised on 2026-09-01". The pattern finds the cell by the text
# around the name, so the name is not in this script.
NOTE_OLD <- paste0(
  "this closes the question [A-Za-z]+ raised on 2026-09-01, ",
  "whether to count compounded progesterone"
)
NOTE_NEW <- paste0(
  "this closes a question raised on 2026-09-01, ",
  "whether to count compounded progesterone"
)

# The shared strings, as <si> elements, and the count attributes of <sst>.
shared_strings <- function(x) {
  si <- regmatches(x, gregexpr("<si>.*?</si>", x, perl = TRUE, useBytes = TRUE))
  sst <- regmatches(x, regexpr("<sst [^>]*>", x, useBytes = TRUE))
  return(list(si = si[[1]], sst = sst))
}

s <- read_member(pt_from, "xl/sharedStrings.xml")
hits <- gregexpr(NOTE_OLD, s, perl = TRUE, useBytes = TRUE)[[1]]
stopifnot(sum(hits > 0) == 1L)
s_new <- sub(NOTE_OLD, NOTE_NEW, s, perl = TRUE, useBytes = TRUE)

# The edit keeps the number of strings and the count and uniqueCount
# attributes, and it changes exactly one string.
before <- shared_strings(s)
after <- shared_strings(s_new)
stopifnot(
  length(after$si) == length(before$si),
  identical(after$sst, before$sst),
  sum(after$si != before$si) == 1L,
  !grepl(NOTE_OLD, s_new, perl = TRUE, useBytes = TRUE)
)

rezip(pt_from, pt_to, list("xl/sharedStrings.xml" = s_new))
stopifnot(identical(read_member(pt_to, "xl/sharedStrings.xml"), s_new))

# DICTIONARY ====

dd_from <- "inst/2023-mht/dataDictionary20260922.xlsx"
dd_to <- "inst/2023-mht/dataDictionary20260924.xlsx"

# Each approach 3 estrogen_only rule of the old sheet carries exactly the cells
# of ESTROGEN_ONLY_EXCLUDES.
old <- readxl::read_excel(
  dd_from,
  sheet = "post_grouping",
  col_types = "text",
  .name_repair = "unique_quiet"
)
old_only <- old[
  old$approach %in% "3" & old$variable %in% "estrogen_only",
  grep("^doesnotinclude", names(old))
]
stopifnot(nrow(old_only) == 3L)
for (i in seq_len(nrow(old_only))) {
  v <- unlist(old_only[i, ], use.names = FALSE)
  stopifnot(identical(v[!is.na(v)], ESTROGEN_ONLY_EXCLUDES))
}

# post_grouping is xl/worksheets/sheet2.xml. Its last row, 92, is blank. It
# separates approach 3 from approach 4, as rows 21 and 41 separate the earlier
# approaches. The new rules take rows 93 to 139.
x <- read_member(dd_from, "xl/worksheets/sheet2.xml")
rows <- regmatches(
  x,
  gregexpr("<row [^>]*>.*?</row>", x, perl = TRUE, useBytes = TRUE)
)[[1]]
number <- as.integer(sub('^<row r="([0-9]+)".*$', "\\1", rows, useBytes = TRUE))
last <- max(number)
stopifnot(
  last == 92L,
  number[length(rows)] == last,
  !grepl("<v>", rows[length(rows)], fixed = TRUE)
)

style <- column_styles(x)
new_rows <- vapply(
  seq_along(RULES),
  function(i) {
    return(rule_row(last + i, RULES[[i]], approach = 4L, style = style))
  },
  character(1)
)
x <- replace_once(
  x,
  "</sheetData>",
  paste0(paste(new_rows, collapse = ""), "</sheetData>")
)
x <- replace_once(
  x,
  sprintf('<dimension ref="A1:AI%d"/>', last),
  sprintf('<dimension ref="A1:AI%d"/>', last + length(RULES))
)

rezip(dd_from, dd_to, list("xl/worksheets/sheet2.xml" = x))
