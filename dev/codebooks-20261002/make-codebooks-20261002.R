#!/usr/bin/env Rscript
#
# Make the two 2026-10-02 workbooks from the 2026-09-24 pair. Run it from the
# package root:
#
#     Rscript dev/codebooks-20261002/make-codebooks-20261002.R
#
# With --check, the script writes nothing under inst/. It makes the workbooks
# twice, each time in its own temporary package root. It exits non-zero unless
# the two runs write identical files equal to inst/2023-mht/*20261002.xlsx.
#
# - product_table_20261002.xlsx is a byte copy of product_table_20260924.xlsx.
# - dataDictionary20261002.xlsx is dataDictionary20260924.xlsx with the 47
#   approach 5 rules appended to the post_grouping sheet. The rules of
#   approaches 1 to 4 do not change.
#
# The script uses no spreadsheet writer. In the dictionary it edits the XML text
# of the post_grouping sheet only. It copies every other zip member byte for
# byte, with its local header and its compressed data. The new cells hold
# inline strings, so xl/sharedStrings.xml does not change.
# tests/testthat/test-v20261002-codebooks.R checks the result.

# ZIP ====

# The zip functions are copies of those in
# dev/codebooks-20260924/make-codebooks-20260924.R.

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

# One approach 5 rule. The categories in `...` fill includes1 and includes2.
# `excludes` fills doesnotinclude1 onward. A rule with no includes declares its
# variable and lights nothing.
rule <- function(variable, ..., excludes = character(0)) {
  includes <- as.character(c(...))
  stopifnot(length(includes) <= 2L, length(excludes) <= 30L)
  return(list(variable = variable, includes = includes, excludes = excludes))
}

# The doesnotinclude cells of each approach 4 estrogen_only rule, in sheet
# order. The test file checks the approach 5 copies cell by cell.
ESTROGEN_ONLY_EXCLUDES <- c(
  "C1", "C2", "C3", "C4", "D1", "D2", "E1",
  "B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B9", "B10", "B11", "B12",
  "D3", "I1", "I2", "C5"
)

# The 47 approach 5 rules, one row each, in the order of the new rows.
# Approach 5 pools the oestrogen route of approach 4 and gives the hormonal
# IUD (E1) its own level. It has no rule for C2.
RULES <- list(
  rule("estrogen_progesterone", "B4"),
  rule("estrogen_progesterone", "B11"),
  rule("estrogen_progesterone", "A1", "C1"),
  rule("estrogen_progesterone", "A1", "C5"),
  rule("estrogen_progesterone", "A2", "C1"),
  rule("estrogen_progesterone", "A2", "C5"),
  rule("estrogen_progesterone", "A6", "C1"),
  rule("estrogen_progesterone", "A6", "C5"),
  rule("estrogen_other_progestogen", "B1"),
  rule("estrogen_other_progestogen", "B2"),
  rule("estrogen_other_progestogen", "B3"),
  rule("estrogen_other_progestogen", "B5"),
  rule("estrogen_other_progestogen", "B6"),
  rule("estrogen_other_progestogen", "B7"),
  rule("estrogen_other_progestogen", "B8"),
  rule("estrogen_other_progestogen", "B9"),
  rule("estrogen_other_progestogen", "B10"),
  rule("estrogen_other_progestogen", "B12"),
  rule("estrogen_other_progestogen", "A1", "C3"),
  rule("estrogen_other_progestogen", "A1", "C4"),
  rule("estrogen_other_progestogen", "A1", "D1"),
  rule("estrogen_other_progestogen", "A1", "D2"),
  rule("estrogen_other_progestogen", "A1", "D3"),
  rule("estrogen_other_progestogen", "A1", "I1"),
  rule("estrogen_other_progestogen", "A1", "I2"),
  rule("estrogen_other_progestogen", "A2", "C3"),
  rule("estrogen_other_progestogen", "A2", "C4"),
  rule("estrogen_other_progestogen", "A2", "D1"),
  rule("estrogen_other_progestogen", "A2", "D2"),
  rule("estrogen_other_progestogen", "A2", "D3"),
  rule("estrogen_other_progestogen", "A2", "I1"),
  rule("estrogen_other_progestogen", "A2", "I2"),
  rule("estrogen_other_progestogen", "A6", "C3"),
  rule("estrogen_other_progestogen", "A6", "C4"),
  rule("estrogen_other_progestogen", "A6", "D1"),
  rule("estrogen_other_progestogen", "A6", "D2"),
  rule("estrogen_other_progestogen", "A6", "D3"),
  rule("estrogen_other_progestogen", "A6", "I1"),
  rule("estrogen_other_progestogen", "A6", "I2"),
  rule("estrogen_iud", "A1", "E1"),
  rule("estrogen_iud", "A2", "E1"),
  rule("estrogen_iud", "A6", "E1"),
  rule("estrogen_only", "A1", excludes = ESTROGEN_ONLY_EXCLUDES),
  rule("estrogen_only", "A2", excludes = ESTROGEN_ONLY_EXCLUDES),
  rule("estrogen_only", "A6", excludes = ESTROGEN_ONLY_EXCLUDES),
  rule("tibolone", "F1"),
  rule("local_or_none_mht")
)

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

# WORKBOOKS ====

# Make the two 2026-10-02 workbooks in the package root `root`. The 2026-09-24
# workbooks MUST be in `root`/inst/2023-mht.
make_codebooks <- function(root) {
  dir <- file.path(root, "inst", "2023-mht")

  # The product table does not change. A file copy keeps it byte for byte.
  pt_from <- file.path(dir, "product_table_20260924.xlsx")
  pt_to <- file.path(dir, "product_table_20261002.xlsx")
  stopifnot(file.copy(pt_from, pt_to, overwrite = TRUE))
  stopifnot(identical(tools::md5sum(pt_to)[[1]], tools::md5sum(pt_from)[[1]]))

  # post_grouping is xl/worksheets/sheet2.xml. Its last row, 139, is the last
  # approach 4 rule. The new rules take the rows after it.
  dd_from <- file.path(dir, "dataDictionary20260924.xlsx")
  dd_to <- file.path(dir, "dataDictionary20261002.xlsx")
  x <- read_member(dd_from, "xl/worksheets/sheet2.xml")
  rows <- regmatches(
    x,
    gregexpr("<row [^>]*>.*?</row>", x, perl = TRUE, useBytes = TRUE)
  )[[1]]
  number <- as.integer(sub(
    '^<row r="([0-9]+)".*$',
    "\\1",
    rows,
    useBytes = TRUE
  ))
  last <- max(number)
  stopifnot(last == 139L, number[length(rows)] == last)

  style <- column_styles(x)
  new_rows <- vapply(
    seq_along(RULES),
    function(i) {
      return(rule_row(last + i, RULES[[i]], approach = 5L, style = style))
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
  return(invisible(c(dd_to, pt_to)))
}

# MAIN ====

INPUTS <- c("dataDictionary20260924.xlsx", "product_table_20260924.xlsx")
OUTPUTS <- c("dataDictionary20261002.xlsx", "product_table_20261002.xlsx")

# One run in a new temporary package root that holds only the two inputs. The
# md5 of each output, named by file.
check_run <- function() {
  root <- tempfile("codebooks-")
  dir <- file.path(root, "inst", "2023-mht")
  dir.create(dir, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE))
  stopifnot(file.copy(file.path("inst", "2023-mht", INPUTS), dir))
  make_codebooks(root)
  return(stats::setNames(unname(tools::md5sum(file.path(dir, OUTPUTS))), OUTPUTS))
}

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0L) {
  make_codebooks(".")
} else if (identical(args, "--check")) {
  first <- check_run()
  second <- check_run()
  committed <- tools::md5sum(file.path("inst", "2023-mht", OUTPUTS))
  committed <- stats::setNames(unname(committed), OUTPUTS)
  print(data.frame(first, second, committed))
  if (!identical(first, second) || !identical(first, committed)) {
    message("--check FAILED: the runs differ, or differ from inst/")
    quit(status = 1L)
  }
  message("--check OK: two runs match the committed workbooks")
} else {
  stop("usage: make-codebooks-20261002.R [--check]", call. = FALSE)
}
