# The 2026-09-22 workbooks are their predecessors plus a named delta. Three
# tests build each expected sheet from the old workbook and the delta, then
# compare it with the new sheet by identical(). Two tests check properties of
# the new post_grouping. The last test compares the zip members byte for byte.
# dev/codebooks-20260922/ makes the new workbooks.
#
# Row numbers in comments are spreadsheet rows, with the header as row 1. Row i
# of a sheet that readxl reads is spreadsheet row i + 1.

codebook <- function(file) {
  return(system.file("2023-mht", file, package = "mht"))
}
PT_OLD <- codebook("product_table_20260902.xlsx")
PT_NEW <- codebook("product_table_20260922.xlsx")
DD_OLD <- codebook("dataDictionary20260828.xlsx")
DD_NEW <- codebook("dataDictionary20260922.xlsx")

read_sheet <- function(path, sheet) {
  return(readxl::read_excel(
    path,
    sheet = sheet,
    col_types = "text",
    .name_repair = "unique_quiet"
  ))
}

# Sheet G, row 60: the five cells of Estradiol Valerate that change.
EV_ROW <- 59L
EV_BEFORE <- c(
  classification = "notmht",
  classification_meaning = "not menopausal hormone therapy",
  exclude_entire_person = "FALSE",
  exclusion_reason = NA,
  source = "decision20260826"
)
EV_AFTER <- c(
  classification = "A7",
  classification_meaning = "oestrogens, injection",
  exclude_entire_person = "TRUE",
  exclusion_reason = "possible gender-affirming therapy",
  source = "decision 2026-09-22"
)

# The 14 post_grouping rules that the edit deletes, as rows of
# dataDictionary20260828.xlsx.
DELETED <- data.frame(
  row = c(8L, 38L, 47L, 61L, 65L, 69L, 73L, 77L, 81L, 85L, 89L, 93L, 99L, 103L),
  variable = c(
    "systemic_mht",
    "transdermal_estrogen",
    "estrogen_only",
    rep("estrogen_progesterone_synthetic", 9),
    rep("estrogen_progesterone_bioidentical", 2)
  ),
  approach = c("1", "2", rep("3", 12)),
  includes1 = "A7",
  includes2 = c(
    NA, NA, NA, "C2", "C3", "C4", "D1", "D2", "D3", "E1", "I1", "I2", "C1", "C5"
  )
)
# Row 47 is the one deleted rule with exclusions, in doesnotinclude1 to 23.
ROW47_EXCLUDES <- c(
  "C1", "C2", "C3", "C4", "D1", "D2", "E1", paste0("B", 1:12),
  "D3", "I1", "I2", "C5"
)
# Rows 2 to 4 keep their row. They lose A7, their doesnotinclude4, and keep
# every other cell.
CLEARED_EXCLUDES <- c("A1", "A2", "A6", "A7", paste0("B", 1:12))

# The 30 doesnotinclude cells of readxl row i, in column order.
excludes <- function(d, i) {
  return(unlist(d[i, grep("^doesnotinclude", names(d))], use.names = FALSE))
}

# Every member of a workbook as raw bytes, in archive order.
members <- function(path) {
  dir <- withr::local_tempdir()
  utils::unzip(path, exdir = dir)
  files <- utils::unzip(path, list = TRUE)$Name
  out <- lapply(file.path(dir, files), function(f) {
    return(readBin(f, "raw", file.size(f)))
  })
  names(out) <- files
  return(out)
}

# The zip member that holds a sheet, from the workbook and its relationships.
sheet_member <- function(path, sheet) {
  m <- members(path)
  wb <- rawToChar(m[["xl/workbook.xml"]])
  rels <- rawToChar(m[["xl/_rels/workbook.xml.rels"]])
  id <- regmatches(
    wb,
    regexec(sprintf('<sheet name="%s"[^>]* r:id="([^"]+)"', sheet), wb)
  )[[1]][2]
  target <- regmatches(
    rels,
    regexec(sprintf('Id="%s"[^>]* Target="([^"]+)"', id), rels)
  )[[1]][2]
  return(paste0("xl/", target))
}

# The members of `new` that differ from the same member of `old`, among those
# the edit does not need.
changed_members <- function(old, new, needed) {
  a <- members(old)
  b <- members(new)
  expect_identical(names(b), names(a))
  kept <- setdiff(names(a), needed)
  same <- vapply(kept, function(m) identical(a[[m]], b[[m]]), logical(1))
  return(kept[!same])
}

test_that("the product table changes only five cells of Estradiol Valerate", {
  sheets <- readxl::excel_sheets(PT_OLD)
  expect_identical(sheets, c("G", "other", "README"))
  expect_identical(readxl::excel_sheets(PT_NEW), sheets)
  for (sheet in sheets) {
    expected <- read_sheet(PT_OLD, sheet)
    if (sheet == "G") {
      expect_identical(
        which(expected$produkt_raw == "Estradiol Valerate"),
        EV_ROW
      )
      for (col in names(EV_AFTER)) {
        expect_identical(expected[[col]][EV_ROW], EV_BEFORE[[col]])
        expected[[col]][EV_ROW] <- EV_AFTER[[col]]
      }
    }
    expect_identical(read_sheet(PT_NEW, sheet), expected, info = sheet)
  }
})

test_that("the dictionary keeps every sheet except post_grouping unchanged", {
  sheets <- readxl::excel_sheets(DD_OLD)
  expect_true("post_grouping" %in% sheets)
  expect_identical(readxl::excel_sheets(DD_NEW), sheets)
  for (sheet in setdiff(sheets, "post_grouping")) {
    expect_identical(
      read_sheet(DD_NEW, sheet),
      read_sheet(DD_OLD, sheet),
      info = sheet
    )
  }
})

test_that("post_grouping loses the 14 A7 rules and A7 from 3 exclusion lists", {
  old <- read_sheet(DD_OLD, "post_grouping")
  new <- read_sheet(DD_NEW, "post_grouping")
  i <- DELETED$row - 1L

  # The deleted rules, pinned by content in the old sheet.
  for (col in c("variable", "approach", "includes1", "includes2")) {
    expect_identical(old[[col]][i], DELETED[[col]], info = col)
  }
  expect_identical(excludes(old, 46L), c(ROW47_EXCLUDES, rep(NA, 7)))
  for (k in setdiff(i, 46L)) {
    expect_true(all(is.na(excludes(old, k))), info = k + 1L)
  }

  # The three rules that lose one cell.
  expect_identical(old$variable[1:3], rep("local_or_none_mht", 3))
  expect_identical(old$approach[1:3], rep("1", 3))
  expect_identical(old$includes1[1:3], c("A3", "A4", "A5"))
  for (k in 1:3) {
    expect_identical(excludes(old, k), c(CLEARED_EXCLUDES, rep(NA, 14)))
  }

  # The rows between the deleted ones, the blank separators included, keep
  # their order.
  expected <- old[-i, ]
  expected$doesnotinclude4[1:3] <- NA_character_
  expect_identical(new, expected)
  expect_identical(nrow(new), 90L)
  expect_identical(sum(!is.na(new$variable)), 88L)
})

test_that("no cell of the new post_grouping holds A7", {
  # The old sheet shows that the check can see A7.
  old <- unlist(read_sheet(DD_OLD, "post_grouping"), use.names = FALSE)
  expect_identical(sum(old == "A7", na.rm = TRUE), 17L)
  new <- unlist(read_sheet(DD_NEW, "post_grouping"), use.names = FALSE)
  expect_identical(sum(new == "A7", na.rm = TRUE), 0L)
})

test_that("the edit makes no rule with an empty includes set", {
  empty <- function(d) {
    return(d[!is.na(d$variable) & is.na(d$includes1) & is.na(d$includes2), ])
  }
  old <- read_sheet(DD_OLD, "post_grouping")
  new <- read_sheet(DD_NEW, "post_grouping")
  # Old row 105 is the one such rule before the edit. It is new row 91.
  expect_identical(nrow(empty(old)), 1L)
  expect_identical(empty(new), empty(old))
  expect_identical(new[90L, ], old[104L, ])
  expect_identical(new$variable[90L], "local_or_none_mht")
  expect_identical(new$approach[90L], "3")
})

test_that("every zip member the edit does not need is byte-identical", {
  pt_needed <- c(sheet_member(PT_OLD, "G"), "xl/sharedStrings.xml")
  expect_identical(
    pt_needed,
    c("xl/worksheets/sheet1.xml", "xl/sharedStrings.xml")
  )
  expect_identical(changed_members(PT_OLD, PT_NEW, pt_needed), character(0))

  dd_needed <- sheet_member(DD_OLD, "post_grouping")
  expect_identical(dd_needed, "xl/worksheets/sheet2.xml")
  expect_identical(changed_members(DD_OLD, DD_NEW, dd_needed), character(0))
})
