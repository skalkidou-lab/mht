# The 2026-10-02 codebook is the 2026-09-24 codebook with the 47 approach 5
# rules appended to post_grouping. The 2026-10-02 product table is a byte copy
# of the 2026-09-24 table. dev/codebooks-20261002/ makes both workbooks, and
# its --check mode shows that two runs write the committed bytes.

codebook <- function(file) {
  path <- system.file("2023-mht", file, package = "mht")
  if (!nzchar(path)) {
    stop(file, " is not installed", call. = FALSE)
  }
  return(path)
}

read_sheet <- function(path, sheet) {
  return(readxl::read_excel(
    path,
    sheet = sheet,
    col_types = "text",
    .name_repair = "unique_quiet"
  ))
}

# One rule, as "variable | includes | doesnotinclude". Each set is sorted, so
# the order of the cells does not count.
rule_key <- function(variable, includes, excludes) {
  return(paste(
    variable,
    paste(sort(includes, method = "radix"), collapse = "+"),
    paste(sort(excludes, method = "radix"), collapse = " "),
    sep = " | "
  ))
}

# The same key for each literal line of the expected table below.
literal_key <- function(line) {
  f <- trimws(strsplit(line, "|", fixed = TRUE)[[1]])
  f <- c(f, rep("", 3L - length(f)))
  cells <- function(x, sep) {
    v <- strsplit(x, sep, fixed = TRUE)[[1]]
    return(v[nzchar(v)])
  }
  return(rule_key(f[1], cells(f[2], "+"), cells(f[3], " ")))
}

# The non-empty cells of row `i` of `x` in the columns `cols`.
row_cells <- function(i, x, cols) {
  v <- unlist(x[i, cols], use.names = FALSE)
  return(v[!is.na(v) & nzchar(v)])
}

# The rules of one approach in a post_grouping sheet. `full` is the rule key.
# `key` leaves out the variable, so rules of two levels compare by their cells.
rules_of <- function(sheet, approach) {
  x <- as.data.frame(sheet[sheet$approach %in% approach, ])
  include_cols <- c("includes1", "includes2")
  exclude_cols <- grep("^doesnotinclude", names(x), value = TRUE)
  includes <- lapply(seq_len(nrow(x)), row_cells, x = x, cols = include_cols)
  excludes <- lapply(seq_len(nrow(x)), row_cells, x = x, cols = exclude_cols)
  return(list(
    variable = x$variable,
    includes = includes,
    full = mapply(rule_key, x$variable, includes, excludes, USE.NAMES = FALSE),
    key = mapply(rule_key, "", includes, excludes, USE.NAMES = FALSE)
  ))
}

# The sorted keys of the rules of `r` whose variable is in `variables`.
keys_of <- function(r, variables) {
  return(sort(r$key[r$variable %in% variables], method = "radix"))
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

# The 47 approach 5 rules, from the contract.
APPROACH5_RULES <- c(
  "estrogen_progesterone | B4 |",
  "estrogen_progesterone | B11 |",
  "estrogen_progesterone | A1+C1 |",
  "estrogen_progesterone | A1+C5 |",
  "estrogen_progesterone | A2+C1 |",
  "estrogen_progesterone | A2+C5 |",
  "estrogen_progesterone | A6+C1 |",
  "estrogen_progesterone | A6+C5 |",
  "estrogen_other_progestogen | B1 |",
  "estrogen_other_progestogen | B2 |",
  "estrogen_other_progestogen | B3 |",
  "estrogen_other_progestogen | B5 |",
  "estrogen_other_progestogen | B6 |",
  "estrogen_other_progestogen | B7 |",
  "estrogen_other_progestogen | B8 |",
  "estrogen_other_progestogen | B9 |",
  "estrogen_other_progestogen | B10 |",
  "estrogen_other_progestogen | B12 |",
  "estrogen_other_progestogen | A1+C3 |",
  "estrogen_other_progestogen | A1+C4 |",
  "estrogen_other_progestogen | A1+D1 |",
  "estrogen_other_progestogen | A1+D2 |",
  "estrogen_other_progestogen | A1+D3 |",
  "estrogen_other_progestogen | A1+I1 |",
  "estrogen_other_progestogen | A1+I2 |",
  "estrogen_other_progestogen | A2+C3 |",
  "estrogen_other_progestogen | A2+C4 |",
  "estrogen_other_progestogen | A2+D1 |",
  "estrogen_other_progestogen | A2+D2 |",
  "estrogen_other_progestogen | A2+D3 |",
  "estrogen_other_progestogen | A2+I1 |",
  "estrogen_other_progestogen | A2+I2 |",
  "estrogen_other_progestogen | A6+C3 |",
  "estrogen_other_progestogen | A6+C4 |",
  "estrogen_other_progestogen | A6+D1 |",
  "estrogen_other_progestogen | A6+D2 |",
  "estrogen_other_progestogen | A6+D3 |",
  "estrogen_other_progestogen | A6+I1 |",
  "estrogen_other_progestogen | A6+I2 |",
  "estrogen_iud | A1+E1 |",
  "estrogen_iud | A2+E1 |",
  "estrogen_iud | A6+E1 |",
  "estrogen_only | A1 | C1 C2 C3 C4 D1 D2 E1 B1 B2 B3 B4 B5 B6 B7 B8 B9 B10 B11 B12 D3 I1 I2 C5",
  "estrogen_only | A2 | C1 C2 C3 C4 D1 D2 E1 B1 B2 B3 B4 B5 B6 B7 B8 B9 B10 B11 B12 D3 I1 I2 C5",
  "estrogen_only | A6 | C1 C2 C3 C4 D1 D2 E1 B1 B2 B3 B4 B5 B6 B7 B8 B9 B10 B11 B12 D3 I1 I2 C5",
  "tibolone | F1 |",
  "local_or_none_mht | |"
)

test_that("1. every 2026-10-02 sheet but post_grouping equals 2026-09-24", {
  dd_new <- codebook("dataDictionary20261002.xlsx")
  dd_old <- codebook("dataDictionary20260924.xlsx")
  sheets <- readxl::excel_sheets(dd_old)
  expect_identical(readxl::excel_sheets(dd_new), sheets)
  expect_true("post_grouping" %in% sheets)
  for (sheet in setdiff(sheets, "post_grouping")) {
    expect_identical(
      read_sheet(dd_new, sheet),
      read_sheet(dd_old, sheet),
      info = sheet
    )
  }

  # Only the post_grouping member, xl/worksheets/sheet2.xml, changes.
  a <- members(dd_old)
  b <- members(dd_new)
  expect_identical(names(b), names(a))
  same <- vapply(names(a), function(m) identical(a[[m]], b[[m]]), logical(1))
  expect_identical(names(a)[!same], "xl/worksheets/sheet2.xml")
})

test_that("2. post_grouping holds the old 135 rules and the 47 of approach 5", {
  new <- read_sheet(codebook("dataDictionary20261002.xlsx"), "post_grouping")
  old <- read_sheet(codebook("dataDictionary20260924.xlsx"), "post_grouping")
  expect_identical(names(new), names(old))
  expect_identical(sum(!is.na(old$approach)), 135L)
  expect_identical(sum(!is.na(new$approach)), 182L)
  expect_identical(nrow(new), nrow(old) + 47L)
  expect_identical(new[seq_len(nrow(old)), ], old)

  appended <- new[nrow(old) + seq_len(47L), ]
  expect_identical(appended$approach, rep("5", 47L))
  expect_identical(sort(unique(new$approach)), c("1", "2", "3", "4", "5"))
  actual <- rules_of(new, "5")$full
  expected <- vapply(APPROACH5_RULES, literal_key, character(1))
  expect_length(expected, 47L)
  expect_identical(
    sort(actual, method = "radix"),
    unname(sort(expected, method = "radix"))
  )
})

test_that("3. the approach 5 rules regroup the approach 4 rules", {
  sheet <- read_sheet(codebook("dataDictionary20261002.xlsx"), "post_grouping")
  four <- rules_of(sheet, "4")
  five <- rules_of(sheet, "5")
  expect_length(five$key, 47L)

  progesterone <- keys_of(five, "estrogen_progesterone")
  expect_length(progesterone, 8L)
  expect_identical(
    progesterone,
    keys_of(
      four,
      c("oral_estrogen_progesterone", "transdermal_estrogen_progesterone")
    )
  )

  other4 <- c(
    "oral_estrogen_other_progestogen",
    "transdermal_estrogen_other_progestogen"
  )
  other5 <- keys_of(five, c("estrogen_other_progestogen", "estrogen_iud"))
  expect_length(other5, 34L)
  expect_identical(other5, keys_of(four, other4))

  iud <- keys_of(five, "estrogen_iud")
  expect_length(iud, 3L)
  has_e1 <- vapply(four$includes, function(x) "E1" %in% x, logical(1))
  expect_identical(
    iud,
    sort(four$key[four$variable %in% other4 & has_e1], method = "radix")
  )

  for (v in c("estrogen_only", "tibolone", "local_or_none_mht")) {
    expect_identical(keys_of(five, v), keys_of(four, v), info = v)
  }

  expect_identical(
    intersect(unlist(five$includes), c("C2", "A3", "A4", "A5", "A7", "G1", "H1")),
    character(0)
  )
})

test_that("4. the 2026-10-02 product table is a byte copy of 2026-09-24", {
  expect_identical(
    unname(tools::md5sum(codebook("product_table_20261002.xlsx"))),
    unname(tools::md5sum(codebook("product_table_20260924.xlsx")))
  )
})

test_that("5. the copied approach 5 rules keep the approach 4 cells in order", {
  sheet <- read_sheet(codebook("dataDictionary20261002.xlsx"), "post_grouping")
  exclude_cols <- paste0("doesnotinclude", 1:30)
  expect_true(all(exclude_cols %in% names(sheet)))
  # The variable and the includes name one row of each approach.
  name_rows <- function(approach, v) {
    x <- as.data.frame(sheet[sheet$approach %in% approach & sheet$variable %in% v, ])
    key <- paste(x$variable, x$includes1, x$includes2)
    expect_false(anyDuplicated(key) > 0L, info = paste(approach, v))
    rownames(x) <- key
    return(x)
  }
  for (v in c("estrogen_only", "tibolone", "local_or_none_mht")) {
    four <- name_rows("4", v)
    five <- name_rows("5", v)
    expect_identical(sort(rownames(five)), sort(rownames(four)), info = v)
    expect_gt(nrow(five), 0L)
    for (k in rownames(four)) {
      expect_identical(
        unlist(five[k, exclude_cols], use.names = FALSE),
        unlist(four[k, exclude_cols], use.names = FALSE),
        info = k
      )
    }
  }
})
