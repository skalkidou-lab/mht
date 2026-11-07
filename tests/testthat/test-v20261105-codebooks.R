# The 2026-11-05 codebook is the 2026-10-02 codebook with the 19 approach 6
# rules appended to post_grouping. The 2026-11-05 product table is a byte copy
# of the 2026-10-02 table. dev/codebooks-20261105/ makes both workbooks, and
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

# The rules of one approach in a post_grouping sheet, in sheet order. `full` is
# the rule key.
rules_of <- function(sheet, approach) {
  x <- as.data.frame(sheet[sheet$approach %in% approach, ])
  include_cols <- c("includes1", "includes2")
  exclude_cols <- grep("^doesnotinclude", names(x), value = TRUE)
  includes <- lapply(seq_len(nrow(x)), row_cells, x = x, cols = include_cols)
  excludes <- lapply(seq_len(nrow(x)), row_cells, x = x, cols = exclude_cols)
  return(list(
    variable = x$variable,
    includes = includes,
    excludes = excludes,
    full = mapply(rule_key, x$variable, includes, excludes, USE.NAMES = FALSE)
  ))
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

LOCAL_EXCLUDES <- "A1 A2 A6 B1 B2 B3 B4 B5 B6 B7 B8 B9 B10 B11 B12 F1"

# The 19 approach 6 rules, from the contract.
APPROACH6_RULES <- c(
  paste("local_mht | A3 |", LOCAL_EXCLUDES),
  paste("local_mht | A4 |", LOCAL_EXCLUDES),
  paste("local_mht | A5 |", LOCAL_EXCLUDES),
  "systemic_mht | A1 |",
  "systemic_mht | A2 |",
  "systemic_mht | A6 |",
  "systemic_mht | B1 |",
  "systemic_mht | B2 |",
  "systemic_mht | B3 |",
  "systemic_mht | B4 |",
  "systemic_mht | B5 |",
  "systemic_mht | B6 |",
  "systemic_mht | B7 |",
  "systemic_mht | B8 |",
  "systemic_mht | B9 |",
  "systemic_mht | B10 |",
  "systemic_mht | B11 |",
  "systemic_mht | B12 |",
  "tibolone | F1 |"
)

test_that("1. every 2026-11-05 sheet but post_grouping equals 2026-10-02", {
  dd_new <- codebook("dataDictionary20261105.xlsx")
  dd_old <- codebook("dataDictionary20261002.xlsx")
  sheets <- readxl::excel_sheets(dd_old)
  expect_identical(readxl::excel_sheets(dd_new), sheets)
  expect_true("post_grouping" %in% sheets)
  expect_length(setdiff(sheets, "post_grouping"), 4L)
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

test_that("2. post_grouping holds the old 182 rules and the 19 of approach 6", {
  new <- read_sheet(codebook("dataDictionary20261105.xlsx"), "post_grouping")
  old <- read_sheet(codebook("dataDictionary20261002.xlsx"), "post_grouping")
  expect_identical(names(new), names(old))
  expect_identical(nrow(old), 185L)
  all_na <- rowSums(!is.na(as.data.frame(old))) == 0L
  expect_identical(sum(all_na), 3L)
  expect_identical(sum(!is.na(old$approach)), 182L)

  # The 185 old rows, the 3 all-NA rows among them, are unchanged and first.
  expect_identical(nrow(new), nrow(old) + 19L)
  expect_identical(new[seq_len(nrow(old)), ], old)

  count <- function(x) {
    return(as.integer(table(factor(x$approach, levels = as.character(1:6)))))
  }
  expect_identical(count(old), c(19L, 19L, 50L, 47L, 47L, 0L))
  expect_identical(count(new), c(19L, 19L, 50L, 47L, 47L, 19L))
  for (a in as.character(1:5)) {
    expect_identical(rules_of(new, a)$full, rules_of(old, a)$full, info = a)
  }

  appended <- new[nrow(old) + seq_len(19L), ]
  expect_identical(appended$approach, rep("6", 19L))
  actual <- rules_of(new, "6")$full
  expected <- vapply(APPROACH6_RULES, literal_key, character(1))
  expect_length(expected, 19L)
  expect_identical(
    sort(actual, method = "radix"),
    unname(sort(expected, method = "radix"))
  )
})

test_that("3. approach 6 is approach 1 with local_mht and F1 in its excludes", {
  sheet <- read_sheet(codebook("dataDictionary20261105.xlsx"), "post_grouping")
  one <- rules_of(sheet, "1")
  six <- rules_of(sheet, "6")
  expect_length(six$full, 19L)
  expect_identical(
    as.integer(table(factor(
      six$variable,
      levels = c("systemic_mht", "local_mht", "tibolone")
    ))),
    c(15L, 3L, 1L)
  )
  expect_setequal(six$variable, c("systemic_mht", "local_mht", "tibolone"))
  expect_false(any(
    c("no_local_or_systemic_mht", "local_or_none_mht") %in% six$variable
  ))

  # Approach 1 with local_or_none_mht renamed and F1 added to its excludes.
  expect_identical(sum(one$variable == "local_or_none_mht"), 3L)
  local <- one$variable == "local_or_none_mht"
  expect_false(any(vapply(one$excludes[local], `%in%`, logical(1), x = "F1")))
  expected <- mapply(
    function(v, inc, exc) {
      if (v == "local_or_none_mht") {
        return(rule_key("local_mht", inc, c(exc, "F1")))
      }
      return(rule_key(v, inc, exc))
    },
    one$variable,
    one$includes,
    one$excludes,
    USE.NAMES = FALSE
  )
  expect_identical(six$full, expected)

  # The cells in order: the systemic and tibolone rows equal approach 1 cell
  # for cell. A local row holds the approach 1 excludes in order, then F1.
  for (i in seq_along(six$full)) {
    expect_identical(
      six$includes[[i]],
      one$includes[[i]],
      info = as.character(i)
    )
    if (one$variable[i] == "local_or_none_mht") {
      expect_identical(
        six$excludes[[i]],
        c(one$excludes[[i]], "F1"),
        info = as.character(i)
      )
    } else {
      expect_identical(six$excludes[[i]], character(0), info = as.character(i))
      expect_identical(
        six$excludes[[i]],
        one$excludes[[i]],
        info = as.character(i)
      )
    }
  }
})

test_that("4. the 2026-11-05 product table is a byte copy of 2026-10-02", {
  new <- codebook("product_table_20261105.xlsx")
  old <- codebook("product_table_20261002.xlsx")
  expect_identical(
    readBin(new, "raw", file.size(new)),
    readBin(old, "raw", file.size(old))
  )
})
