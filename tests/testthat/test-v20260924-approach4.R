# add_lmed_v20260924() adds approach 4 to the 2026-09-22 answer. Approach 4
# splits the oestrogen route by progestogen type. Every other output must equal
# the 2026-09-22 answer, and one call must open only the two 2026-09-24
# workbooks.
#
# Tests 1 to 15 and test 20 each build a small input with named products. Grid
# week 1 starts on Monday 2016-01-04, and every dispensing falls on a Monday. A
# supply of d days from grid week k then covers grid weeks k to k + d / 7 - 1.
#
# Durations follow the MHT_groups sheet. Utrogestan 100 mg covers
# floor(fddd / 28) * 28 days. Duphaston covers floor(fddd / 12) * 28 days.
# Provera and Cerazette cover floor(fddd / 4) * 28 days, and need 3 months.
# Mirena covers 1680 days whatever its fddd. Every other product used here
# covers fddd days.

UTROGESTAN_LNMN <- "Utrogestan, kapsel, mjuk 100 mg"
GRID_FIRST <- as.Date("2016-01-04")

# The Monday that starts grid week `k`.
monday <- function(k) {
  return(GRID_FIRST + 7L * (as.integer(k) - 1L))
}

# The ISO week of grid week `k`.
grid_week <- function(k) {
  return(cstime::date_to_isoyearweek_c(monday(k)))
}

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

fixture <- function() {
  skeleton <- data.table::copy(mht::fake_skeleton_mht)
  lmed <- data.table::setDT(data.table::copy(mht::fake_lmed_2026))
  lmed[, lnmn := NA_character_]
  lmed[produkt == "Utrogestan", lnmn := UTROGESTAN_LNMN]
  # `Paracetamol` is a fixture-only name. The table is built from the real
  # deliveries, which write branded spellings instead.
  lmed <- lmed[produkt != "Paracetamol"]
  return(list(skeleton = skeleton, lmed = lmed))
}

# A dropped prescription warns. The fixture holds one negative-duration row.
# Muffle only that warning, so any other warning still reaches the reporter.
muffle_dropped <- function(expr) {
  return(withCallingHandlers(expr, warning = function(w) {
    if (grepl("prescriptions dropped", conditionMessage(w), fixed = TRUE)) {
      invokeRestart("muffleWarning")
    }
  }))
}

# Consecutive ISO weeks, the same run for every id, from the Monday `first`.
weekly_grid <- function(ids, first = "2016-01-04", n = 26L) {
  weeks <- cstime::date_to_isoyearweek_c(
    seq(as.Date(first), by = 7, length.out = n)
  )
  return(data.table::data.table(
    id = rep(ids, each = n),
    isoyearweek = rep(weeks, times = length(ids))
  ))
}

# One dispensing per element. Utrogestan needs `lnmn` for its strength. Every
# other product used here needs none, so its `lnmn` is NA.
dispensings <- function(lopnr, produkt, edatum, fddd, lnmn = NA_character_) {
  return(data.table::data.table(
    lopnr = lopnr,
    produkt = produkt,
    edatum = as.Date(edatum),
    fddd = fddd,
    lnmn = lnmn
  ))
}

# One call of the new entry point on a grid of `n` weeks for every person.
run4 <- function(lmed, n = 26L) {
  skeleton <- weekly_grid(sort(unique(lmed$lopnr)), n = n)
  add_lmed_v20260924(skeleton, lmed, verbose = FALSE)
  return(skeleton)
}

# Column `col` of one person in grid weeks `weeks`.
in_weeks <- function(out, person, weeks, col = "approach4") {
  return(out[id == person][match(grid_week(weeks), isoyearweek)][[col]])
}

expect_level <- function(out, person, weeks, level, col = "approach4") {
  expect_identical(
    in_weeks(out, person, weeks, col),
    rep(level, length(weeks)),
    info = paste("id", person, col)
  )
}

test_that("1. Femoston and Femostonconti alone are oral_estrogen_progesterone", {
  lmed <- dispensings(
    lopnr = c(1L, 2L),
    produkt = c("Femoston", "Femostonconti"),
    edatum = monday(c(2, 2)),
    fddd = c(56, 56)
  )
  out <- run4(lmed)
  for (i in 1:2) {
    expect_level(out, i, 2:9, "oral_estrogen_progesterone")
    expect_level(out, i, c(1, 10), "local_or_none_mht")
  }
})

test_that("2. oral oestrogen with C1 or C5 is oral_estrogen_progesterone", {
  # 1: Progynon + Utrogestan. 2: Progynon + Duphaston. 3: Premarina +
  # Utrogestan. Duphaston fddd 24 covers 56 days.
  lmed <- dispensings(
    lopnr = c(1L, 1L, 2L, 2L, 3L, 3L),
    produkt = c(
      "Progynon",
      "Utrogestan",
      "Progynon",
      "Duphaston",
      "Premarina",
      "Utrogestan"
    ),
    edatum = monday(rep(2, 6)),
    fddd = c(56, 56, 56, 24, 56, 56),
    lnmn = c(NA, UTROGESTAN_LNMN, NA, NA, NA, UTROGESTAN_LNMN)
  )
  out <- run4(lmed)
  for (i in 1:3) {
    expect_level(out, i, 2:9, "oral_estrogen_progesterone")
    expect_level(out, i, c(1, 10), "local_or_none_mht")
  }
})

test_that("3. oral oestrogen with another progestogen is its own level", {
  # Ids 1 to 6 take Progynon, id 7 takes Premarina. Provera and Cerazette fddd
  # 12 cover 84 days, so their weeks 10 to 13 hold a progestogen alone.
  partner <- c(
    "Provera",
    "Cerazette",
    "Slinda",
    "Mirena",
    "Nexplanon",
    "Depo-Provera",
    "Provera"
  )
  lmed <- dispensings(
    lopnr = rep(1:7, each = 2),
    produkt = as.vector(rbind(c(rep("Progynon", 6), "Premarina"), partner)),
    edatum = monday(rep(2, 14)),
    fddd = as.vector(rbind(56, c(12, 12, 56, 1, 56, 56, 12)))
  )
  out <- run4(lmed)
  for (i in 1:7) {
    expect_level(out, i, 2:9, "oral_estrogen_other_progestogen")
    expect_level(out, i, c(1, 10), "local_or_none_mht")
  }
})

test_that("4. Activelle alone is oral_estrogen_other_progestogen", {
  lmed <- dispensings(1L, "Activelle", monday(2), 56)
  out <- run4(lmed)
  expect_level(out, 1L, 2:9, "oral_estrogen_other_progestogen")
  expect_level(out, 1L, c(1, 10), "local_or_none_mht")
})

test_that("5. Divigel with C1 or C5 is transdermal_estrogen_progesterone", {
  # 1: Divigel + Utrogestan. 2: Divigel + Duphaston.
  lmed <- dispensings(
    lopnr = c(1L, 1L, 2L, 2L),
    produkt = c("Divigel", "Utrogestan", "Divigel", "Duphaston"),
    edatum = monday(rep(2, 4)),
    fddd = c(56, 56, 56, 24),
    lnmn = c(NA, UTROGESTAN_LNMN, NA, NA)
  )
  out <- run4(lmed)
  for (i in 1:2) {
    expect_level(out, i, 2:9, "transdermal_estrogen_progesterone")
    expect_level(out, i, c(1, 10), "local_or_none_mht")
  }
})

test_that("6. Divigel + Mirena and Estalis are transdermal_other_progestogen", {
  # 1: Divigel + Mirena. 2: Estalis alone.
  lmed <- dispensings(
    lopnr = c(1L, 1L, 2L),
    produkt = c("Divigel", "Mirena", "Estalis"),
    edatum = monday(c(2, 2, 2)),
    fddd = c(56, 1, 56)
  )
  out <- run4(lmed)
  for (i in 1:2) {
    expect_level(out, i, 2:9, "transdermal_estrogen_other_progestogen")
    expect_level(out, i, c(1, 10), "local_or_none_mht")
  }
})

test_that("7. a progestogen alone is local_or_none_mht", {
  # 1: Mirena. 2: Utrogestan. 3: Cerazette.
  lmed <- dispensings(
    lopnr = 1:3,
    produkt = c("Mirena", "Utrogestan", "Cerazette"),
    edatum = monday(c(2, 2, 2)),
    fddd = c(1, 56, 12),
    lnmn = c(NA, UTROGESTAN_LNMN, NA)
  )
  out <- run4(lmed)
  category <- c("E1", "C1", "C3")
  for (i in 1:3) {
    # The product covers the weeks, so the level is not an absent row.
    expect_level(out, i, 2:9, TRUE, col = category[i])
    expect_level(out, i, 1:26, "local_or_none_mht")
  }
})

test_that("8. Divigel alone is estrogen_only", {
  lmed <- dispensings(1L, "Divigel", monday(2), 56)
  out <- run4(lmed)
  expect_level(out, 1L, 2:9, "estrogen_only")
  expect_level(out, 1L, c(1, 10), "local_or_none_mht")
})

test_that("9. Oestriol Aspen alone is local_or_none_mht", {
  lmed <- dispensings(1L, "Oestriol Aspen", monday(2), 56)
  out <- run4(lmed)
  expect_level(out, 1L, 2:9, TRUE, col = "A5")
  expect_level(out, 1L, 1:26, "local_or_none_mht")
})

test_that("10. Livial is tibolone", {
  lmed <- dispensings(1L, "Livial", monday(2), 56)
  out <- run4(lmed)
  expect_level(out, 1L, 2:9, "tibolone")
  expect_level(out, 1L, c(1, 10), "local_or_none_mht")
})

test_that("11. Divigel + Utrogestan + Provera on one day is a clash", {
  # Each supply covers 84 days, grid weeks 2 to 13. Provera fddd 12 covers 84
  # days. The clash lasts to the end of the episode.
  lmed <- dispensings(
    lopnr = c(1L, 1L, 1L),
    produkt = c("Divigel", "Utrogestan", "Provera"),
    edatum = monday(c(2, 2, 2)),
    fddd = c(84, 84, 12),
    lnmn = c(NA, UTROGESTAN_LNMN, NA)
  )
  out <- run4(lmed)
  expect_level(out, 1L, 1, "local_or_none_mht")
  expect_level(out, 1L, 2:13, "clashingprescriptions")
  expect_level(out, 1L, 14, "local_or_none_mht")
})

test_that("12. Utrogestan added to Progynon + Provera wins while it lasts", {
  # Progynon and Provera cover grid weeks 1 to 20 (140 days; Provera fddd 20
  # is 5 months). Utrogestan from week 5 covers 56 days, weeks 5 to 12.
  lmed <- dispensings(
    lopnr = c(1L, 1L, 1L),
    produkt = c("Progynon", "Provera", "Utrogestan"),
    edatum = monday(c(1, 1, 5)),
    fddd = c(140, 20, 56),
    lnmn = c(NA, NA, UTROGESTAN_LNMN)
  )
  out <- run4(lmed)
  expect_level(out, 1L, 1:4, "oral_estrogen_other_progestogen")
  expect_level(out, 1L, 5:12, "oral_estrogen_progesterone")
  expect_level(out, 1L, 13:20, "oral_estrogen_other_progestogen")
  expect_level(out, 1L, 21, "local_or_none_mht")
})

test_that("13. Femoston then Provera alone stays until Femoston ends", {
  # Femoston covers grid weeks 1 to 12. Provera from week 5 covers 84 days,
  # weeks 5 to 16. A progestogen alone lights no level.
  lmed <- dispensings(
    lopnr = c(1L, 1L),
    produkt = c("Femoston", "Provera"),
    edatum = monday(c(1, 5)),
    fddd = c(84, 12)
  )
  out <- run4(lmed)
  expect_level(out, 1L, 1:12, "oral_estrogen_progesterone")
  expect_level(out, 1L, 13:26, "local_or_none_mht")
})

test_that("14. Femoston then Progynon + Provera moves on week 5", {
  # Femoston covers grid weeks 1 to 12. Progynon and Provera from week 5 cover
  # 84 days, weeks 5 to 16.
  lmed <- dispensings(
    lopnr = c(1L, 1L, 1L),
    produkt = c("Femoston", "Progynon", "Provera"),
    edatum = monday(c(1, 5, 5)),
    fddd = c(84, 84, 12)
  )
  out <- run4(lmed)
  expect_level(out, 1L, 1:4, "oral_estrogen_progesterone")
  expect_level(out, 1L, 5:16, "oral_estrogen_other_progestogen")
  expect_level(out, 1L, 17, "local_or_none_mht")
})

test_that("15. the rd_approach4 columns agree with the rd_approach3 columns", {
  # Every person takes Progynon + Provera. Progynon fddd 1120 and Provera fddd
  # 160 cover 160 weeks. Progynon fddd 140 and Provera fddd 20 cover 20 weeks.
  # 1: grid weeks 1 to 160, then a stop.
  # 2: grid weeks 1 to 20, then a stop.
  # 3: grid weeks 1 to 160, a stop, then a restart in weeks 171 to 190.
  lmed <- dispensings(
    lopnr = c(1L, 1L, 2L, 2L, 3L, 3L, 3L, 3L),
    produkt = rep(c("Progynon", "Provera"), 4),
    edatum = monday(c(1, 1, 1, 1, 1, 1, 171, 171)),
    fddd = c(1120, 160, 140, 20, 1120, 160, 140, 20)
  )
  out <- run4(lmed, n = 220L)
  levels <- c(
    approach4 = "oral_estrogen_other_progestogen",
    approach3 = "estrogen_progesterone_synthetic"
  )
  for (a in names(levels)) {
    single <- paste0("rd_", a, "_single")
    multiple <- paste0("rd_", a, "_multiple")
    treated <- levels[[a]]
    expect_level(out, 1L, 1:160, treated, col = a)
    expect_level(out, 2L, 1:20, treated, col = a)
    expect_level(out, 3L, c(1:160, 171:190), treated, col = a)
    for (rd in c(single, multiple)) {
      # 160 treated weeks reach 156, so the stop is `previous`.
      expect_level(out, 1L, 1:160, treated, col = rd)
      expect_level(out, 1L, 161:220, "previous", col = rd)
      # 20 treated weeks do not reach 156, so the stop is `exclude`.
      expect_level(out, 2L, 1:20, treated, col = rd)
      expect_level(out, 2L, 21:220, "exclude", col = rd)
      expect_level(out, 3L, 1:160, treated, col = rd)
      expect_level(out, 3L, 161:170, "previous", col = rd)
    }
    # A restart ends the one lifetime episode of the single variant.
    expect_level(out, 3L, 171:220, "exclude", col = single)
    # The multiple variant lets her re-enter. Her second episode is 20 weeks.
    expect_level(out, 3L, 171:190, treated, col = multiple)
    expect_level(out, 3L, 191:220, "exclude", col = multiple)
  }
})

test_that("16. every 2026-09-22 column is unchanged, and three are added", {
  f_new <- fixture()
  new <- muffle_dropped(
    add_lmed_v20260924(f_new$skeleton, f_new$lmed, verbose = FALSE)
  )
  f_old <- fixture()
  old <- muffle_dropped(
    add_lmed_v20260922(f_old$skeleton, f_old$lmed, verbose = FALSE)
  )
  added <- setdiff(names(new), names(old))
  expect_length(added, 3L)
  expect_setequal(
    added,
    c("approach4", "rd_approach4_single", "rd_approach4_multiple")
  )
  expect_identical(setdiff(names(new), added), names(old))
  for (k in names(old)) {
    expect_identical(new[[k]], old[[k]], info = k)
  }
})

# One approach 4 rule, as "variable | includes | doesnotinclude". Each set is
# sorted, so the order of the cells does not count.
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

# The 47 approach 4 rules, one line each.
APPROACH4_RULES <- c(
  "oral_estrogen_progesterone | B4 |",
  "oral_estrogen_progesterone | B11 |",
  "oral_estrogen_progesterone | A2+C1 |",
  "oral_estrogen_progesterone | A2+C5 |",
  "oral_estrogen_progesterone | A6+C1 |",
  "oral_estrogen_progesterone | A6+C5 |",
  "oral_estrogen_other_progestogen | B2 |",
  "oral_estrogen_other_progestogen | B3 |",
  "oral_estrogen_other_progestogen | B5 |",
  "oral_estrogen_other_progestogen | B6 |",
  "oral_estrogen_other_progestogen | B8 |",
  "oral_estrogen_other_progestogen | B9 |",
  "oral_estrogen_other_progestogen | B10 |",
  "oral_estrogen_other_progestogen | B12 |",
  "oral_estrogen_other_progestogen | A2+C3 |",
  "oral_estrogen_other_progestogen | A2+C4 |",
  "oral_estrogen_other_progestogen | A2+D1 |",
  "oral_estrogen_other_progestogen | A2+D2 |",
  "oral_estrogen_other_progestogen | A2+D3 |",
  "oral_estrogen_other_progestogen | A2+E1 |",
  "oral_estrogen_other_progestogen | A2+I1 |",
  "oral_estrogen_other_progestogen | A2+I2 |",
  "oral_estrogen_other_progestogen | A6+C3 |",
  "oral_estrogen_other_progestogen | A6+C4 |",
  "oral_estrogen_other_progestogen | A6+D1 |",
  "oral_estrogen_other_progestogen | A6+D2 |",
  "oral_estrogen_other_progestogen | A6+D3 |",
  "oral_estrogen_other_progestogen | A6+E1 |",
  "oral_estrogen_other_progestogen | A6+I1 |",
  "oral_estrogen_other_progestogen | A6+I2 |",
  "transdermal_estrogen_progesterone | A1+C1 |",
  "transdermal_estrogen_progesterone | A1+C5 |",
  "transdermal_estrogen_other_progestogen | B1 |",
  "transdermal_estrogen_other_progestogen | B7 |",
  "transdermal_estrogen_other_progestogen | A1+C3 |",
  "transdermal_estrogen_other_progestogen | A1+C4 |",
  "transdermal_estrogen_other_progestogen | A1+D1 |",
  "transdermal_estrogen_other_progestogen | A1+D2 |",
  "transdermal_estrogen_other_progestogen | A1+D3 |",
  "transdermal_estrogen_other_progestogen | A1+E1 |",
  "transdermal_estrogen_other_progestogen | A1+I1 |",
  "transdermal_estrogen_other_progestogen | A1+I2 |",
  "estrogen_only | A1 | C1 C2 C3 C4 D1 D2 E1 B1 B2 B3 B4 B5 B6 B7 B8 B9 B10 B11 B12 D3 I1 I2 C5",
  "estrogen_only | A2 | C1 C2 C3 C4 D1 D2 E1 B1 B2 B3 B4 B5 B6 B7 B8 B9 B10 B11 B12 D3 I1 I2 C5",
  "estrogen_only | A6 | C1 C2 C3 C4 D1 D2 E1 B1 B2 B3 B4 B5 B6 B7 B8 B9 B10 B11 B12 D3 I1 I2 C5",
  "tibolone | F1 |",
  "local_or_none_mht | |"
)

test_that("17. post_grouping holds the old 88 rules and the 47 of approach 4", {
  new <- read_sheet(codebook("dataDictionary20260924.xlsx"), "post_grouping")
  old <- read_sheet(codebook("dataDictionary20260922.xlsx"), "post_grouping")
  expect_identical(sum(!is.na(new$approach)), 135L)
  expect_identical(sum(!is.na(old$approach)), 88L)
  expect_identical(
    new[new$approach %in% c("1", "2", "3"), ],
    old[!is.na(old$approach), ]
  )

  four <- as.data.frame(new[new$approach %in% "4", ])
  expect_identical(nrow(four), 47L)
  expect_identical(sort(unique(new$approach)), c("1", "2", "3", "4"))
  include_cols <- c("includes1", "includes2")
  exclude_cols <- grep("^doesnotinclude", names(four), value = TRUE)
  cells <- function(i, cols) {
    v <- unlist(four[i, cols], use.names = FALSE)
    return(v[!is.na(v) & nzchar(v)])
  }
  actual <- vapply(
    seq_len(nrow(four)),
    function(i) {
      return(rule_key(
        four$variable[i],
        cells(i, include_cols),
        cells(i, exclude_cols)
      ))
    },
    character(1)
  )
  expected <- vapply(APPROACH4_RULES, literal_key, character(1))
  expect_length(expected, 47L)
  expect_identical(
    sort(actual, method = "radix"),
    unname(sort(expected, method = "radix"))
  )
})

test_that("18. one call opens only the two 2026-09-24 workbooks", {
  skeleton <- weekly_grid(1:2)
  lmed <- dispensings(
    lopnr = c(1L, 2L),
    produkt = c("Divigel", "Progynon"),
    edatum = c("2016-01-11", "2016-02-01"),
    fddd = c(56, 28)
  )
  seen <- new.env()
  seen$files <- character(0)
  record <- bquote(assign(
    "files",
    c(get("files", envir = .(seen)), basename(path)),
    envir = .(seen)
  ))
  ns <- asNamespace("readxl")
  for (fun in c("read_excel", "excel_sheets")) {
    suppressMessages(trace(fun, tracer = record, where = ns, print = FALSE))
  }
  withr::defer(for (fun in c("read_excel", "excel_sheets")) {
    suppressMessages(untrace(fun, where = ns))
  })
  add_lmed_v20260924(skeleton, lmed, verbose = FALSE)
  expect_identical(
    sort(unique(seen$files)),
    c("dataDictionary20260924.xlsx", "product_table_20260924.xlsx")
  )
})

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

# The one note cell that the 2026-09-24 product table rewords. The old text
# names a person, so this file holds only its md5.
OLD_NOTE_MD5 <- "354f73fab8482fb41c02a95b9c6f179d"
NEW_NOTE <- paste0(
  "Exclude the whole person. RESOLVED 2026-09-02: this closes a question ",
  "raised on 2026-09-01, whether to count compounded progesterone as ",
  "micronized progesterone despite the missing quantity. The answer is no. ",
  "The codebook's separate no FDDD reason also still stands."
)

# The md5 of the UTF-8 bytes of one string.
md5_text <- function(x) {
  f <- withr::local_tempfile()
  writeBin(charToRaw(enc2utf8(x)), f)
  return(unname(tools::md5sum(f)))
}

test_that("19. the 2026-09-24 workbooks keep every unedited part", {
  pt_new <- codebook("product_table_20260924.xlsx")
  dd_new <- codebook("dataDictionary20260924.xlsx")
  pt_old <- codebook("product_table_20260922.xlsx")
  dd_old <- codebook("dataDictionary20260922.xlsx")

  # The product table changes xl/sharedStrings.xml only.
  expect_identical(
    changed_members(pt_old, pt_new, "xl/sharedStrings.xml"),
    character(0)
  )

  # Every sheet reads the same, except for exactly one cell of `note`.
  pt_sheets <- readxl::excel_sheets(pt_old)
  expect_identical(readxl::excel_sheets(pt_new), pt_sheets)
  all_notes <- character(0)
  old_notes <- character(0)
  new_notes <- character(0)
  for (sheet in pt_sheets) {
    a <- read_sheet(pt_old, sheet)
    b <- read_sheet(pt_new, sheet)
    expect_identical(names(b), names(a), info = sheet)
    kept <- setdiff(names(a), "note")
    expect_identical(b[kept], a[kept], info = sheet)
    if ("note" %in% names(a)) {
      all_notes <- c(all_notes, a$note[!is.na(a$note)])
      same <- vapply(
        seq_len(nrow(a)),
        function(i) identical(a$note[i], b$note[i]),
        logical(1)
      )
      old_notes <- c(old_notes, a$note[!same])
      new_notes <- c(new_notes, b$note[!same])
    }
  }
  expect_identical(length(old_notes), 1L, info = "exactly one cell differs")
  expect_identical(
    vapply(old_notes, md5_text, character(1), USE.NAMES = FALSE),
    OLD_NOTE_MD5
  )
  expect_identical(new_notes, NEW_NOTE)

  # The removed name is the word between "the question" and "raised" in the
  # old cell. No member of the new table holds it, in any case.
  old_cell <- all_notes[vapply(all_notes, md5_text, character(1)) == OLD_NOTE_MD5]
  expect_length(old_cell, 1L)
  name <- sub("^.* the question ([A-Za-z]+) raised .*$", "\\1", old_cell[1])
  expect_match(name, "^[A-Za-z]+$")
  holds_name <- function(path) {
    found <- vapply(
      members(path),
      function(m) length(grepRaw(name, m, ignore.case = TRUE)) > 0L,
      logical(1)
    )
    return(names(found)[found])
  }
  expect_identical(holds_name(pt_old), "xl/sharedStrings.xml")
  expect_identical(holds_name(pt_new), character(0), info = "name check")

  needed <- c(sheet_member(dd_old, "post_grouping"), "xl/sharedStrings.xml")
  expect_identical(needed[1], "xl/worksheets/sheet2.xml")
  expect_identical(sheet_member(dd_new, "post_grouping"), needed[1])
  expect_identical(changed_members(dd_old, dd_new, needed), character(0))

  sheets <- readxl::excel_sheets(dd_old)
  expect_identical(readxl::excel_sheets(dd_new), sheets)
  for (sheet in setdiff(sheets, "post_grouping")) {
    expect_identical(
      read_sheet(dd_new, sheet),
      read_sheet(dd_old, sheet),
      info = sheet
    )
  }
})

test_that("20. a Prolutex holder is flagged and lights no approach 4 level", {
  # 1: Divigel + Prolutex. 2: Divigel alone, which shows that C2 blocks the
  # level.
  lmed <- dispensings(
    lopnr = c(1L, 1L, 2L),
    produkt = c("Divigel", "Prolutex", "Divigel"),
    edatum = monday(c(2, 2, 2)),
    fddd = c(56, 56, 56)
  )
  out <- run4(lmed)
  expect_true(all(out[id == 1L]$ri_mht_excluded_product))
  expect_level(out, 1L, 2:9, TRUE, col = "A1")
  expect_level(out, 1L, 2:9, TRUE, col = "C2")
  expect_level(out, 1L, 1:26, "local_or_none_mht")
  expect_false(any(out[id == 2L]$ri_mht_excluded_product))
  expect_level(out, 2L, 2:9, "estrogen_only")
})
