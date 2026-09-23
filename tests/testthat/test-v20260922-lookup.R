# add_lmed_v20260922() is add_lmed_v20260902() on the two 2026-09-22
# workbooks. Two outputs change: Estradiol Valerate flags the woman, and an A7
# week counts as no treatment. Every other output must match the 2026-09-02
# answer, and one call must open only the two new workbooks.
#
# Items 1 to 5 run on the prepared package fixture. Items 6 to 14 each build a
# small input of their own, a few persons over a few weeks, with named products.

fixture <- function() {
  skeleton <- data.table::copy(mht::fake_skeleton_mht)
  lmed <- data.table::setDT(data.table::copy(mht::fake_lmed_2026))
  lmed[, lnmn := NA_character_]
  lmed[produkt == "Utrogestan", lnmn := "Utrogestan, kapsel, mjuk 100 mg"]
  # `Paracetamol` is a fixture-only name. The table is built from the real
  # deliveries, which write branded spellings instead.
  lmed <- lmed[produkt != "Paracetamol"]
  return(list(skeleton = skeleton, lmed = lmed))
}

# A dropped prescription warns. The fixture holds one negative-duration row,
# and items 12 and 13 each hold a row with no duration. Muffle only that
# warning, so any other warning still reaches the reporter.
muffle_dropped <- function(expr) {
  return(withCallingHandlers(expr, warning = function(w) {
    if (grepl("prescriptions dropped", conditionMessage(w), fixed = TRUE)) {
      invokeRestart("muffleWarning")
    }
  }))
}

# Both entry points on the prepared fixture. Each call takes seconds, so the
# file runs them once and every test that reads them shares the result.
runs <- new.env()
fixture_runs <- function() {
  if (is.null(runs$new)) {
    f_old <- fixture()
    f_new <- fixture()
    runs$old <- muffle_dropped(
      add_lmed_v20260902(f_old$skeleton, f_old$lmed, verbose = FALSE)
    )
    runs$new <- muffle_dropped(
      add_lmed_v20260922(f_new$skeleton, f_new$lmed, verbose = FALSE)
    )
  }
  return(list(old = runs$old, new = runs$new))
}

# Every column of `actual` against the same column of `expected`, by name.
expect_same_columns <- function(actual, expected, rows = TRUE) {
  expect_identical(names(actual), names(expected))
  for (k in names(expected)) {
    expect_identical(actual[[k]][rows], expected[[k]][rows], info = k)
  }
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

# One dispensing per element. No product used here needs a strength, so
# `lnmn` is NA.
dispensings <- function(lopnr, produkt, edatum, fddd) {
  return(data.table::data.table(
    lopnr = lopnr,
    produkt = produkt,
    edatum = as.Date(edatum),
    fddd = fddd,
    lnmn = NA_character_
  ))
}

test_that("1. the columns, their order and their types match 2026-09-02", {
  r <- fixture_runs()
  expect_identical(names(r$new), names(r$old))
  expect_identical(lapply(r$new, class), lapply(r$old, class))
  expect_identical(
    vapply(r$new, typeof, character(1)),
    vapply(r$old, typeof, character(1))
  )
  added <- setdiff(names(r$new), names(mht::fake_skeleton_mht))
  expect_length(added, 46L)
})

test_that("2. a person unflagged in both, holding no A7, is unchanged", {
  r <- fixture_runs()
  f <- fixture()
  a7 <- union(
    mht:::lmed_read_product_table_v20260902()[classification == "A7"],
    mht:::lmed_read_product_table_v20260922()[classification == "A7"]
  )$produkt_clean
  holds_a7 <- f$lmed[
    mht:::lmed_normalize_product_name_v20260828(produkt) %chin% a7
  ]$lopnr
  flagged <- union(
    r$old[ri_mht_excluded_product == TRUE]$id,
    r$new[ri_mht_excluded_product == TRUE]$id
  )
  keep <- setdiff(unique(r$old$id), union(holds_a7, flagged))
  # Id 8 is the fixture's only A7 person and its only flagged person.
  expect_identical(keep, setdiff(unique(r$old$id), 8L))
  expect_same_columns(r$new, r$old, rows = r$old$id %in% keep)
})

test_that("3. id 8 differs only in the approach and rd_approach columns", {
  r <- fixture_runs()
  her <- r$old$id == 8L
  expect_true(all(r$old$ri_mht_excluded_product[her]))
  expect_true(all(r$new$ri_mht_excluded_product[her]))
  same <- vapply(
    names(r$old),
    function(k) identical(r$new[[k]][her], r$old[[k]][her]),
    logical(1)
  )
  differs <- names(r$old)[!same]
  approaches <- c("approach1", "approach2", "approach3")
  allowed <- c(approaches, grep("^rd_approach", names(r$old), value = TRUE))
  expect_true(any(approaches %in% differs))
  expect_true(all(differs %in% allowed), info = toString(differs))
  categories <- mht:::lmed_materialised_categories_v20260902()
  expect_length(categories, 33L)
  person <- c("ri_mht_excluded_product", "ri_mht_excluded_reason")
  for (k in c(categories, person)) {
    expect_identical(r$new[[k]][her], r$old[[k]][her], info = k)
  }
})

test_that("4. an Estradiol Valerate dispensing flags her and lights A7", {
  skeleton_before <- as.data.frame(data.table::copy(mht::fake_skeleton_mht))
  lmed_before <- as.data.frame(data.table::copy(mht::fake_lmed_2026))
  f <- fixture()
  # Id 18 holds only Paracetamol, which the preparation drops.
  expect_identical(nrow(f$lmed[lopnr == 18L]), 0L)
  ev <- dispensings(18L, "Estradiol Valerate", "2016-05-23", 28)
  lmed <- rbind(f$lmed, ev)
  new <- muffle_dropped(add_lmed_v20260922(
    data.table::copy(f$skeleton),
    lmed,
    verbose = FALSE
  ))
  old <- muffle_dropped(add_lmed_v20260902(
    data.table::copy(f$skeleton),
    lmed,
    verbose = FALSE
  ))
  week <- cstime::date_to_isoyearweek_c(ev$edatum)
  her_new <- new[id == 18L]
  her_old <- old[id == 18L]
  expect_true(all(her_new$ri_mht_excluded_product))
  expect_identical(
    unique(her_new$ri_mht_excluded_reason),
    "possible gender-affirming therapy"
  )
  expect_true(her_new[isoyearweek == week]$A7)
  expect_false(any(her_old$ri_mht_excluded_product))
  expect_true(all(is.na(her_old$ri_mht_excluded_reason)))
  expect_false(any(her_old$A7))
  expect_identical(as.data.frame(mht::fake_skeleton_mht), skeleton_before)
  expect_identical(as.data.frame(mht::fake_lmed_2026), lmed_before)
})

test_that("5. a progestogen-only person gets no oestrogen approach level", {
  r <- fixture_runs()
  categories <- mht:::lmed_materialised_categories_v20260902()
  lit <- c("11" = "D2", "12" = "D3", "13" = "E1", "14" = "E1")
  for (i in names(lit)) {
    p <- r$new[id == as.integer(i)]
    on <- categories[vapply(categories, function(k) any(p[[k]]), logical(1))]
    expect_identical(on, lit[[i]], info = i)
    levels <- c(p$approach1, p$approach2, p$approach3)
    oestrogen <- startsWith(levels, "estrogen_progesterone_") |
      levels == "estrogen_only"
    expect_false(any(oestrogen), info = i)
  }
})

test_that("6. an A7-only week counts as no treatment in every approach", {
  for (product in c("Neofollin", "Estradiol Valerate")) {
    skeleton <- weekly_grid(1L, n = 52L)
    lmed <- dispensings(1L, product, "2016-05-23", 28)
    add_lmed_v20260922(skeleton, lmed, verbose = FALSE)
    week <- cstime::date_to_isoyearweek_c(lmed$edatum)
    expect_true(skeleton[isoyearweek == week]$A7, info = product)
    for (a in c("approach1", "approach2", "approach3")) {
      expect_true(
        all(skeleton[[a]] == "local_or_none_mht"),
        info = paste(product, a)
      )
    }
  }
})

test_that("7. one call opens only the two 2026-09-22 workbooks", {
  skeleton <- weekly_grid(1:2)
  lmed <- dispensings(
    lopnr = c(1L, 2L),
    produkt = c("Divigel", "Neofollin"),
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
  add_lmed_v20260922(skeleton, lmed, verbose = FALSE)
  expect_identical(
    sort(unique(seen$files)),
    c("dataDictionary20260922.xlsx", "product_table_20260922.xlsx")
  )
})

test_that("8. create_rd = FALSE writes no rd column and changes no other", {
  skeleton <- weekly_grid(1:3)
  lmed <- dispensings(
    lopnr = c(1L, 2L, 3L),
    produkt = c("Divigel", "Progynon", "Vagifem"),
    edatum = c("2016-01-11", "2016-03-07", "2016-01-18"),
    fddd = c(56, 28, 90)
  )
  with_rd <- add_lmed_v20260922(
    data.table::copy(skeleton),
    lmed,
    verbose = FALSE
  )
  expect_length(grep("^rd_approach", names(with_rd)), 8L)

  without <- data.table::copy(skeleton)
  without[, rd_approach_stale := "from an earlier run"]
  add_lmed_v20260922(without, lmed, create_rd = FALSE, verbose = FALSE)
  expect_identical(
    grep("^rd_approach", names(without), value = TRUE),
    character(0)
  )
  kept <- grep("^rd_approach", names(with_rd), value = TRUE, invert = TRUE)
  expect_same_columns(without, with_rd[, kept, with = FALSE])

  old <- data.table::copy(skeleton)
  old[, rd_approach_stale := "from an earlier run"]
  add_lmed_v20260902(old, lmed, create_rd = FALSE, verbose = FALSE)
  expect_identical(names(without), names(old))
})

test_that("9. the caller's row order and key columns are kept", {
  skeleton <- weekly_grid(1:3)
  lmed <- dispensings(
    lopnr = c(1L, 2L, 3L, 3L),
    produkt = c("Divigel", "Estradiol Valerate", "Estalis", "Cerazette"),
    edatum = c("2016-01-11", "2016-02-01", "2016-01-18", "2016-01-18"),
    fddd = c(56, 28, 56, 56)
  )
  sorted <- add_lmed_v20260922(
    data.table::copy(skeleton),
    lmed,
    verbose = FALSE
  )
  order <- withr::with_seed(20260922, sample.int(nrow(skeleton)))
  shuffled <- skeleton[order]
  keys <- data.table::copy(shuffled)
  add_lmed_v20260922(shuffled, lmed, verbose = FALSE)
  expect_identical(shuffled$id, keys$id)
  expect_identical(shuffled$isoyearweek, keys$isoyearweek)
  expect_same_columns(shuffled, sorted[order])
})

test_that("10. a second call gives the table the first call gives", {
  skeleton <- weekly_grid(1:3)
  lmed <- dispensings(
    lopnr = c(1L, 2L, 3L),
    produkt = c("Progynon", "Neofollin", "Divigel"),
    edatum = c("2016-01-11", "2016-02-01", "2016-03-07"),
    fddd = c(56, 28, 56)
  )
  once <- add_lmed_v20260922(
    data.table::copy(skeleton),
    lmed,
    verbose = FALSE
  )
  add_lmed_v20260922(skeleton, lmed, verbose = FALSE)
  add_lmed_v20260922(skeleton, lmed, verbose = FALSE)
  expect_same_columns(skeleton, once)
})

test_that("11a. a non-default id column is read by name, not as lopnr", {
  skeleton <- weekly_grid(1:3)
  lmed <- dispensings(
    lopnr = c(1L, 2L, 3L),
    produkt = c("Divigel", "Estradiol Valerate", "Vagifem"),
    edatum = c("2016-01-11", "2016-02-01", "2016-01-18"),
    fddd = c(56, 28, 90)
  )
  by_default <- add_lmed_v20260922(
    data.table::copy(skeleton),
    lmed,
    verbose = FALSE
  )
  renamed <- data.table::copy(lmed)
  data.table::setnames(renamed, "lopnr", "person")
  # A decoy `lopnr` column that names another person on every row. A call that
  # read `lopnr` instead of `id_name` would give each person another's product.
  renamed[, lopnr := c(2L, 3L, 1L)]
  by_name <- add_lmed_v20260922(
    data.table::copy(skeleton),
    renamed,
    id_name = "person",
    verbose = FALSE
  )
  expect_same_columns(by_name, by_default)
})

test_that("11b. a non-default id column needs no lopnr column", {
  skeleton <- weekly_grid(1:3)
  lmed <- dispensings(
    lopnr = c(1L, 2L, 3L),
    produkt = c("Divigel", "Estradiol Valerate", "Vagifem"),
    edatum = c("2016-01-11", "2016-02-01", "2016-01-18"),
    fddd = c(56, 28, 90)
  )
  by_default <- add_lmed_v20260922(
    data.table::copy(skeleton),
    lmed,
    verbose = FALSE
  )
  renamed <- data.table::copy(lmed)
  data.table::setnames(renamed, "lopnr", "person")
  expect_false("lopnr" %in% names(renamed))
  by_name <- add_lmed_v20260922(
    data.table::copy(skeleton),
    renamed,
    id_name = "person",
    verbose = FALSE
  )
  expect_same_columns(by_name, by_default)
})

test_that("12. the call leaves the caller's lmed unmodified", {
  skeleton <- weekly_grid(1:2)
  # Id 5 is outside the skeleton, and the Neofollin row has no duration, so
  # the call restricts the rows and drops one.
  lmed <- dispensings(
    lopnr = c(1L, 2L, 5L),
    produkt = c("Divigel", "Neofollin", "Progynon"),
    edatum = c("2016-01-11", "2016-02-01", "2016-01-11"),
    fddd = c(56, NA, 28)
  )
  before <- data.table::copy(lmed)
  muffle_dropped(add_lmed_v20260922(skeleton, lmed, verbose = FALSE))
  expect_identical(lmed, before)
})

test_that("13. the Estradiol Valerate flag needs no duration or grid week", {
  # The 2026-09-02 layer flags Delestrogen in both cases, so the 2026-09-22
  # layer must flag Estradiol Valerate in both.
  cases <- list(
    no_duration = list(edatum = "2016-02-01", fddd = NA_real_),
    before_grid = list(edatum = "1990-01-01", fddd = 28)
  )
  for (nm in names(cases)) {
    skeleton <- weekly_grid(1:2)
    run <- function(fun, produkt) {
      lmed <- dispensings(
        lopnr = c(1L, 2L),
        produkt = c("Divigel", produkt),
        edatum = c("2016-01-11", cases[[nm]]$edatum),
        fddd = c(56, cases[[nm]]$fddd)
      )
      return(muffle_dropped(fun(
        data.table::copy(skeleton),
        lmed,
        verbose = FALSE
      )))
    }
    new <- run(add_lmed_v20260922, "Estradiol Valerate")
    old <- run(add_lmed_v20260902, "Delestrogen")
    for (out in list(new, old)) {
      her <- out[id == 2L]
      expect_true(all(her$ri_mht_excluded_product), info = nm)
      expect_identical(
        unique(her$ri_mht_excluded_reason),
        "possible gender-affirming therapy",
        info = nm
      )
      expect_false(any(her$A7), info = nm)
      expect_false(any(out[id == 1L]$ri_mht_excluded_product), info = nm)
    }
  }
})

test_that("14. create_rd = FALSE then TRUE restores the rd columns", {
  skeleton <- weekly_grid(1:3)
  lmed <- dispensings(
    lopnr = c(1L, 1L, 3L),
    produkt = c("Divigel", "Progynon", "Estalis"),
    edatum = c("2016-01-11", "2016-04-04", "2016-01-18"),
    fddd = c(56, 28, 56)
  )
  fresh <- add_lmed_v20260922(
    data.table::copy(skeleton),
    lmed,
    verbose = FALSE
  )
  add_lmed_v20260922(skeleton, lmed, create_rd = FALSE, verbose = FALSE)
  add_lmed_v20260922(skeleton, lmed, verbose = FALSE)
  expect_length(grep("^rd_approach", names(fresh)), 8L)
  expect_setequal(names(skeleton), names(fresh))
  for (k in names(fresh)) {
    expect_identical(skeleton[[k]], fresh[[k]], info = k)
  }
})
