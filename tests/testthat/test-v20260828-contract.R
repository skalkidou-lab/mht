# Pins the contract of `add_lmed_v20260828()`, the 2026-08-28 entry point.
#
# The subjects are:
#
#   * the return value
#   * the caller's row order
#   * the caller's other columns
#   * a repeated call
#   * a stale `rd_approach*` column
#   * the `id_name` argument
#
# THE WHOLE PIPELINE RUNS ON A PRIVATE TABLE. Every working name the layer
# writes lives on that table, so a caller column of the same name is neither
# overwritten nor deleted. Two tests below give the caller such a column and
# read it back.

library(data.table)

absorb <- function(expr) {
  tryCatch(expr, error = function(e) paste0("ERROR: ", conditionMessage(e)))
}

grid <- function(ids = 1L, from = "2010-01-04", n = 300L) {
  weeks <- cstime::date_to_isoyearweek_c(
    seq(as.Date(from), by = "week", length.out = n)
  )
  return(CJ(id = ids, isoyearweek = weeks, sorted = TRUE))
}

# Divigel is transdermal oestrogen. One year of it lights `systemic_mht`, and
# it takes no strength-keyed codebook rule, so it needs no `lnmn`.
divigel <- function(ids = 1L, edatum = "2010-06-07") {
  return(data.table(
    lopnr = ids,
    produkt = "Divigel",
    edatum = as.Date(edatum),
    fddd = 365
  ))
}

# A prescription table whose person identifier is named
# `p1163_lopnr_personnr` rather than `lopnr`.
renamed_id <- function(ids = 1L) {
  d <- divigel(ids)
  setnames(d, "lopnr", "p1163_lopnr_personnr")
  return(d)
}

# Every name the layer writes on its private table and then deletes. The
# approach variables come from the codebook, so a codebook edit reaches this
# list with no test change.
working_names <- function() {
  rules <- mht:::lmed_read_approach_rules_v20260828()
  vars <- unique(vapply(rules, function(r) r$variable, character(1)))
  return(c(
    "run_min",
    "tied_n",
    "on_mht",
    "episode",
    "carry",
    vars,
    paste0("run_", vars),
    "var_to_clean",
    "var_to_clean_lag1",
    "isoyearweek_first_previous",
    "temp",
    "reinitiation_isoyearweek",
    "n",
    "length_on_mht",
    "last_session_on_mht"
  ))
}

# ==================================================== the caller's own columns ====

test_that("a caller column named n or local_or_none_mht survives the call", {
  s <- grid()
  s[, n := "caller n"]
  s[, local_or_none_mht := "caller local_or_none_mht"]
  add_lmed_v20260828(s, divigel(), verbose = FALSE)

  expect_identical(absorb(s[["n"]]), rep("caller n", 300L))
  expect_identical(
    absorb(s[["local_or_none_mht"]]),
    rep("caller local_or_none_mht", 300L)
  )
})

test_that("every working name of the layer survives as a caller column", {
  names_used <- working_names()
  s <- grid()
  for (nm in names_used) {
    s[, (nm) := paste0("caller ", nm)]
  }
  absorb(add_lmed_v20260828(s, divigel(), verbose = FALSE))

  # 21 approach-layer names and 8 exposure-layer names, on the 2026-08-28
  # codebook. The approach layer holds 5 fixed names, the 8 variables of the
  # `post_grouping` sheet, and one `run_` name per variable. The count is
  # asserted. A codebook edit that drops a variable then fails this test.
  expect_identical(length(names_used), 29L)
  lost <- names_used[!names_used %in% names(s)]
  expect_identical(absorb(lost), character(0))
  changed <- names_used[vapply(
    names_used,
    function(nm) !identical(s[[nm]], rep(paste0("caller ", nm), 300L)),
    logical(1)
  )]
  expect_identical(absorb(changed), character(0))
})

# ============================================================== the row order ====

test_that("the caller's row order is the row order on return", {
  lmed <- data.table(
    lopnr = 1:3,
    produkt = "Divigel",
    edatum = as.Date(c("2010-06-07", "2010-09-06", "2010-03-01")),
    fddd = 365
  )

  sorted <- grid(1:3, "2010-01-04", 60L)
  add_lmed_v20260828(sorted, lmed, verbose = FALSE)

  withr::with_seed(20260828L, {
    shuffled <- grid(1:3, "2010-01-04", 60L)[sample(180L)]
  })
  before_id <- copy(shuffled[["id"]])
  before_week <- copy(shuffled[["isoyearweek"]])
  add_lmed_v20260828(shuffled, lmed, verbose = FALSE)

  expect_identical(shuffled[["id"]], before_id)
  expect_identical(shuffled[["isoyearweek"]], before_week)

  # The values must follow the rows they belong to, not the layer's sort.
  expected <- sorted[
    data.table(id = before_id, isoyearweek = before_week),
    on = .(id, isoyearweek),
    approach1
  ]
  expect_identical(absorb(shuffled[["approach1"]]), expected)
  expect_false(identical(shuffled[["approach1"]], sorted[["approach1"]]))
})

# ============================================================ the return value ====

test_that("the call returns the skeleton, invisibly", {
  s <- grid()
  r <- add_lmed_v20260828(s, divigel(), verbose = FALSE)

  expect_true(data.table::is.data.table(r))
  expect_identical(r, s)
  expect_identical(data.table::address(r), data.table::address(s))

  s2 <- grid()
  seen <- withVisible(add_lmed_v20260828(s2, divigel(), verbose = FALSE))
  expect_false(seen$visible)
  expect_identical(seen$value, s2)
})

# ============================================================= a repeated call ====

test_that("a second call gives the first call's table", {
  s <- grid()
  add_lmed_v20260828(s, divigel(), verbose = FALSE)
  first <- copy(s)

  add_lmed_v20260828(s, divigel(), verbose = FALSE)
  expect_identical(s, first)

  # The second call recomputes. It does not accept the columns it finds.
  s[, approach1 := "wrong"]
  s[, A1 := TRUE]
  s[, rd_approach1_single := "wrong"]
  add_lmed_v20260828(s, divigel(), verbose = FALSE)
  expect_identical(s, first)
})

# ================================================== a stale rd_approach column ====

test_that("create_rd = FALSE removes an rd_approach column the caller carries", {
  s <- grid()
  s[, rd_approach1_single := "caller rd_approach1_single"]
  add_lmed_v20260828(s, divigel(), create_rd = FALSE, verbose = FALSE)

  expect_identical(
    absorb(grep("^rd_approach", names(s), value = TRUE)),
    character(0)
  )
  expect_identical(absorb(s[["rd_approach1_single"]]), NULL)
})

test_that("create_rd = TRUE after create_rd = FALSE rebuilds the eight columns", {
  s <- grid()
  add_lmed_v20260828(s, divigel(), create_rd = TRUE, verbose = FALSE)
  first <- copy(s)

  add_lmed_v20260828(s, divigel(), create_rd = FALSE, verbose = FALSE)
  expect_identical(
    absorb(grep("^rd_approach", names(s), value = TRUE)),
    character(0)
  )

  add_lmed_v20260828(s, divigel(), create_rd = TRUE, verbose = FALSE)
  expect_identical(s, first)
})

# ================================================================ the id_name ====

test_that("id_name selects the identifier column of the prescription table", {
  s <- grid()
  add_lmed_v20260828(
    s,
    renamed_id(),
    id_name = "p1163_lopnr_personnr",
    verbose = FALSE
  )
  expect_identical(absorb(sum(s[["A1"]])), 53L)
  expect_identical(
    absorb(unique(s[["approach1"]])),
    c("local_or_none_mht", "systemic_mht")
  )

  # `add_lmed_v20250909()` takes no `id_name` and reads the identifier from
  # `lopnr`. The two values below pin what it writes for a prescription table
  # that names the identifier otherwise.
  v20250909 <- grid()
  suppressWarnings(add_lmed_v20250909(v20250909, renamed_id(), verbose = FALSE))
  expect_identical(absorb(sum(v20250909[["A1"]])), 0L)
  expect_identical(
    absorb(unique(v20250909[["approach1"]])),
    "local_or_none_mht"
  )
})

test_that("an id_name that names no column of lmed is an error", {
  s <- grid()
  expect_identical(
    absorb(add_lmed_v20260828(s, divigel(), id_name = "nope", verbose = FALSE)),
    "ERROR: lmed has no column nope"
  )
  expect_identical(names(s), c("id", "isoyearweek"))
})

# ======================================================== the written columns ====

test_that("the call writes the documented columns and no other", {
  s <- grid()
  add_lmed_v20260828(s, divigel(), verbose = FALSE)

  written <- setdiff(names(s), c("id", "isoyearweek"))
  categories <- grep("^[A-Z][0-9]+$", written, value = TRUE)
  expect_identical(length(categories), 33L)
  expect_identical(
    absorb(setdiff(written, categories)),
    c(
      "approach1",
      "approach2",
      "approach3",
      "rd_approach1_single",
      "rd_approach1_multiple",
      "rd_approach2_single",
      "rd_approach2_multiple",
      "rd_approach3_single",
      "rd_approach3_multiple",
      "rd_approach3b_single",
      "rd_approach3b_multiple"
    )
  )
})

test_that("create_rd = FALSE writes the categories and the approaches alone", {
  s <- grid()
  add_lmed_v20260828(s, divigel(), create_rd = FALSE, verbose = FALSE)

  expect_identical(
    absorb(setdiff(names(s), c("id", "isoyearweek"))[34:36]),
    c("approach1", "approach2", "approach3")
  )
  expect_identical(ncol(s), 38L)
})

test_that("the caller's lmed is not modified", {
  lmed <- divigel()
  before <- copy(lmed)
  s <- grid()
  add_lmed_v20260828(s, lmed, verbose = FALSE)

  expect_identical(lmed, before)
})

# ============================================================ rejected inputs ====
#
# A rejected call reports the fault and leaves the caller's object as it was.

test_that("a person who holds one ISO week twice is an error", {
  s <- rbind(grid(1L, "2010-01-04", 3L), grid(1L, "2010-01-04", 1L))
  expect_identical(
    absorb(add_lmed_v20260828(s, divigel(), verbose = FALSE)),
    paste(
      "ERROR: the check fails for 1 of 1 persons. Each person must hold each",
      "ISO week once, consecutively. id 1 has 4 rows, 3 distinct weeks,",
      "spanning 3 weeks"
    )
  )
  expect_identical(names(s), c("id", "isoyearweek"))
})

test_that("a gap in a person's weeks is an error", {
  s <- grid(1L, "2010-01-04", 5L)[-3]
  expect_identical(
    absorb(add_lmed_v20260828(s, divigel(), verbose = FALSE)),
    paste(
      "ERROR: the check fails for 1 of 1 persons. Each person must hold each",
      "ISO week once, consecutively. id 1 has 4 rows, 4 distinct weeks,",
      "spanning 5 weeks"
    )
  )
  expect_identical(names(s), c("id", "isoyearweek"))
})

test_that("an isoyearweek that is not YYYY-WW is an error", {
  # The exposure layer compares ISO weeks as strings, against a `9999-99`
  # sentinel. `2010-1` sorts after `2010-09`, so the comparison would read a
  # later week as an earlier one.
  s <- grid()
  s[1, isoyearweek := "2010-1"]
  expect_identical(
    absorb(add_lmed_v20260828(s, divigel(), verbose = FALSE)),
    "ERROR: skeleton$isoyearweek holds a value that is no ISO week: 2010-1"
  )
  expect_identical(names(s), c("id", "isoyearweek"))
})

test_that("a skeleton that is no data.table is an error", {
  s <- as.data.frame(grid())
  expect_identical(
    absorb(add_lmed_v20260828(s, divigel(), verbose = FALSE)),
    "ERROR: skeleton must be a data.table"
  )
})
