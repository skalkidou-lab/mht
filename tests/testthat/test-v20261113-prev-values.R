# add_lmed_v20261113() writes the 28 columns of add_lmed_v20261112(). A
# former-use week is `previous3y` in a `_prev3y` column and `previousany` in a
# `_prevany` column, where add_lmed_v20261112() writes `previous` in both.
# Every other cell is the same.
#
# These tests do not call skip_on_cran(). The discriminator runs them
# non-interactively and without NOT_CRAN, where skip_on_cran() skips them.
#
# Toy week 1 starts on Monday 2020-01-06, and every dispensing falls on a
# Monday. Divigel takes no minimum-dose rule and no fixed duration, so a
# supply of 7 * n days covers n weeks.

UTROGESTAN_LNMN <- "Utrogestan, kapsel, mjuk 100 mg"
TOY_FIRST <- as.Date("2020-01-06")
RD_BASE <- c(outer(
  paste0("rd_approach", c(1:6, "3b")),
  c("_single", "_multiple"),
  paste0
))
RULE_VALUES <- c(prev3y = "previous3y", prevany = "previousany")
RD_NEW <- c(outer(RD_BASE, paste0("_", names(RULE_VALUES)), paste0))

# The rule suffix of an rd column: "prev3y" or "prevany".
rule_of <- function(col) {
  return(sub("^.*_", "", col))
}

# The fixture of test-v20261112-prev-rules.R.
fixture <- function() {
  skeleton <- data.table::copy(mht::fake_skeleton_mht)
  lmed <- data.table::setDT(data.table::copy(mht::fake_lmed_2026))
  lmed[, lnmn := NA_character_]
  lmed[produkt == "Utrogestan", lnmn := UTROGESTAN_LNMN]
  lmed <- lmed[produkt != "Paracetamol"]
  return(list(skeleton = skeleton, lmed = lmed))
}

# A dropped prescription warns. The fixture holds one negative-duration row.
muffle_dropped <- function(expr) {
  return(withCallingHandlers(expr, warning = function(w) {
    if (grepl("prescriptions dropped", conditionMessage(w), fixed = TRUE)) {
      invokeRestart("muffleWarning")
    }
  }))
}

# One prescription for person 1 on the Monday of `start_week`, n weeks long.
rx <- function(produkt, start_week, n_weeks) {
  return(data.table::data.table(
    lopnr = 1L,
    produkt = produkt,
    edatum = TOY_FIRST + 7L * (start_week - 1L),
    fddd = 7 * n_weeks,
    lnmn = NA_character_
  ))
}

run_toy <- function(lmed, n_weeks) {
  skel <- data.table::data.table(
    id = 1L,
    isoyearweek = cstime::date_to_isoyearweek_c(
      TOY_FIRST + 7L * (seq_len(n_weeks) - 1L)
    )
  )
  mht::add_lmed_v20261113(skel, lmed, verbose = FALSE)
  return(skel[])
}

run_fixture <- function(fn) {
  f <- fixture()
  muffle_dropped(fn(f$skeleton, f$lmed, verbose = FALSE))
  return(f$skeleton[])
}

test_that("e. each rd column is the 2026-11-12 column with previous mapped to the rule's value", {
  old <- run_fixture(mht::add_lmed_v20261112)
  new <- run_fixture(mht::add_lmed_v20261113)

  expect_identical(names(new), names(old))
  rd <- grep("^rd_approach", names(new), value = TRUE)
  expect_length(rd, 28L)
  expect_setequal(rd, RD_NEW)

  for (col in RD_NEW) {
    value <- RULE_VALUES[[rule_of(col)]]
    former <- old[[col]] == "previous"
    # The equality below is vacuous for a column without a former-use week.
    expect_gt(sum(former), 0L, label = paste("former-use weeks in", col))
    expected <- old[[col]]
    expected[former] <- value
    expect_identical(new[[col]], expected, info = paste(col, value))
    expect_false(any(new[[col]] == "previous"), info = col)
  }

  for (col in setdiff(names(old), RD_NEW)) {
    expect_identical(new[[col]], old[[col]], info = col)
  }
  for (col in names(new)) {
    expect_false(isTRUE(any(new[[col]] == "previous")), info = col)
  }
})

test_that("c1. Divigel for 155 weeks then a stop: prev3y exclude, prevany previousany", {
  s <- run_toy(rx("Divigel", 1, 155), 160L)
  expect_identical(s$approach1[1:155], rep("systemic_mht", 155L))
  for (p in c("single", "multiple")) {
    expect_identical(
      s[[paste0("rd_approach1_", p, "_prev3y")]][156:160],
      rep("exclude", 5L)
    )
    expect_identical(
      s[[paste0("rd_approach1_", p, "_prevany")]][156:160],
      rep("previousany", 5L)
    )
  }
})

test_that("c2. Divigel for 156 weeks then a stop: previous3y and previousany", {
  s <- run_toy(rx("Divigel", 1, 156), 160L)
  rd <- grep("^rd_approach", names(s), value = TRUE)
  expect_setequal(rd, RD_NEW)
  for (col in rd) {
    expect_identical(
      s[[col]][157:160],
      rep(RULE_VALUES[[rule_of(col)]], 4L),
      info = col
    )
  }
})

test_that("c3. a 10-week episode then a stop is previousany under prevany", {
  s <- run_toy(rx("Divigel", 5, 10), 30L)
  prevany <- grep("_prevany$", names(s), value = TRUE)
  prev3y <- grep("_prev3y$", names(s), value = TRUE)
  expect_length(prevany, 14L)
  expect_length(prev3y, 14L)
  for (col in prevany) {
    expect_identical(s[[col]][15:30], rep("previousany", 16L), info = col)
  }
  for (col in prev3y) {
    expect_identical(s[[col]][15:30], rep("exclude", 16L), info = col)
  }
})
