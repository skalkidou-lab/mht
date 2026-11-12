# add_lmed_v20261112() writes each rd_approach column twice, once per rule for
# former use. `prev3y` is the 2026-11-05 rule: a stop gives `previous` only
# after an episode of 156 weeks or more. `prevany` gives `previous` after any
# stop. Every other column equals the 2026-11-05 answer.
#
# Toy week 1 starts on Monday 2020-01-06, and every dispensing falls on a
# Monday. Divigel and Vagifem take no minimum-dose rule and no fixed duration,
# so a supply of 7 * n days covers n weeks.

UTROGESTAN_LNMN <- "Utrogestan, kapsel, mjuk 100 mg"
TOY_FIRST <- as.Date("2020-01-06")
RD_BASE <- c(outer(
  paste0("rd_approach", c(1:6, "3b")),
  c("_single", "_multiple"),
  paste0
))
RD_NEW <- c(outer(RD_BASE, c("_prev3y", "_prevany"), paste0))

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
  mht::add_lmed_v20261112(skel, lmed, verbose = FALSE)
  return(skel[])
}

run_fixture <- function(fn) {
  f <- fixture()
  muffle_dropped(fn(f$skeleton, f$lmed, verbose = FALSE))
  return(f$skeleton[])
}

test_that("a. every 2026-11-05 column equals its 2026-11-12 column, rd_X as rd_X_prev3y", {
  skip_on_cran()
  old <- run_fixture(mht::add_lmed_v20261105)
  new <- run_fixture(mht::add_lmed_v20261112)
  expect_true(all(RD_BASE %in% names(old)))
  for (col in setdiff(names(old), c("id", "isoyearweek"))) {
    new_col <- if (col %in% RD_BASE) paste0(col, "_prev3y") else col
    expect_identical(new[[new_col]], old[[col]], info = col)
  }
  expect_setequal(setdiff(names(new), names(old)), RD_NEW)
  expect_setequal(setdiff(names(old), names(new)), RD_BASE)
})

test_that("b. exactly the 28 suffixed rd columns exist, and no unsuffixed one", {
  skip_on_cran()
  new <- run_fixture(mht::add_lmed_v20261112)
  rd <- grep("^rd_approach", names(new), value = TRUE)
  expect_length(rd, 28L)
  expect_setequal(rd, RD_NEW)
  expect_false(any(RD_BASE %in% names(new)))
})

test_that("b2. a skeleton carrying the old unsuffixed columns loses them", {
  skip_on_cran()
  f <- fixture()
  muffle_dropped(mht::add_lmed_v20261105(f$skeleton, f$lmed, verbose = FALSE))
  muffle_dropped(mht::add_lmed_v20261112(f$skeleton, f$lmed, verbose = FALSE))
  expect_false(any(RD_BASE %in% names(f$skeleton)))
  expect_length(grep("^rd_approach", names(f$skeleton)), 28L)
})

test_that("c1. Divigel for 155 weeks then a stop: prev3y exclude, prevany previous", {
  skip_on_cran()
  s <- run_toy(rx("Divigel", 1, 155), 160L)
  expect_identical(s$approach1[1:155], rep("systemic_mht", 155L))
  for (p in c("single", "multiple")) {
    expect_identical(
      s[[paste0("rd_approach1_", p, "_prev3y")]][156:160],
      rep("exclude", 5L)
    )
    expect_identical(
      s[[paste0("rd_approach1_", p, "_prevany")]][156:160],
      rep("previous", 5L)
    )
  }
})

test_that("c2. Divigel for 156 weeks then a stop: previous in both rules", {
  skip_on_cran()
  s <- run_toy(rx("Divigel", 1, 156), 160L)
  for (col in grep("^rd_approach", names(s), value = TRUE)) {
    expect_identical(s[[col]][157:160], rep("previous", 4L), info = col)
  }
})

test_that("c3. a 10-week episode then a stop is previous under prevany", {
  skip_on_cran()
  s <- run_toy(rx("Divigel", 5, 10), 30L)
  for (col in grep("_prevany$", names(s), value = TRUE)) {
    expect_identical(s[[col]][15:30], rep("previous", 16L), info = col)
  }
  for (col in grep("_prev3y$", names(s), value = TRUE)) {
    expect_identical(s[[col]][15:30], rep("exclude", 16L), info = col)
  }
})

test_that("c4. a restart: single is exclude from the restart, multiple re-enters", {
  skip_on_cran()
  # Divigel weeks 1 to 10, a 10-week stop (beyond the 4-week bridge), then
  # Divigel again from week 21 to 30.
  s <- run_toy(rbind(rx("Divigel", 1, 10), rx("Divigel", 21, 10)), 40L)
  for (r in c("prev3y", "prevany")) {
    single <- s[[paste0("rd_approach1_single_", r)]]
    multiple <- s[[paste0("rd_approach1_multiple_", r)]]
    expect_identical(single[21:40], rep("exclude", 20L), info = r)
    expect_identical(multiple[21:30], rep("systemic_mht", 10L), info = r)
  }
  expect_identical(s$rd_approach1_single_prevany[11:20], rep("previous", 10L))
  expect_identical(s$rd_approach1_single_prev3y[11:20], rep("exclude", 10L))
  expect_identical(s$rd_approach1_multiple_prevany[31:40], rep("previous", 10L))
  expect_identical(s$rd_approach1_multiple_prev3y[31:40], rep("exclude", 10L))
})

test_that("c5. approach 6: a local_mht episode follows the same two rules", {
  skip_on_cran()
  short <- run_toy(rx("Vagifem", 1, 155), 160L)
  long <- run_toy(rx("Vagifem", 1, 156), 160L)
  expect_identical(short$approach6[1:155], rep("local_mht", 155L))
  for (p in c("single", "multiple")) {
    expect_identical(
      short[[paste0("rd_approach6_", p, "_prev3y")]][156:160],
      rep("exclude", 5L)
    )
    expect_identical(
      short[[paste0("rd_approach6_", p, "_prevany")]][156:160],
      rep("previous", 5L)
    )
    expect_identical(
      long[[paste0("rd_approach6_", p, "_prev3y")]][157:160],
      rep("previous", 4L)
    )
    expect_identical(
      long[[paste0("rd_approach6_", p, "_prevany")]][157:160],
      rep("previous", 4L)
    )
  }
  # Approach 1 counts local treatment as untreated, so nothing stops.
  expect_true(all(short$rd_approach1_single_prevany == "local_or_none_mht"))
})

test_that("c6. untreated weeks before any treatment keep the reference level in both rules", {
  skip_on_cran()
  expect_identical(
    mht:::reference_levels_v20261105,
    c(
      approach1 = "local_or_none_mht",
      approach2 = "local_or_none_mht",
      approach3 = "local_or_none_mht",
      approach4 = "local_or_none_mht",
      approach5 = "local_or_none_mht",
      approach6 = "no_local_or_systemic_mht"
    )
  )
  s <- run_toy(rx("Divigel", 5, 10), 30L)
  for (col in RD_NEW) {
    reference <- if (startsWith(col, "rd_approach6_")) {
      "no_local_or_systemic_mht"
    } else {
      "local_or_none_mht"
    }
    expect_identical(s[[col]][1:4], rep(reference, 4L), info = col)
  }
})

test_that("d. on the fixture, prevany differs from prev3y only as exclude -> previous", {
  skip_on_cran()
  new <- run_fixture(mht::add_lmed_v20261112)
  n_differ <- 0L
  for (b in RD_BASE) {
    a3 <- new[[paste0(b, "_prev3y")]]
    aa <- new[[paste0(b, "_prevany")]]
    differ <- a3 != aa
    expect_true(all(a3[differ] == "exclude"), info = b)
    expect_true(all(aa[differ] == "previous"), info = b)
    n_differ <- n_differ + sum(differ)
  }
  # The check is vacuous unless the fixture holds a short episode that stops.
  expect_gt(n_differ, 0L)
})
