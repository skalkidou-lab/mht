# Pins the 2026-08-28 person-week arithmetic: the closed exposure interval, the
# gap bridge bounded on both sides, the skeleton continuity the row counting
# needs, and the clipping a person's first retained week imposes.

library(data.table)

absorb <- function(expr) {
  tryCatch(expr, error = function(e) paste0("ERROR: ", conditionMessage(e)))
}

# A person-week grid of consecutive ISO weeks, one person per id.
grid <- function(ids, from, to) {
  weeks <- cstime::date_to_isoyearweek_c(
    seq(as.Date(from), as.Date(to), by = "week")
  )
  CJ(id = ids, isoyearweek = weeks, sorted = TRUE)
}

flag_weeks <- function(skeleton, lmed) {
  d <- mht:::lmed_durations_v20260828(lmed, verbose = FALSE)
  mht:::apply_lmed_categories_to_skeleton_v20260828(
    skeleton,
    d[, .(lopnr, start_isoyearweek, stop_isoyearweek, product_category)],
    verbose = FALSE
  )
  invisible(skeleton)
}

# -------------------------------------------------- the interval is inclusive ----

test_that("one day of supply covers one ISO week, not two", {
  # 2020-01-05 is a Sunday, the last day of ISO week 2020-01. The next day is
  # the Monday of 2020-02, so the frozen `edatum + round(fddd)` crossed the
  # boundary and covered two weeks for one day of supply.
  expect_identical(
    cstime::date_to_isoyearweek_c(as.Date(c("2020-01-05", "2020-01-06"))),
    c("2020-01", "2020-02")
  )
  d <- mht:::lmed_durations_v20260828(
    data.table(
      lopnr = 1L,
      produkt = "Divigel",
      edatum = as.Date("2020-01-05"),
      fddd = 1
    ),
    verbose = FALSE
  )
  expect_identical(d$duration_days, 1)
  expect_identical(d$start_isoyearweek, "2020-01")
  expect_identical(d$stop_isoyearweek, "2020-01")

  skeleton <- grid(1L, "2019-12-30", "2020-03-30")
  flag_weeks(
    skeleton,
    data.table(
      lopnr = 1L,
      produkt = "Divigel",
      edatum = as.Date("2020-01-05"),
      fddd = 1
    )
  )
  expect_identical(sum(skeleton$A1), 1L)
  expect_identical(skeleton$isoyearweek[skeleton$A1], "2020-01")
})

test_that("the frozen layer covers two weeks for the same one day of supply", {
  # The `before` side of the pin above, driven through the frozen entry point.
  skeleton <- grid(1L, "2019-12-30", "2020-03-30")
  suppressWarnings(add_lmed_v20250909(
    skeleton,
    data.table(
      lopnr = 1L,
      produkt = "Divigel",
      edatum = as.Date("2020-01-05"),
      fddd = 1
    ),
    verbose = FALSE
  ))
  expect_identical(sum(skeleton$A1), 2L)
  expect_identical(skeleton$isoyearweek[skeleton$A1], c("2020-01", "2020-02"))
})

test_that("a whole treatment month covers exactly four ISO weeks", {
  d <- mht:::lmed_durations_v20260828(
    data.table(
      lopnr = 1L,
      produkt = "Utrogestan",
      lnmn = "Utrogestan, kapsel, mjuk 100 mg",
      edatum = as.Date("2020-01-06"),
      fddd = 30
    ),
    verbose = FALSE
  )
  expect_identical(d$duration_days, 28)
  expect_identical(d$start_isoyearweek, "2020-02")
  expect_identical(d$stop_isoyearweek, "2020-05")
})

# ------------------------------------------------- the gap bridge, both sides ----

test_that("the bridge fills an internal gap and leaves the two ends alone", {
  bridge <- mht:::replace_false_runs_v20260828
  frozen <- mht:::replace_false_runs_v20250909
  x <- c(
    FALSE,
    FALSE,
    TRUE,
    TRUE,
    FALSE,
    FALSE,
    FALSE,
    TRUE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    TRUE,
    FALSE,
    FALSE
  )
  # The frozen helper converted the leading and the trailing run too, so a
  # woman started treatment up to four weeks early and stopped four weeks late.
  expect_identical(
    frozen(x),
    c(
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      TRUE,
      TRUE,
      TRUE
    )
  )
  expect_identical(
    bridge(x),
    c(
      FALSE,
      FALSE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      TRUE,
      FALSE,
      FALSE
    )
  )
})

test_that("the bridge fills four weeks and leaves five", {
  bridge <- mht:::replace_false_runs_v20260828
  four <- c(TRUE, FALSE, FALSE, FALSE, FALSE, TRUE)
  five <- c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)
  expect_identical(bridge(four), rep(TRUE, 6L))
  expect_identical(bridge(five), five)
})

test_that("the bridge leaves a vector with no bounded gap unchanged", {
  bridge <- mht:::replace_false_runs_v20260828
  expect_identical(bridge(rep(FALSE, 3L)), rep(FALSE, 3L))
  expect_identical(bridge(rep(TRUE, 3L)), rep(TRUE, 3L))
  expect_identical(bridge(logical(0)), logical(0))
  expect_identical(bridge(c(FALSE, TRUE)), c(FALSE, TRUE))
  expect_identical(bridge(c(TRUE, FALSE)), c(TRUE, FALSE))
})

# ----------------------------------------------------- the skeleton continuity ----

test_that("a skeleton of consecutive weeks passes, across a 53-week year", {
  ok <- data.table(
    id = rep(1:2, each = 4L),
    isoyearweek = rep(c("2020-51", "2020-52", "2020-53", "2021-01"), 2L)
  )
  expect_identical(absorb(mht:::assert_person_weeks_v20260828(ok)), TRUE)
})

test_that("a repeated week is an error, because gap bridging counts rows", {
  x <- data.table(id = 1L, isoyearweek = c("2020-01", "2020-01", "2020-02"))
  expect_identical(
    absorb(mht:::assert_person_weeks_v20260828(x)),
    paste0(
      "ERROR: the check fails for 1 of 1 persons. Each person must hold each ",
      "ISO week once, consecutively. id 1 has 3 rows, 2 distinct weeks, ",
      "spanning 2 weeks"
    )
  )
})

test_that("a missing week is an error, because the 156-week rule counts rows", {
  x <- data.table(id = 1L, isoyearweek = c("2020-01", "2020-02", "2020-05"))
  expect_identical(
    absorb(mht:::assert_person_weeks_v20260828(x)),
    paste0(
      "ERROR: the check fails for 1 of 1 persons. Each person must hold each ",
      "ISO week once, consecutively. id 1 has 3 rows, 3 distinct weeks, ",
      "spanning 5 weeks"
    )
  )
})

test_that("a value that is no ISO week is an error", {
  x <- data.table(id = 1L, isoyearweek = c("2020-1", "2020-02"))
  expect_identical(
    absorb(mht:::assert_person_weeks_v20260828(x)),
    "ERROR: skeleton$isoyearweek holds a value that is no ISO week: 2020-1"
  )
})

test_that("the person-week grid asserts continuity before it joins anything", {
  # The check is live: it fires through the function every consumer calls,
  # not only when a test calls it directly.
  skeleton <- data.table(id = 1L, isoyearweek = c("2020-01", "2020-05"))
  lmed <- data.table(
    lopnr = 1L,
    produkt = "Divigel",
    edatum = as.Date("2020-01-06"),
    fddd = 10
  )
  expect_identical(
    absorb(flag_weeks(skeleton, lmed)),
    paste0(
      "ERROR: the check fails for 1 of 1 persons. Each person must hold each ",
      "ISO week once, consecutively. id 1 has 2 rows, 2 distinct weeks, ",
      "spanning 5 weeks"
    )
  )
  # Nothing was written before the check fired.
  expect_identical(names(skeleton), c("id", "isoyearweek"))
})

# ------------------------------------------------------------- left truncation ----

test_that("an episode already running at the first retained week is clipped", {
  # DECIDED: duration means weeks observed under follow-up. The layer counts
  # observed weeks only and never reaches behind the first retained week. The
  # 2026 delivery is windowed to 2006 through 2024, so a treatment that started
  # before the window reads as starting in the first observed week.
  skeleton <- grid(1L, "2020-06-29", "2020-12-28")
  expect_identical(skeleton$isoyearweek[1], "2020-27")

  lmed <- data.table(
    lopnr = 1L,
    produkt = "Mirena",
    edatum = as.Date("2018-01-01"),
    fddd = NA_real_
  )
  d <- mht:::lmed_durations_v20260828(lmed, verbose = FALSE)
  expect_identical(d$duration_days, 1680)
  expect_identical(d$start_isoyearweek, "2018-01")

  flag_weeks(skeleton, lmed)
  # The prescription covers 1680 days from 2018-01-01. The skeleton holds 27
  # weeks of it, and the layer reports 27, not 240.
  expect_identical(sum(skeleton$E1), 27L)
  expect_identical(nrow(skeleton), 27L)
  expect_true(all(skeleton$E1))
})

test_that("an episode ending after the last retained week is clipped too", {
  skeleton <- grid(1L, "2020-06-29", "2020-12-28")
  lmed <- data.table(
    lopnr = 1L,
    produkt = "Mirena",
    edatum = as.Date("2020-09-28"),
    fddd = NA_real_
  )
  flag_weeks(skeleton, lmed)
  expect_identical(sum(skeleton$E1), 14L)
  expect_identical(skeleton$isoyearweek[skeleton$E1][1], "2020-40")
})
