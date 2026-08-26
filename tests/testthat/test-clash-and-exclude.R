# Pins how `clashingprescriptions` and `exclude` behave TODAY, defects included.
#
# Every expectation here was OBTAINED BY RUNNING
# `create_exposure_variables_v20250909()` on the fixtures below, never derived
# from the function body. Two of them are known defects, frozen on purpose: when
# the repair lands, the diff of this file is the record of what changed.
#
# The subject is the transition rule that creates `previous`. It fires when a
# person moves from an active level to `local_or_none_mht`. Which levels count
# as active decides whether a woman who stops treatment is recorded as a former
# user, and that in turn decides whether she can re-enter a new-user analysis.

on_mht <- "systemic_mht"
off_mht <- "local_or_none_mht"
clash <- "clashingprescriptions"
censored <- "exclude"

iso_weeks <- function(n) {
  i <- seq_len(n) - 1L
  sprintf("%04d-%02d", 2000L + i %/% 52L, i %% 52L + 1L)
}

# approach2 and approach3 track approach1, so one fixture drives all three.
person <- function(id, approach1) {
  data.table::data.table(
    id = id,
    isoyearweek = iso_weeks(length(approach1)),
    approach1 = approach1,
    approach2 = approach1,
    approach3 = approach1
  )
}

# Episodes are 160 weeks, comfortably past the 156-week minimum, so nothing
# below turns on the three-year rule.
run_one <- function(approach1) {
  d <- person(1L, approach1)
  data.table::setorder(d, id, isoyearweek)
  mht:::create_exposure_variables_v20250909(d)
  d[["rd_approach1_single"]]
}

# --------------------------------------------------------- the control ------

test_that("stopping systemic MHT records the later weeks as `previous`", {
  v <- run_one(c(rep(off_mht, 5), rep(on_mht, 160), rep(off_mht, 10)))

  expect_identical(sort(unique(v)), c("local_or_none_mht", "previous", "systemic_mht"))
  expect_identical(unique(utils::tail(v, 10)), "previous")
})

# ------------------------------------------- clashingprescriptions is active --

test_that("a clash that ends in no treatment records `previous`", {
  # This is the property the repair depends on. A clashing week counts as an
  # active level, so the move to `local_or_none_mht` is a stop like any other
  # and the woman is carried forward as a former user.
  v <- run_one(c(rep(off_mht, 5), rep(clash, 160), rep(off_mht, 10)))

  expect_identical(
    sort(unique(v)),
    c("clashingprescriptions", "local_or_none_mht", "previous")
  )
  expect_identical(unique(utils::tail(v, 10)), "previous")
})

# ------------------------------------------------------ exclude is inert -----

test_that("PINNED DEFECT: `exclude` never becomes `previous`, and swallows the weeks after it", {
  # `exclude` is not an active level, so `local_or_none_mht` -> `exclude` ->
  # `local_or_none_mht` contains no stop for the transition rule to find. The
  # woman leaves this sequence having never been recorded as treated.
  #
  # This is why the overlap repair CANNOT be implemented by writing `exclude`
  # into the clashing weeks: the exclusion would never lift.
  v <- run_one(c(rep(off_mht, 5), rep(censored, 160), rep(off_mht, 10)))

  expect_false("previous" %in% v)
  expect_identical(sort(unique(v)), c("exclude", "local_or_none_mht"))

  # the ten untreated weeks at the end are absorbed, not returned to the cohort
  expect_identical(unique(utils::tail(v, 10)), "exclude")
})

# ------------------------------------------- the false-initiation defect -----

test_that("PINNED DEFECT: a clash that resolves into a group looks like a first treatment", {
  # Weeks 6 to 15 clash, then week 16 resolves to `systemic_mht`. Nothing before
  # week 16 carries the value `systemic_mht`.
  #
  # The target-trial specs exclude prior use by testing
  # `rd_approach1_single == systemic_mht` over the lifetime before baseline
  # (003-iliadis-stroke/spec_v002.yaml). A clashing week is a different value,
  # so this woman passes that test as treatment-naive and enrols as an initiator
  # at week 16, having been treated since week 6.
  #
  # The repair carries the clash forward until she reaches `local_or_none_mht`,
  # so no week ever resolves out of a clash into a group.
  v <- run_one(c(rep(off_mht, 5), rep(clash, 10), rep(on_mht, 160), rep(off_mht, 10)))

  expect_identical(v[15], "clashingprescriptions")
  expect_identical(v[16], "systemic_mht")
  expect_false("systemic_mht" %in% v[seq_len(15)])

  # she does reach `previous` once she stops, so only the entry is wrong
  expect_identical(unique(utils::tail(v, 10)), "previous")
})

# ------------------------------------- where the clash comes from, and how far --
#
# The tests above drive `create_exposure_variables_v20250909()` on a hand-built
# skeleton. The two below drive the REAL entry point, so the clash is produced
# by the classifier rather than supplied to it.
#
# `Divigel` is transdermal oestrogen and `Femanest` is peroral oestrogen. Given
# the same dispensing date they start together, their run lengths stay equal,
# and the run-length rule cannot separate them.

clash_fixture <- function() {
  weeks <- cstime::date_to_isoyearweek_c(as.Date("2010-01-04") + (seq_len(300) - 1L) * 7L)
  skeleton <- data.table::data.table(id = 1L, isoyearweek = weeks)
  lmed <- data.table::data.table(
    lopnr = c(1L, 1L),
    produkt = c("Divigel", "Femanest"),
    edatum = as.Date(c("2010-06-07", "2010-06-07")),
    fddd = c(365, 365)
  )
  suppressMessages(add_lmed_v20250909(skeleton, lmed, verbose = FALSE))
  skeleton
}

test_that("approach 1 can never clash: it carries one run-length group", {
  # A clash needs two groups tied on run length. The resolver skips
  # `local_or_none_mht`, so approach 1 has exactly one group to time and the tie
  # test can never be met. Approaches 2 and 3 have two and three.
  #
  # This bounds the defect below: it cannot reach an analysis that enrols on
  # `rd_approach1_single`.
  skeleton <- clash_fixture()

  expect_false("clashingprescriptions" %in% skeleton[["approach1"]])
  expect_true("clashingprescriptions" %in% skeleton[["approach2"]])
})

test_that("PINNED DEFECT: one clashing week is carried to the end of follow-up", {
  # Both prescriptions cover 365 days, so the overlap lasts about 52 weeks. The
  # flag outlives it by a factor of five.
  skeleton <- clash_fixture()
  v <- skeleton[["approach2"]]

  expect_identical(which(v == "clashingprescriptions")[1], 23L)
  expect_identical(v[length(v)], "clashingprescriptions")

  # 278 of 300 weeks, against about 52 weeks of real overlap. The repair stops
  # the flag at her first untreated week, so this count must fall when it lands.
  expect_identical(sum(v == "clashingprescriptions"), 278L)
})
