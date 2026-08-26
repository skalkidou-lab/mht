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
