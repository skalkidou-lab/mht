# Pins the gap-bridging rule in `replace_false_runs_v20250909()`: every FALSE run
# of length <= 4 becomes TRUE, and every FALSE run of length >= 5 is left
# alone.
#
# The vectors here are built INLINE on purpose. The shipped fixture
# (`fake_lmed_2026` + `fake_skeleton_mht`) contains no FALSE run of exactly 4,
# so it cannot tell `<= 4` from `<= 3` and cannot pin this boundary at all.

test_that("a FALSE run of 3 is bridged", {
  expect_identical(
    mht:::replace_false_runs_v20250909(c(TRUE, FALSE, FALSE, FALSE, TRUE)),
    rep(TRUE, 5)
  )
})

test_that("a FALSE run of exactly 4 is bridged (the inclusive side of <= 4)", {
  expect_identical(
    mht:::replace_false_runs_v20250909(c(TRUE, rep(FALSE, 4), TRUE)),
    rep(TRUE, 6)
  )
})

test_that("a FALSE run of 5 is NOT bridged (the exclusive side of <= 4)", {
  expect_identical(
    mht:::replace_false_runs_v20250909(c(TRUE, rep(FALSE, 5), TRUE)),
    c(TRUE, rep(FALSE, 5), TRUE)
  )
})

test_that("runs of 3, 4 and 5 in one vector are bridged, bridged, kept", {
  x <- c(
    TRUE,
    FALSE, FALSE, FALSE,
    TRUE,
    FALSE, FALSE, FALSE, FALSE,
    TRUE,
    FALSE, FALSE, FALSE, FALSE, FALSE,
    TRUE
  )
  expect_identical(
    mht:::replace_false_runs_v20250909(x),
    c(rep(TRUE, 10), rep(FALSE, 5), TRUE)
  )
})

test_that("the rule ignores position: leading and trailing runs count too", {
  # a leading run of 4 is bridged
  expect_identical(
    mht:::replace_false_runs_v20250909(c(rep(FALSE, 4), TRUE)),
    rep(TRUE, 5)
  )
  # a trailing run of 5 is not
  expect_identical(
    mht:::replace_false_runs_v20250909(c(TRUE, rep(FALSE, 5))),
    c(TRUE, rep(FALSE, 5))
  )
})

test_that("an all-FALSE vector is bridged only when it is 4 long or shorter", {
  expect_identical(mht:::replace_false_runs_v20250909(rep(FALSE, 4)), rep(TRUE, 4))
  expect_identical(mht:::replace_false_runs_v20250909(rep(FALSE, 5)), rep(FALSE, 5))
})

test_that("an all-TRUE vector and an empty vector are returned unchanged", {
  expect_identical(mht:::replace_false_runs_v20250909(rep(TRUE, 7)), rep(TRUE, 7))
  expect_identical(mht:::replace_false_runs_v20250909(logical(0)), logical(0))
})
