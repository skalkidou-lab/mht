# Pins `x2026_cumulative_reset()`: a cumulative count of TRUE weeks that
# restarts at every FALSE. Vectors are built INLINE so the reset and the
# never-reset cases are both present and unambiguous.

test_that("the count restarts after a FALSE", {
  expect_identical(
    mht:::x2026_cumulative_reset(c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE)),
    c(1L, 2L, 0L, 1L, 2L, 3L)
  )
})

test_that("a run that never resets counts straight up to its length", {
  expect_identical(
    mht:::x2026_cumulative_reset(rep(TRUE, 5)),
    1:5
  )
})

test_that("every FALSE week is 0, and a FALSE run does not accumulate", {
  expect_identical(
    mht:::x2026_cumulative_reset(c(FALSE, FALSE, TRUE, FALSE)),
    c(0L, 0L, 1L, 0L)
  )
  expect_identical(
    mht:::x2026_cumulative_reset(rep(FALSE, 3)),
    c(0L, 0L, 0L)
  )
})

test_that("two separate TRUE runs each start again at 1", {
  expect_identical(
    mht:::x2026_cumulative_reset(c(
      TRUE, TRUE, TRUE,
      FALSE, FALSE,
      TRUE, TRUE
    )),
    c(1L, 2L, 3L, 0L, 0L, 1L, 2L)
  )
})

test_that("the result is integer, not logical or double", {
  expect_type(mht:::x2026_cumulative_reset(c(TRUE, FALSE, TRUE)), "integer")
})
