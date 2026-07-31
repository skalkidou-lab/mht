# Pins `create_exposure_variables_v20250909()`, in particular the three-year
# minimum-duration rule `last_session_on_mht < 3 * 52` (156 weeks).
#
# The skeleton here is built INLINE on purpose. The shipped fixture
# (`fake_lmed_2026` + `fake_skeleton_mht`) contains no treatment episode of
# 153-156 weeks, so it cannot tell `3 * 52` from any nearby threshold. The
# persons below straddle the boundary on both sides and sit exactly on it.

on_mht <- "systemic_mht"
off_mht <- "local_or_none_mht"

# ISO-like week labels that sort as strings and stay below the "9999-99"
# sentinel the function uses for "this person never stopped".
iso_weeks <- function(n) {
  i <- seq_len(n) - 1L
  sprintf("%04d-%02d", 2000L + i %/% 52L, i %% 52L + 1L)
}

# One person's weeks. approach2 and approach3 track approach1, so the same
# fixture exercises all three approaches and the approach3b collapse.
person <- function(id, approach1) {
  data.table::data.table(
    id = id,
    isoyearweek = iso_weeks(length(approach1)),
    approach1 = approach1,
    approach2 = data.table::fifelse(
      approach1 == on_mht,
      "peroral_estrogen",
      approach1
    ),
    approach3 = data.table::fifelse(
      approach1 == on_mht,
      "estrogen_progesterone_bioidentical",
      approach1
    )
  )
}

# id 1: 155 weeks on MHT, then off      - one week SHORT of the threshold
# id 2: 156 weeks on MHT, then off      - exactly ON the threshold
# id 3: 157 weeks on MHT, then off      - one week PAST the threshold
# id 4: 160 weeks on, 10 off, 10 on     - re-initiation, for single vs multiple
# id 5: never on MHT
build_skeleton <- function() {
  d <- data.table::rbindlist(list(
    person(1L, c(rep(on_mht, 155), rep(off_mht, 20))),
    person(2L, c(rep(on_mht, 156), rep(off_mht, 20))),
    person(3L, c(rep(on_mht, 157), rep(off_mht, 20))),
    person(4L, c(rep(on_mht, 160), rep(off_mht, 10), rep(on_mht, 10))),
    person(5L, rep(off_mht, 30))
  ))
  data.table::setorder(d, id, isoyearweek)
  d[]
}

# The last `n` weeks of one person's row block.
#
# Deliberately NOT written as `d[d$id == person_id]`: data.table evaluates `i`
# inside the table, so a bare comparison would resolve both sides to the `id`
# COLUMN and silently select every row. Subset the plain vectors instead.
person_values <- function(d, person_id, column) {
  d[[column]][d[["id"]] == person_id]
}

tail_values <- function(d, person_id, column, n) {
  v <- person_values(d, person_id, column)
  v[seq.int(length(v) - n + 1L, length(v))]
}

test_that("the eight rd_approach* columns are created", {
  d <- build_skeleton()
  mht:::create_exposure_variables_v20250909(d)
  expect_true(all(
    c(
      "rd_approach1_single",
      "rd_approach1_multiple",
      "rd_approach2_single",
      "rd_approach2_multiple",
      "rd_approach3_single",
      "rd_approach3_multiple",
      "rd_approach3b_single",
      "rd_approach3b_multiple"
    ) %in%
      names(d)
  ))
})

test_that("an episode of 155 weeks is too short: the following weeks are 'exclude'", {
  # below the boundary. 155 < 3 * 52, so "previous" is downgraded to "exclude".
  d <- build_skeleton()
  mht:::create_exposure_variables_v20250909(d)
  for (v in c(
    "rd_approach1_single",
    "rd_approach1_multiple",
    "rd_approach2_single",
    "rd_approach2_multiple",
    "rd_approach3_single",
    "rd_approach3_multiple",
    "rd_approach3b_single",
    "rd_approach3b_multiple"
  )) {
    expect_identical(tail_values(d, 1L, v, 20L), rep("exclude", 20L), info = v)
  }
})

test_that("an episode of exactly 156 weeks qualifies: the following weeks are 'previous'", {
  # exactly on the boundary. 156 < 3 * 52 is FALSE, so "previous" survives.
  d <- build_skeleton()
  mht:::create_exposure_variables_v20250909(d)
  for (v in c(
    "rd_approach1_single",
    "rd_approach1_multiple",
    "rd_approach2_single",
    "rd_approach2_multiple",
    "rd_approach3_single",
    "rd_approach3_multiple",
    "rd_approach3b_single",
    "rd_approach3b_multiple"
  )) {
    expect_identical(tail_values(d, 2L, v, 20L), rep("previous", 20L), info = v)
  }
})

test_that("an episode of 157 weeks qualifies: the following weeks are 'previous'", {
  # above the boundary.
  d <- build_skeleton()
  mht:::create_exposure_variables_v20250909(d)
  for (v in c(
    "rd_approach1_single",
    "rd_approach1_multiple",
    "rd_approach2_single",
    "rd_approach2_multiple",
    "rd_approach3_single",
    "rd_approach3_multiple",
    "rd_approach3b_single",
    "rd_approach3b_multiple"
  )) {
    expect_identical(tail_values(d, 3L, v, 20L), rep("previous", 20L), info = v)
  }
})

test_that("the 155 / 156 / 157 persons differ only at the boundary", {
  # the same three assertions read as one table, so a threshold moved by one
  # week cannot pass by coincidence
  d <- build_skeleton()
  mht:::create_exposure_variables_v20250909(d)
  got <- vapply(
    1:3,
    function(i) unique(tail_values(d, i, "rd_approach1_single", 20L)),
    character(1)
  )
  expect_identical(got, c("exclude", "previous", "previous"))
})

test_that("weeks on MHT keep their treatment level in every approach", {
  d <- build_skeleton()
  mht:::create_exposure_variables_v20250909(d)
  expect_identical(
    unique(person_values(d, 3L, "rd_approach1_single")[1:157]),
    "systemic_mht"
  )
  expect_identical(
    unique(person_values(d, 3L, "rd_approach2_single")[1:157]),
    "peroral_estrogen"
  )
  expect_identical(
    unique(person_values(d, 3L, "rd_approach3_single")[1:157]),
    "estrogen_progesterone_bioidentical"
  )
})

test_that("re-initiation is excluded under 'single' and allowed under 'multiple'", {
  d <- build_skeleton()
  mht:::create_exposure_variables_v20250909(d)
  # id 4: 160 on, 10 off, 10 on. 160 >= 156, so the off weeks are "previous".
  expect_identical(tail_values(d, 4L, "rd_approach1_single", 20L),
                   c(rep("previous", 10L), rep("exclude", 10L)))
  expect_identical(tail_values(d, 4L, "rd_approach1_multiple", 20L),
                   c(rep("previous", 10L), rep("systemic_mht", 10L)))
})

test_that("a person who was never on MHT stays 'local_or_none_mht'", {
  d <- build_skeleton()
  mht:::create_exposure_variables_v20250909(d)
  expect_identical(unique(person_values(d, 5L, "rd_approach1_single")), off_mht)
  expect_identical(unique(person_values(d, 5L, "rd_approach1_multiple")), off_mht)
  expect_identical(unique(person_values(d, 5L, "rd_approach3b_single")), off_mht)
})

test_that("approach3b collapses the two progesterone subtypes into one level", {
  d <- build_skeleton()
  mht:::create_exposure_variables_v20250909(d)
  expect_identical(
    unique(person_values(d, 3L, "rd_approach3b_single")[1:157]),
    "estrogen_progesterone"
  )
  expect_false("estrogen_progesterone_bioidentical" %in% d$rd_approach3b_single)
  expect_false("estrogen_progesterone_synthetic" %in% d$rd_approach3b_single)
})

test_that("the synthetic progesterone level also collapses to estrogen_progesterone", {
  d <- data.table::data.table(
    id = 1L,
    isoyearweek = iso_weeks(10L),
    approach1 = rep(on_mht, 10L),
    approach2 = rep("peroral_estrogen", 10L),
    approach3 = rep("estrogen_progesterone_synthetic", 10L)
  )
  mht:::create_exposure_variables_v20250909(d)
  expect_identical(unique(d$rd_approach3b_multiple), "estrogen_progesterone")
})

test_that("the skeleton is modified by reference: the caller's object gains the columns", {
  d <- build_skeleton()
  expect_false("rd_approach1_single" %in% names(d))
  invisible(mht:::create_exposure_variables_v20250909(d))
  expect_true("rd_approach1_single" %in% names(d))
  # the helper columns the function builds are all removed again
  expect_false(any(
    c("var_to_clean", "on_mht", "n", "length_on_mht", "last_session_on_mht") %in%
      names(d)
  ))
})
