# Pins the skeleton contract of `add_lmed_v20260902()`: weekly rows only.
#
# `swereg::create_skeleton()` writes one annual row per person per year before
# the weekly rows. An annual row carries `is_isoyear == TRUE` and an
# `isoyearweek` of the form `"YYYY-**"`, which is no ISO week. The entry point
# stops on it, and it runs on the weekly subset. A later dated entry point that
# accepts an annual row has to change this file to do so.

lmed_fixture <- function() {
  lmed <- data.table::setDT(data.table::copy(mht::fake_lmed_2026))
  lmed[, lnmn := NA_character_]
  lmed[produkt == "Utrogestan", lnmn := "Utrogestan, kapsel, mjuk 100 mg"]
  # `Paracetamol` is a fixture-only name, so the product table holds no row for
  # it. Dropping it changes no exposure, because it is not MHT either way.
  return(lmed[produkt != "Paracetamol"])
}

# The synthetic skeleton, in the shape `swereg::create_skeleton()` delivers:
# the annual rows of 1990 to 2014 for each person, then the weekly rows.
skeleton_fixture <- function() {
  weekly <- data.table::copy(mht::fake_skeleton_mht)
  weekly[, is_isoyear := FALSE]
  annual <- data.table::CJ(
    id = sort(unique(weekly$id)),
    isoyearweek = paste0(1990:2014, "-**"),
    sorted = TRUE
  )
  annual[, is_isoyear := TRUE]
  return(rbind(annual, weekly))
}

test_that("an annual row stops the run and names the value", {
  skeleton <- skeleton_fixture()
  expect_error(
    suppressWarnings(
      add_lmed_v20260902(skeleton, lmed_fixture(), verbose = FALSE)
    ),
    "no ISO week"
  )
})

test_that("the weekly subset runs, and the caller keeps the annual rows", {
  skeleton <- skeleton_fixture()
  weekly <- skeleton[is_isoyear == FALSE]
  suppressWarnings(
    add_lmed_v20260902(weekly, lmed_fixture(), verbose = FALSE)
  )
  expect_true("approach1" %in% names(weekly))
  expect_identical(nrow(weekly), nrow(mht::fake_skeleton_mht))
  # The subset is a new table, so the skeleton the caller holds is unchanged.
  expect_false("approach1" %in% names(skeleton))
  expect_identical(
    sum(skeleton$is_isoyear),
    25L * data.table::uniqueN(skeleton$id)
  )
})
