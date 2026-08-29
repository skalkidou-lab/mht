# The frozen 2026-08-28 baseline.
#
# `inst/testdata/*_20260828.rds` holds one input pair and one output, captured
# by `dev/capture-golden-20260828.R`. This file replays `add_lmed_v20260828()`
# on that input and compares the result with `identical()`.
#
# WHAT THIS BASELINE DETECTS
#
# `dev/capture-golden-20260828.R` captured the baseline from
# `add_lmed_v20260828()`, and this file replays it against the same function.
# Replaying it detects drift in `add_lmed_v20260828()`. A change in any layer
# of the 2026-08-28 pipeline fails here. It cannot detect agreement or
# disagreement with any other function.
#
# The goldens are read from the INSTALLED package with `system.file()`, so this
# file runs unchanged on CI.

golden_path <- function(...) {
  p <- system.file("testdata", ..., package = "mht")
  if (!nzchar(p)) {
    stop(
      "golden file not found in the installed package: ",
      paste(..., sep = "/")
    )
  }
  p
}

# data.table keeps two in-memory caches that are not data: the
# `.internal.selfref` external pointer, which is ALWAYS stale after readRDS,
# and the secondary-index cache. Drop exactly those two. Column set, column
# order, key, types and values are all still compared by identical().
strip_dt_caches <- function(x) {
  x <- data.table::copy(x)
  data.table::setattr(x, ".internal.selfref", NULL)
  data.table::setattr(x, "index", NULL)
  x
}

test_that("the 2026-08-28 goldens ship with the installed package", {
  for (f in c(
    "input_skeleton_20260828.rds",
    "input_lmed_20260828.rds",
    "expected_20260828.rds"
  )) {
    expect_true(file.exists(golden_path(f)), info = f)
  }
})

test_that("the captured input carries the extended fixture", {
  # The capture extended `fake_lmed_2026` with an `lnmn` column, because the
  # Utrogestan rule of the 2026-08-28 codebook is keyed on strength. Pin the
  # extension, so a later capture cannot quietly drop it.
  lmed <- readRDS(golden_path("input_lmed_20260828.rds"))
  expect_true("lnmn" %in% names(lmed))
  expect_identical(
    unique(lmed$lnmn[lmed$produkt == "Utrogestan"]),
    "Utrogestan, kapsel, mjuk 100 mg"
  )
  expect_true(all(is.na(lmed$lnmn[lmed$produkt != "Utrogestan"])))
})

test_that("add_lmed_v20260828() reproduces the frozen 2026-08-28 golden exactly", {
  skeleton <- data.table::copy(readRDS(golden_path(
    "input_skeleton_20260828.rds"
  )))
  lmed <- data.table::copy(readRDS(golden_path("input_lmed_20260828.rds")))
  expected <- readRDS(golden_path("expected_20260828.rds"))

  # the fixture holds one negative-duration row, which warns
  suppressWarnings(add_lmed_v20260828(skeleton, lmed, verbose = FALSE))

  expect_identical(strip_dt_caches(skeleton), strip_dt_caches(expected))
})

test_that("MANIFEST.sha256 names every golden that ships", {
  # `test-golden.R` checks the forward direction: every name in the manifest
  # has a file. This is the reverse direction. A manifest that only verifies
  # what it lists cannot report a golden that nobody listed.
  #
  # The count is asserted too. Set equality alone goes green when a file and
  # its manifest line disappear together, and the count does not.
  manifest <- readLines(golden_path("MANIFEST.sha256"), warn = FALSE)
  manifest <- manifest[nzchar(trimws(manifest))]
  listed <- vapply(
    strsplit(trimws(manifest), "[[:space:]]+"),
    function(parts) parts[[length(parts)]],
    character(1)
  )
  shipped <- list.files(golden_path(), pattern = "\\.rds$")

  expect_setequal(listed, shipped)
  expect_length(manifest, 9L)
  expect_length(shipped, 9L)
})
