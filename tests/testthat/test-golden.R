# The frozen equivalence test.
#
# `inst/testdata/` holds inputs and outputs captured from the pre-extraction
# implementation (see `inst/testdata/PROVENANCE.md`). Both exported entry
# points are replayed here and compared with `identical()`. Any change in
# behaviour, anywhere in either pipeline, fails this test.
#
# The goldens are read from the INSTALLED package with `system.file()`, never
# from a path under `/tmp`, so this test runs unchanged on CI.

testdata <- function(...) {
  p <- system.file("testdata", ..., package = "mht")
  if (!nzchar(p)) {
    stop("golden file not found in the installed package: ", paste(..., sep = "/"))
  }
  p
}

# data.table keeps two in-memory caches that are not data: the
# `.internal.selfref` external pointer, which is ALWAYS stale after readRDS,
# and the secondary-index cache. Drop exactly those two. Column set, column
# order, key, types and values are all still compared by identical().
strip_caches <- function(x) {
  x <- data.table::copy(x)
  data.table::setattr(x, ".internal.selfref", NULL)
  data.table::setattr(x, "index", NULL)
  x
}

test_that("the frozen goldens ship with the installed package", {
  for (f in c(
    "input_skeleton_2023.rds",
    "input_lmed_2023.rds",
    "expected_2023.rds",
    "input_skeleton_2026.rds",
    "input_lmed_2026.rds",
    "expected_2026.rds",
    "MANIFEST.sha256",
    "PROVENANCE.md"
  )) {
    expect_true(file.exists(testdata(f)), info = f)
  }
})

test_that("add_lmed_v20250909() reproduces the frozen 2026 golden exactly", {
  skeleton <- data.table::copy(readRDS(testdata("input_skeleton_2026.rds")))
  lmed <- data.table::copy(readRDS(testdata("input_lmed_2026.rds")))
  expected <- readRDS(testdata("expected_2026.rds"))

  # the fixture deliberately holds one negative-duration row, which warns
  suppressWarnings(add_lmed_v20250909(skeleton, lmed, verbose = FALSE))

  expect_identical(strip_caches(skeleton), strip_caches(expected))
})

test_that("add_lmed_v20230509() reproduces the frozen 2023 golden exactly", {
  skeleton <- data.table::copy(readRDS(testdata("input_skeleton_2023.rds")))
  lmed <- data.table::copy(readRDS(testdata("input_lmed_2023.rds")))
  expected <- readRDS(testdata("expected_2023.rds"))

  suppressMessages(suppressWarnings(add_lmed_v20230509(skeleton, lmed)))

  expect_identical(strip_caches(skeleton), strip_caches(expected))
})

test_that("every golden named in MANIFEST.sha256 is present", {
  # the checksums themselves are asserted by the phase discriminator, which
  # runs sha256sum outside R. Here we only pin that the manifest and the files
  # it names travel together inside the installed package.
  manifest <- readLines(testdata("MANIFEST.sha256"), warn = FALSE)
  manifest <- manifest[nzchar(trimws(manifest))]
  expect_gt(length(manifest), 0L)
  for (line in manifest) {
    parts <- strsplit(trimws(line), "[[:space:]]+")[[1]]
    expect_true(file.exists(testdata(parts[[length(parts)]])), info = line)
  }
})
