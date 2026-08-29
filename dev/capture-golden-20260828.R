# Capture the frozen 2026-08-28 golden.
#
# Run from the package root:
#
#   Rscript --vanilla dev/capture-golden-20260828.R
#
# The script writes three files into inst/testdata/ and prints the three
# MANIFEST.sha256 lines to append. It refuses to overwrite an existing file.
# Set MHT_GOLDEN_OVERWRITE=1 to replace one.
#
# Replaying the baseline detects drift in add_lmed_v20260828(). It detects
# nothing about any other function. inst/testdata/PROVENANCE.md carries the
# whole record, under "The 2026-08-28 baseline".

suppressMessages(pkgload::load_all(".", quiet = TRUE))
library(data.table)

out_dir <- file.path("inst", "testdata")
overwrite <- identical(Sys.getenv("MHT_GOLDEN_OVERWRITE"), "1")

write_once <- function(x, file) {
  path <- file.path(out_dir, file)
  if (file.exists(path) && !overwrite) {
    stop(
      "refusing to overwrite the frozen golden ",
      path,
      "; set MHT_GOLDEN_OVERWRITE=1 to replace it",
      call. = FALSE
    )
  }
  saveRDS(x, path)
  return(invisible(path))
}

# THE INPUT. Both fixtures ship with the package. The skeleton carries the same
# values as inst/testdata/input_skeleton_2026.rds. The two files differ as
# bytes, because two R sessions serialised them.
skeleton <- copy(fake_skeleton_mht)
lmed <- copy(fake_lmed_2026)

# THE FIXTURE EXTENSION. fake_lmed_2026 carries no lnmn column, and it carries
# Utrogestan. The 2026-08-28 codebook keys the Utrogestan rule on strength. The
# layer reads that strength out of lnmn, and reports an error when the column is
# absent. The extension adds lnmn, gives every other product NA, and gives
# Utrogestan the 100 mg register spelling. That is the same workaround the
# ?add_lmed_v20260828 example uses.
lmed[, lnmn := NA_character_]
lmed[produkt == "Utrogestan", lnmn := "Utrogestan, kapsel, mjuk 100 mg"]

# The fixture holds one negative-duration row, which warns.
expected <- copy(skeleton)
suppressWarnings(add_lmed_v20260828(expected, lmed, verbose = FALSE))

write_once(skeleton, "input_skeleton_20260828.rds")
write_once(lmed, "input_lmed_20260828.rds")
write_once(expected, "expected_20260828.rds")

# The manifest lines come from sha256sum itself, so the format cannot drift
# from what `sha256sum -c` reads back.
cat("\nAppend these lines to inst/testdata/MANIFEST.sha256:\n\n")
manifest_files <- c(
  "expected_20260828.rds",
  "input_lmed_20260828.rds",
  "input_skeleton_20260828.rds"
)
lines <- system2(
  "sha256sum",
  file.path(out_dir, manifest_files),
  stdout = TRUE
)
# The manifest names each file with no directory, because `sha256sum -c` runs
# from inside inst/testdata.
cat(sub(paste0(out_dir, "/"), "", lines, fixed = TRUE), sep = "\n")
cat("\n")

cat("\nColumns written:", ncol(expected), "\n")
cat("Rows:", nrow(expected), "\n")
