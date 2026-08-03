# Changelog

## mht 26.8.3

- **New codebook version `dataDictionary20260803.xlsx` ships, and
  NOTHING reads it yet.** It is staged for the next dated entry point.
  Both existing entry points keep reading `dataDictionary20241105.xlsx`,
  unchanged.
- **A dated function is frozen, and so are its inputs.**
  [`add_lmed_v20230509()`](https://skalkidou-lab.github.io/mht/reference/add_lmed_v20230509.md)
  and
  [`add_lmed_v20250909()`](https://skalkidou-lab.github.io/mht/reference/add_lmed_v20250909.md)
  are dated, therefore immutable — repointing either at a new codebook
  would change a frozen artefact, even when the change is behaviourally
  inert. **To adopt `dataDictionary20260803.xlsx`, create a new dated
  entry point that reads it. Do not edit an existing one.**
- **The only change to the codebook itself is an annotation on the
  `Rules` sheet.** `MHT_groups`, `post_grouping`, `FDDD` and
  `covariates` are identical between the two files, verified cell by
  cell. No code reads `Rules`. So classification, duration and exposure
  output are unchanged — the 17 golden tests pass untouched, and no
  reclean is needed.
- The annotation marks `Rules` item 4 **superseded, item 4 only**. Item
  4 rounds down to whole treatment **periods**. A later study decision,
  taken in November 2024, replaced it with rounding down to whole
  **months** — the rule that added `minimum_monthly_dose` and
  `minimum_months` to `MHT_groups`, and the one the minimum-dose loop in
  `R/lmed-v20230509.R` has implemented ever since. Items 1, 2, 3 and 5
  are unaffected.
- The annotation states the difference in full: the two rules agree only
  when `fddd` is a multiple of `minimum_monthly_dose` times
  `minimum_months`, and 15 of the 18 products carrying a rule are
  affected. **The decision itself, and the correspondence recording it,
  are held in the study’s own project record, which is not public.**

## mht 26.7.31.1

- **Both exported entry points are renamed, and the suffix dates the
  METHODOLOGY.** `x2023_add_lmed()` becomes
  [`add_lmed_v20230509()`](https://skalkidou-lab.github.io/mht/reference/add_lmed_v20230509.md);
  `x2026_add_lmed()` becomes
  [`add_lmed_v20250909()`](https://skalkidou-lab.github.io/mht/reference/add_lmed_v20250909.md).
  The eleven internal helpers take the same suffix and drop the `x20xx_`
  prefix.
- The `v<YYYYMMDD>` suffix is **not** the study year and **not** the
  release date. It is the date on which that definition’s methodology
  was created:
  - `v20230509` — the approach layer (`post_grouping`, the approach
    definitions) was created **2023-05-09**, in the `2022-mht`
    repository, commit `ce396c2`.
  - `v20250909` — the `rd_approach{1,2,3}_*` methodology and its
    sensitivity variants were created **2025-09-09**, in the
    `structural-mht-registry-data` repository, commit `3603214`. That is
    five months before the code was written into `swereg`, which is why
    the old `x2026` prefix was misleading: 2026 was the study year, not
    the year the method was devised.
- Behaviour is unchanged. This release is a rename only, proven against
  the frozen goldens in `inst/testdata/` with
  [`identical()`](https://rdrr.io/r/base/identical.html).
- See the three-column migration map in `README.md` if you are coming
  from either earlier name.

## mht 26.7.31

- First release. `mht` carries the MHT-specific code that previously
  lived in `swereg`, extracted into a package of its own so the MHT
  exposure definitions can be versioned and reviewed separately from the
  general registry tooling.
- Function names drop the `mht` infix, because the package name now
  supplies it. `swereg::x2023_mht_add_lmed()` becomes
  `mht::x2023_add_lmed()`, and `swereg::x2026_mht_add_lmed()` becomes
  `mht::x2026_add_lmed()`. See the migration map in `README.md`.
- `mht` does not depend on `swereg`. It operates on person-week
  skeletons of the form `swereg` produces, but takes them as plain
  `data.table` arguments, so the two packages can be installed and
  upgraded independently.
