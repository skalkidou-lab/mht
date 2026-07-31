# mht 26.7.31.1

* **Both exported entry points are renamed, and the suffix dates the
  METHODOLOGY.** `x2023_add_lmed()` becomes `add_lmed_v20230509()`;
  `x2026_add_lmed()` becomes `add_lmed_v20250909()`. The eleven internal helpers
  take the same suffix and drop the `x20xx_` prefix.
* The `v<YYYYMMDD>` suffix is **not** the study year and **not** the release
  date. It is the date on which that definition's methodology was created:
    * `v20230509` — the approach layer (`post_grouping`, the approach
      definitions) was created **2023-05-09**, in the `2022-mht` repository,
      commit `ce396c2`.
    * `v20250909` — the `rd_approach{1,2,3}_*` methodology and its sensitivity
      variants were created **2025-09-09**, in the
      `structural-mht-registry-data` repository, commit `3603214`. That is five
      months before the code was written into `swereg`, which is why the old
      `x2026` prefix was misleading: 2026 was the study year, not the year the
      method was devised.
* Behaviour is unchanged. This release is a rename only, proven against the
  frozen goldens in `inst/testdata/` with `identical()`.
* See the three-column migration map in `README.md` if you are coming from
  either earlier name.

# mht 26.7.31

* First release. `mht` carries the MHT-specific code that previously lived in
  `swereg`, extracted into a package of its own so the MHT exposure definitions
  can be versioned and reviewed separately from the general registry tooling.
* Function names drop the `mht` infix, because the package name now supplies it.
  `swereg::x2023_mht_add_lmed()` becomes `mht::x2023_add_lmed()`, and
  `swereg::x2026_mht_add_lmed()` becomes `mht::x2026_add_lmed()`. See the
  migration map in `README.md`.
* `mht` does not depend on `swereg`. It operates on person-week skeletons of the
  form `swereg` produces, but takes them as plain `data.table` arguments, so the
  two packages can be installed and upgraded independently.
