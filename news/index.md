# Changelog

## mht 26.7.31

- First release. `mht` carries the MHT-specific code that previously
  lived in `swereg`, extracted into a package of its own so the MHT
  exposure definitions can be versioned and reviewed separately from the
  general registry tooling.
- Function names drop the `mht` infix, because the package name now
  supplies it. `swereg::x2023_mht_add_lmed()` becomes
  [`mht::x2023_add_lmed()`](https://skalkidou-lab.github.io/mht/reference/x2023_add_lmed.md),
  and `swereg::x2026_mht_add_lmed()` becomes
  [`mht::x2026_add_lmed()`](https://skalkidou-lab.github.io/mht/reference/x2026_add_lmed.md).
  See the migration map in `README.md`.
- `mht` does not depend on `swereg`. It operates on person-week
  skeletons of the form `swereg` produces, but takes them as plain
  `data.table` arguments, so the two packages can be installed and
  upgraded independently.
