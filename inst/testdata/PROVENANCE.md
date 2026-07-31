# Provenance of the frozen MHT goldens

Captured 2026-07-31 by Phase 1 of the `mht` extraction plan.
These files are FROZEN. Never regenerate them automatically.

## Source package

| Item | Value |
|---|---|
| Repository | `/home/ricwh321/code/swereg` (papadopoulos-lab/swereg) |
| Commit | `e6f4d34c0264f16b432cf730c0a8b4252f9cb396` |
| DESCRIPTION Version | `26.8.5` |
| Tarball | `swereg_26.8.5.tar.gz` |
| Tarball sha256 | `a3a16f98c6d551c0d9c7fd554ec080242391a4975ef44ea0df291f5786e8ef37` |
| Isolated library | `/tmp/mht-lib-old` (`R CMD INSTALL -l /tmp/mht-lib-old`) |
| `find.package("swereg")` at capture | `/tmp/mht-lib-old/swereg` |

The tarball was built from a `git archive e6f4d34` export, so the swereg working
tree was never written to.

## Function-body identity (installed vs source)

`digest::digest(list(body = body(fn), formals = formals(fn)), algo = "xxhash64")`,
source parsed with `keep.source = FALSE`. Equal version strings do not prove
equal trees; these hashes do.

| Function | Installed (`/tmp/mht-lib-old`) | Source (`e6f4d34`) | Match |
|---|---|---|---|
| `x2023_mht_add_lmed` | `f326b10324d72405` | `f326b10324d72405` | yes |
| `x2026_mht_add_lmed` | `b1d5e1b012d72aba` | `b1d5e1b012d72aba` | yes |

## Environment

| Item | Value |
|---|---|
| R | R version 4.5.2 (2025-10-31) |
| data.table | 1.18.4 |
| stringr | 1.6.0 |
| cstime | 2025.10.13 |
| readxl | 1.5.0 |
| glue | 1.8.1 |
| Platform | x86_64-pc-linux-gnu, Ubuntu (uppsala) |

Dependencies resolved from the user library
`/home/ricwh321/R/x86_64-pc-linux-gnu-library/4.5`, which was **not** written to.
`/tmp/mht-lib-old` was placed FIRST on `.libPaths()`, so `swereg` itself always
resolved from the isolated library; `capture.R` asserts this.

## Fixture

| Item | Value |
|---|---|
| Generator | `mht/dev/generate_fake_lmed.R` |
| Seed | `set.seed(42)` |
| lmed rows | 128 |
| skeleton rows | 5616 (18 persons x 312 ISO weeks) |
| ISO week span | `2015-02` .. `2020-52` (312 weeks, 6 ISO years) |
| Product categories present in lmed | 28 |

The grid spans 312 weeks so the 3-year minimum-duration rule
(`x2026-mht-specifics.R:486`, `last_session_on_mht < 3 * 52`) is reachable.
`previous` appears in all 8 `rd_approach*` columns of the 2026 golden.

## Files

| File | Role |
|---|---|
| `input_skeleton_2026.rds` | deep copy of the skeleton handed to `x2026_mht_add_lmed` |
| `input_lmed_2026.rds` | lmed fixture, `lopnr` id column |
| `expected_2026.rds` | skeleton after `x2026_mht_add_lmed` (46 cols) |
| `input_skeleton_2023.rds` | deep copy of the skeleton handed to `x2023_mht_add_lmed` |
| `input_lmed_2023.rds` | lmed fixture, `p1163_lopnr_personnr` id column |
| `expected_2023.rds` | skeleton after `x2023_mht_add_lmed` (38 cols) |
| `fake_lmed_2023.rda`, `fake_lmed_2026.rda`, `fake_skeleton_mht.rda` | frozen copies of the package fixtures |
| `MANIFEST.sha256` | sha256 of all nine files above |
| `capture.R` | the capture script that produced them |
| `verify.R` | the discriminator |
| `TRANSCRIPT.md` | red/green evidence per assertion |

Stored as `.rds`, not `.qs2`, so `mht` need not declare a `qs2` dependency.

---

NOTE: the copy of this file in `mht/inst/testdata/` sits beside a
`MANIFEST.sha256` scoped to the six `.rds` goldens in that directory only. The
three `.rda` fixtures live in `mht/data/` and are hashed by the master manifest
at `/tmp/mht-goldens/MANIFEST.sha256`.
