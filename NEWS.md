# mht 26.8.29

* **`inst/2023-mht/dataDictionary20260828.xlsx` ships, and the 2026-08-28
  functions read it.** Every older codebook stays in place, read by the entry
  point that already names it.
* **`add_lmed_v20260828()` is the 2026-08-28 entry point.** It returns
  `invisible(skeleton)` and keeps the caller's row order.
* **`add_lmed_v20260828()` owns the product category columns, `approach1` to
  `approach3`, and every column whose name starts with `rd_approach`.** It
  changes no other column of `skeleton`, so a caller column that shares a name
  with a working column survives.
* **`add_lmed_v20260828()` requires each person to hold each ISO week once,
  with no gap, and `isoyearweek` to read `"YYYY-WW"`.** Gap bridging and the
  three-year rule both count rows, so one row MUST be one observed week.
* **A second call to `add_lmed_v20260828()` gives the table the first call
  gives.** It recomputes every output column from the product categories up.
* **`id_name` names the person identifier column of `lmed`, and defaults to
  `"lopnr"`.**
* **`create_rd = FALSE` writes no `rd_approach*` column, and removes every one
  that `skeleton` already carries.**
* **A product name matches a codebook name as a prefix, after both are reduced
  to their lowercase ASCII letters.** `Estramon 100` reaches the `estramon`
  rung. Where one codebook name is a prefix of another the longer one sits
  first, and `dev/check-crosswalk.R` asserts that ordering.
* **One codebook rule applies to each prescription, and the longest matching
  product name wins.** Two rules of equal name length that both apply are an
  error.
* **The duration layer reads every rule from the codebook's own cells.** Of the
  116 product rows of the `MHT_groups` sheet, 24 carry a rule: 6 a fixed
  duration in days, and 18 a whole-months pair. A codebook edit changes the
  answer with no code change.
* **Two of those 18 rules are keyed on a strength band, and the strength comes
  from `lnmn`, the register product name.** `lnmn` is REQUIRED as soon as a
  strength-keyed rule names a product in `lmed`.
* **An exposure interval is closed and inclusive.** A prescription dispensed on
  `edatum` for `d` days covers `edatum` to `edatum + d - 1`.
* **A prescription with no category, no dispensing date or no positive duration
  contributes nothing.** The run reports one aggregate warning.
* **Tibolone is its own treatment group, in all three approaches.**
* **Two treatments that start in the same week mark that week
  `clashingprescriptions`, through to the last treated week of that episode.**
  The first untreated week after it is an ordinary stop, so the woman is
  recorded as a former user.
* **The four-week bridge needs a treated week on each side.** A leading or a
  trailing run of untreated weeks is not a gap.

# mht 26.8.26

* **The lint gate is wired up.** The shared workflow's `static-checks` job was
  red on 22 lints, and `R-CMD-check` and `pkgdown` were skipped behind it.
  Eleven `<path>:<linter>` pairs are now allowlisted in
  `.github/workflows/r-package.yml`. Every one is in a dated, frozen artefact:
  `lmed-v20230509.R`, `lmed-v20250909.R` and `exposure-v20250909.R`. Those files
  cannot be fixed, because `mht` changes behaviour by creating a new dated
  function rather than by editing an existing one.
* **`first_non_na()` gains an explicit `return()`.** It was the only lint in a
  file that is not frozen, so it is fixed rather than allowlisted.

* **`tests/testthat/test-clash-and-exclude.R` pins how `clashingprescriptions`
  and `exclude` reach `previous`.** The transition rule that records a woman as
  a former MHT user fires only when she moves from an active level to
  `local_or_none_mht`. Which levels count as active decides whether she can
  re-enter a new-user analysis, and the two levels behave differently.
* **`clashingprescriptions` is an active level.** A clash that ends in no
  treatment records the later weeks as `previous`, exactly as stopping systemic
  MHT does.
* **`exclude` is inert, and the file pins that as a defect.** The sequence
  `local_or_none_mht` to `exclude` to `local_or_none_mht` contains no stop for
  the transition rule to find, so no `previous` is ever recorded and the
  untreated weeks after the exclusion are absorbed into it. An overlap repair
  written by putting `exclude` into the clashing weeks would therefore never
  lift.
* **A clash that resolves into a group is pinned as a defect.** Where clashing
  weeks are followed by `systemic_mht`, no earlier week carries the value
  `systemic_mht`. The target-trial specs exclude prior use by testing that one
  value, so such a woman passes as treatment-naive and enrols as an initiator
  while already treated.

* **Two tests drive the real entry point, so the classifier produces the clash
  rather than receiving it.** `Divigel` and `Femanest` dispensed on the same day
  start together and their run lengths stay equal, so the run-length rule cannot
  separate them.
* **Approach 1 can never clash, and the file pins that.** A clash needs two
  groups tied on run length, the resolver skips `local_or_none_mht`, and
  approach 1 has one group left to time. Approaches 2 and 3 have two and three.
  This bounds the defect: it cannot reach an analysis that enrols on
  `rd_approach1_single`.
* **The flag outliving the overlap is pinned as a defect.** Two prescriptions
  covering 365 days each overlap for about 52 weeks, and the flag then covers
  278 of 300 weeks.

# mht 26.8.21

* **The pkgdown template package is renamed to `pptemplate`.** `DESCRIPTION`
  `Config/Needs/website` and `_pkgdown.yml` `template: package:` both name
  `papadopoulos-lab/pptemplate` now. The house style itself is unchanged.
* **A caller of the shared `papadopoulos-lab/pptemplate` workflow replaces
  `.github/workflows/check-and-pkgdown.yml`.** The shared workflow runs
  `loc-limit`, then `R-CMD-check`, then `pkgdown`. No file in `R/` is over 1000
  code lines, so the caller needs no `loc-allowlist`.

# mht 26.8.6

## Licensing

* `DESCRIPTION` `Authors@R` now declares **Richard Aubrey White** as the
  copyright holder, with `role = "cph"`. It declared none at all, and
  neither did any other package in the fleet. Nothing in `R CMD check`
  reports that.
* The copyright year is now 2026. It read 2026.
* `CLAUDE.md` now carries a Licensing section, so the year gets checked
  rather than silently ageing.

* Prose only. This release rewrites the roxygen documentation, `README.md`,
  `index.md` and `NEWS.md` to ASD-STE100 (Simplified Technical English). No
  package code changed.
* No claim changed. The sweep found documented claims that the code does not
  support. It left every one of those claims in place, and reported it.
* The rewrite splits long sentences, prefers the active voice, and uses one term
  for each concept. Sequences that sat inside one prose sentence became
  bulleted lists. The `@return` warning on both entry points now uses the
  RFC-2119 keyword MUST NOT.

# mht 26.8.3.2

* **Adds the hex logo at `man/figures/logo.png`, generated by `dev/logo.R`.**
  The package was the only one in the family without one, so the pkgdown hero
  band had an empty art column. The template picks the file up on its own. No
  `_pkgdown.yml` change was needed.
* **The logo is generated, never hand-drawn.** `dev/logo.R` builds it with
  `hexSticker`, and `dev/` is `.Rbuildignore`d, so neither the script nor its
  three build-only packages ship. To regenerate the logo, run
  `Rscript dev/logo.R` from the package root.
* No package code changed. No exported function, argument or return value is
  affected.

# mht 26.8.3.1

* **New test file `tests/testthat/test-classifier-invariants.R` pins the whole
  exposure pipeline stage by stage, defects included.** No code changed. Every
  expectation was obtained by running the exported entry points. None was read
  out of the codebook. None was derived from the ladder source. When a repair
  lands, the diff of that file is the record of what changed.
* **The pin is three frozen literal tables.** They are:
  * the category the classifier returns for each of the ladder's own pattern
    literals;
  * the category it returns for each product name in the codebook's
    `MHT_groups` sheet;
  * the category each rung DECLARES, in ladder order and with duplicates.

  All three were generated by running the code and pasted in as literals, so
  relabelling any rung turns the suite red. They are NOT a clinically approved
  product-to-category map. They record present behaviour only.
* **The declared table is what covers a SHADOWED rung.** A rung that can never
  fire has no observable behaviour to change. Relabelling it is therefore
  invisible to the other two tables. Yet a future reordering repair unshadows
  exactly those shadowed rungs, and a label that drifted in the meantime would
  land the wrong category silently. The frozen declared sequence also pins the
  ladder's length and its per-pattern multiplicity. Nobody can then add a
  second, shadowed rung for an existing pattern unnoticed.
* Pinned per stage:
  * which rows enter;
  * what a raw product name becomes;
  * which ladder rung a name reaches under first-match ordering;
  * which categories can be materialised as skeleton columns;
  * how the minimum-dose loop resolves;
  * how the duration rules take precedence over each other;
  * which approach columns a category set produces;
  * what reaches the caller's skeleton.
* **The pinned defects.** Three ladder rungs are shadowed by an earlier rung
  and can never fire, so `B10`, `C5` and `D1` are unreachable for every input.
  Seven category columns stay `FALSE` over the codebook product universe:
  `B10`, `B12`, `C2`, `C5`, `D1`, `D4` and `I1`. That count is scoped to the
  codebook deliberately. `I1` is reachable from the spellings `MiniPe` and
  `Mini Pe`. It misses only because the codebook spells the product `Mini-Pe`,
  and the hyphen survives normalisation. The ladder produces category `G1`, but
  `G1` is absent from the column list. A `G1` prescription is therefore
  classified and then reaches no column at all. No approach rule reads
  categories `F1`, `G1` and `H1`, so adding one of them to a person changes no
  approach value.
* **The minimum-dose loop compounds.** It writes the duration once per matching
  codebook row, and each iteration reads the previous result. A product name
  matching two rows therefore has the rule applied twice. The loop also matches
  the RAW product name, while the ladder matches the normalised one. The loop
  writes last, so it overrides both the IUD and the Jaydess duration rules
  rather than yielding to them.
* **The duration pins run from two anchor dates, because one anchor is
  directional.** The exposure duration is observable only to the ISO week. Every
  pinned duration is a multiple of seven days, so the interval ends on the
  anchor's own weekday. A Sunday anchor therefore detects one day LONGER, and is
  blind to one day shorter. A Monday anchor is the mirror image. Running both
  gives one-day sensitivity in each direction. Measured, then verified by
  mutation: 1680 to 1681 is red from the Sunday anchor, 1680 to 1679 from the
  Monday one. Each anchor's blind direction is asserted too, for both the IUD
  and the Jaydess duration. A blind assertion is not a tautology. It holds only
  while the duration stays inside that ISO week, so it still detects a change at
  week resolution. Also verified by mutation, by moving each constant a full
  week. Pins that name no anchor use the Sunday default and detect lengthening
  only, which the test file states.
* **The documented contract is pinned as it really behaves.** An entry point
  returns a length-1 logical, not the skeleton. It does three things to the
  caller's objects:
  * it reorders the caller's skeleton;
  * it deletes caller columns whose names collide with its own working columns;
  * it attaches a `data.table` index to the caller's `lmed`.

  "Mutates by reference and only adds columns" is not an accurate description,
  and the contract needs a decision.

# mht 26.8.3

* **New codebook version `dataDictionary20260803.xlsx` ships, and NOTHING reads
  it yet.** It is staged for the next dated entry point. Both existing entry
  points keep reading `dataDictionary20241105.xlsx`, unchanged.
* **A dated function is frozen, and so are its inputs.** `add_lmed_v20230509()`
  and `add_lmed_v20250909()` are dated, and therefore immutable. Repointing
  either at a new codebook would change a frozen artefact, even when the change
  is behaviourally inert. **To adopt `dataDictionary20260803.xlsx`, you MUST
  create a new dated entry point that reads it. You MUST NOT edit an existing
  one.**
* **The only change to the codebook itself is an annotation on the `Rules`
  sheet.** `MHT_groups`, `post_grouping`, `FDDD` and `covariates` are identical
  between the two files, verified cell by cell. No code reads `Rules`. So
  classification, duration and exposure output are unchanged — the 17 golden
  tests pass untouched, and no reclean is needed.
* The annotation marks `Rules` item 4 **superseded, item 4 only**. Item 4 rounds
  down to whole treatment **periods**. A later study decision, taken in November
  2024, replaced it with rounding down to whole **months**. That decision added
  `minimum_monthly_dose` and `minimum_months` to `MHT_groups`. The minimum-dose
  loop in `R/lmed-v20230509.R` implements the months rule. It did so from
  November 2024 onward. Items 1, 2, 3 and 5 are unaffected.
* The annotation states the difference in full. The two rules agree only when
  `fddd` is a multiple of `minimum_monthly_dose` times `minimum_months`. Of the
  18 products carrying a rule, 15 are affected. **The decision itself, and the
  correspondence recording it, are held in the study's own project record. That
  record is not public.**

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
      months before the code was written into `swereg`. The old `x2026` prefix
      was therefore misleading: 2026 was the study year, not the year the
      method was devised.
* Behaviour is unchanged. This release is a rename only, proven against the
  frozen goldens in `inst/testdata/` with `identical()`.
* See the three-column migration map in `README.md` if you are coming from
  either earlier name.

# mht 26.7.31

* First release. `mht` carries the MHT-specific code that previously lived in
  `swereg`. That code moved into a package of its own, so the MHT exposure
  definitions can be versioned and reviewed separately from the general
  registry tooling.
* Function names drop the `mht` infix, because the package name now supplies it.
  `swereg::x2023_mht_add_lmed()` becomes `mht::x2023_add_lmed()`, and
  `swereg::x2026_mht_add_lmed()` becomes `mht::x2026_add_lmed()`. See the
  migration map in `README.md`.
* `mht` does not depend on `swereg`. It operates on person-week skeletons of the
  form `swereg` produces, but takes them as plain `data.table` arguments. The
  two packages can therefore be installed and upgraded independently.
