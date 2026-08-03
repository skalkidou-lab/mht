# CLAUDE.md — mht

Guidance for Claude Code when working in this repository.

## A DATED ARTEFACT IS FROZEN. NEVER EDIT ONE.

This package versions by **date suffix**, and the suffix means
**frozen**, not “current”.

| Artefact        | Example                                                                                                                                                                                      | Rule                   |
|-----------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|------------------------|
| Entry point     | [`add_lmed_v20230509()`](https://skalkidou-lab.github.io/mht/reference/add_lmed_v20230509.md), [`add_lmed_v20250909()`](https://skalkidou-lab.github.io/mht/reference/add_lmed_v20250909.md) | Immutable once created |
| Internal helper | the eleven `*_v20230509()` / `*_v20250909()` helpers                                                                                                                                         | Immutable once created |
| Codebook        | `inst/2023-mht/dataDictionary20241105.xlsx`                                                                                                                                                  | Immutable once created |

**To change behaviour, create a NEW dated artefact. Do not edit an
existing one.** That is the entire purpose of the dating convention: two
analyses read this package, and a frozen version is what lets an
already-run analysis stay reproducible while the next one moves on.

**A frozen function’s INPUTS are frozen too.** Repointing
[`add_lmed_v20230509()`](https://skalkidou-lab.github.io/mht/reference/add_lmed_v20230509.md)
at a newer codebook edits a frozen artefact, **even when the two
codebooks are provably identical on every sheet the code reads**. “It
makes no computational difference” is NOT a reason to repoint. It was
tried on 2026-08-03 and reverted the same day.

So, to adopt a new codebook:

1.  Add the new dated `.xlsx` to `inst/2023-mht/`. Leave every older one
    in place.
2.  Create a new dated entry point that reads it.
3.  Leave the existing entry points, and the codebook each already
    names, untouched.

**A new codebook that ships and is read by nothing is the correct
intermediate state.** `dataDictionary20260803.xlsx` is exactly that
today.

Read `NEWS.md` under the version that introduced a suffix before
assuming what it dates. The `v<YYYYMMDD>` suffix on a function is **the
date that methodology was created** — not the study year, not the
release date, and not the delivery it serves.

## The codebook

`inst/2023-mht/` ships every dated codebook version. Only two sheets are
read by code:

| Sheet                         | Read by                                                     |
|-------------------------------|-------------------------------------------------------------|
| `MHT_groups`                  | the minimum-dose loop, and the product ladder’s source data |
| `post_grouping`               | the approach definitions                                    |
| `Rules`, `FDDD`, `covariates` | **nothing** — humans only                                   |

`Rules` item 4 is annotated **superseded** in
`dataDictionary20260803.xlsx`. Its whole-periods rounding rule was
replaced in November 2024 by a whole-months rule, which is what the
minimum-dose loop implements. The annotation states the difference and
the affected products. **The decision itself, and the correspondence
recording it, live in the study’s own project record, which is not
public** — do not restate either here.

**Edit a codebook `.xlsx` by surgical XML edit, never with a spreadsheet
writer.** `openxlsx2`, `openpyxl` and friends rewrite the whole file on
save, so a one-cell change can silently reformat a 158×11 sheet. Unzip,
edit the one shared string, copy every other zip entry byte-for-byte,
rezip. Then prove it: read every sheet from both files with `readxl` and
compare with [`identical()`](https://rdrr.io/r/base/identical.html).

## This repository is PUBLIC

`skalkidou-lab/mht` is a public repository, and `pkgdown` publishes
`NEWS.md`, `README.md` and this file to a public site — including
`search.json`, which indexes their full text.

**Never commit here:** a person’s name, an email address or its subject
line, quoted private correspondence, a path inside a private repository,
or any statement about unpublished findings, defects or manuscripts.
Keep the technical rule; drop who decided it and why it was raised.
Anything that identifies a person or a private artefact belongs in the
study’s private project record.

Check `git remote -v` and the repository’s visibility **before** writing
prose here, not after.

## Tests

``` bash
NOT_CRAN=true Rscript --vanilla -e 'suppressMessages(pkgload::load_all(".")); testthat::test_dir("tests/testthat", reporter="summary")'
```

`NOT_CRAN=true` matters — several tests `skip_on_cran()`, which fires
non-interactively and hides them silently. Grep the summary for `SKIP`.

`inst/testdata/` holds frozen goldens with a `MANIFEST.sha256`, captured
to prove the `swereg` → `mht` extraction changed nothing. **Preserve
them immutably; do not re-capture over them.** A repaired baseline goes
in a separate file.

## Downstream

Two pipelines call this package, so **any change here changes both**:

- one calls
  [`add_lmed_v20230509()`](https://skalkidou-lab.github.io/mht/reference/add_lmed_v20230509.md)
  with `id_name = "p1163_lopnr_personnr"`
- one calls
  [`add_lmed_v20250909()`](https://skalkidou-lab.github.io/mht/reference/add_lmed_v20250909.md)
  with `id_name = "lopnr"`

**The consuming pipeline’s phase hash does NOT follow the `mht::`
call.** It hashes the body and formals of its own wrapper function only.
So changing exposure logic here, reinstalling, and re-running silently
keeps the OLD classification — no error, no warning, and an identical
summary table. After any behavioural change here, the consumer must edit
that wrapper’s **body** by hand to force its hash to move.
