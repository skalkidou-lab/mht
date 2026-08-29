# Pins the 2026-08-28 approach layer. The subjects are:
#
#   * the codebook the approach rules come from
#   * the tibolone group
#   * the clash that ends with the treated episode
#   * the eight `rd_approach*` columns
#   * the `Utrogest` rule alias
#
# THE LAYER MUST CALL ITS OWN HELPERS. A resolver that calls a frozen
# `_v20250909` helper is correct-looking and unreachable, and every earlier test
# of this package stays green while it happens. Two tests below catch it. One
# reads the deparsed bodies. One measures the four-week bridge, which is the
# behaviour the two helper versions disagree on.

library(data.table)

absorb <- function(expr) {
  tryCatch(expr, error = function(e) paste0("ERROR: ", conditionMessage(e)))
}

# A compact, fully pinned reading of a person's labels over follow-up.
runs <- function(v) {
  r <- rle(v)
  return(paste0(r$lengths, " ", r$values))
}

grid <- function(ids, from, n) {
  weeks <- cstime::date_to_isoyearweek_c(
    seq(as.Date(from), by = "week", length.out = n)
  )
  return(CJ(id = ids, isoyearweek = weeks, sorted = TRUE))
}

# The whole 2026-08-28 path from dispensed prescriptions to approach labels.
approaches <- function(lmed, n = 300L, from = "2010-01-04", ids = 1L) {
  skeleton <- grid(ids, from, n)
  d <- mht:::lmed_durations_v20260828(lmed, verbose = FALSE)
  mht:::apply_lmed_categories_to_skeleton_v20260828(
    skeleton,
    d[, .(lopnr, start_isoyearweek, stop_isoyearweek, product_category)],
    verbose = FALSE
  )
  mht:::apply_lmed_approaches_to_skeleton_v20260828(skeleton)
  return(skeleton)
}

# One person-week grid with every product-category column and no exposure.
empty_grid <- function() {
  skeleton <- grid(1L, "2010-01-04", 1L)
  mht:::apply_lmed_categories_to_skeleton_v20260828(
    skeleton,
    data.table(
      lopnr = integer(0),
      start_isoyearweek = character(0),
      stop_isoyearweek = character(0),
      product_category = character(0)
    ),
    verbose = FALSE
  )
  return(skeleton)
}

rule_categories <- function() {
  rules <- mht:::lmed_read_approach_rules_v20260828()
  read <- unlist(lapply(rules, function(r) c(r$includes, r$excludes)))
  return(sort(unique(read)))
}

livial <- function() {
  return(data.table(
    lopnr = 1L,
    produkt = "Livial",
    edatum = as.Date("2010-06-07"),
    fddd = 365
  ))
}

# Divigel is transdermal oestrogen and Femanest is peroral oestrogen. Dispensed
# on one day they start together, their run lengths stay equal, and the
# run-length rule cannot separate them. The third row restarts Divigel alone,
# more than four weeks after the first episode ends.
clashing <- function() {
  return(data.table(
    lopnr = 1L,
    produkt = c("Divigel", "Femanest", "Divigel"),
    edatum = as.Date(c("2010-06-07", "2010-06-07", "2015-01-05")),
    fddd = c(1200, 100, 300)
  ))
}

# ============================================================ the invariant ====
#
# Every classified category is read by at least one approach rule, or is
# deliberately and visibly not.

test_that("the person-week grid carries every category an approach rule reads", {
  skeleton <- empty_grid()
  expect_identical(
    absorb(setdiff(rule_categories(), names(skeleton))),
    character(0)
  )
  expect_identical(length(rule_categories()), 31L)
})

test_that("exactly two grid categories are read by no approach rule", {
  # Both are deliberate and both are visible. `dev/check-crosswalk.R` reports
  # them, and D4 sits on its UNUSED_CATEGORIES register. No product reaches D4,
  # and H1 is the androgens, which the coauthors count as no MHT.
  skeleton <- empty_grid()
  grid_categories <- setdiff(names(skeleton), c("id", "isoyearweek"))
  expect_identical(length(grid_categories), 33L)
  expect_identical(
    absorb(setdiff(grid_categories, rule_categories())),
    c("D4", "H1")
  )
})

test_that("a rule that reads an absent category is an error, not a silent no-op", {
  skeleton <- empty_grid()
  skeleton[, F1 := NULL]
  expect_identical(
    absorb(mht:::apply_lmed_approaches_to_skeleton_v20260828(skeleton)),
    paste0(
      "ERROR: the approach rules read F1, which the person-week grid does ",
      "not carry. Every category a rule reads must be a column of the ",
      "skeleton."
    )
  )
})

test_that("the approach rules come from the 2026-08-28 codebook alone", {
  body_text <- paste(
    deparse(body(utils::removeSource(
      mht:::lmed_read_approach_rules_v20260828
    ))),
    collapse = " "
  )
  expect_true(grepl("dataDictionary20260828.xlsx", body_text, fixed = TRUE))
  expect_false(grepl("dataDictionary20241105.xlsx", body_text, fixed = TRUE))
  # The sheet is read, never rebuilt as R source and evaluated.
  expect_false(grepl("glue", body_text, fixed = TRUE))
  expect_false(grepl("eval(parse", body_text, fixed = TRUE))
})

test_that("the resolver names no category of its own", {
  # Every category enters through post_grouping. A literal here would put one
  # definition in the sheet and a second in the code.
  body_text <- paste(
    deparse(body(utils::removeSource(
      mht:::apply_lmed_approaches_to_skeleton_v20260828
    ))),
    collapse = " "
  )
  expect_identical(
    absorb(unlist(regmatches(
      body_text,
      gregexpr("\"[A-Z][0-9]+\"", body_text)
    ))),
    character(0)
  )
})

# ====================================================== the frozen helpers ====

test_that("no frozen 2025-09-09 identifier reaches the 2026-08-28 approach layer", {
  fns <- c(
    "apply_lmed_approaches_to_skeleton_v20260828",
    "lmed_read_approach_rules_v20260828",
    "lmed_rule_cells_v20260828",
    "lmed_assert_categories_present_v20260828",
    "lmed_light_approach_variables_v20260828",
    "lmed_carry_clash_v20260828",
    "cumulative_reset_v20260828",
    "first_non_na_v20260828",
    "rd_approach_columns_v20260828",
    "create_exposure_variables_v20260828"
  )
  frozen <- vapply(
    fns,
    function(nm) {
      fn <- get(nm, envir = asNamespace("mht"))
      text <- paste(deparse(body(utils::removeSource(fn))), collapse = " ")
      return(grepl("_v20250909", text, fixed = TRUE))
    },
    logical(1)
  )
  expect_identical(names(frozen)[frozen], character(0))
})

test_that("the resolver bridges no gap at the start or the end of follow-up", {
  # The behaviour the two bridge helpers disagree on. The frozen helper converts
  # a leading and a trailing FALSE run of four weeks or less. A woman then starts
  # treatment up to four weeks early. She stops up to four weeks late.
  #
  # Divigel covers 91 days from the Monday of the fourth week of the grid. Three
  # untreated weeks sit before it and four sit after it.
  skeleton <- approaches(
    data.table(
      lopnr = 1L,
      produkt = "Divigel",
      edatum = as.Date("2010-01-25"),
      fddd = 91
    ),
    n = 20L
  )
  expect_identical(
    absorb(runs(skeleton$A1)),
    c("3 FALSE", "13 TRUE", "4 FALSE")
  )
  expect_identical(
    absorb(runs(skeleton$approach1)),
    c("3 local_or_none_mht", "13 systemic_mht", "4 local_or_none_mht")
  )
})

test_that("the resolver leaves behind the three approach columns and nothing else", {
  skeleton <- approaches(livial())
  expect_identical(
    absorb(setdiff(
      names(skeleton),
      c(
        "id",
        "isoyearweek",
        setdiff(names(empty_grid()), c("id", "isoyearweek"))
      )
    )),
    c("approach1", "approach2", "approach3")
  )
})

# ============================================================ Q8, tibolone ====

test_that("tibolone is its own group in all three approaches", {
  skeleton <- approaches(livial())
  expect_identical(absorb(sum(skeleton$F1)), 53L)
  for (a in c("approach1", "approach2", "approach3")) {
    expect_identical(
      absorb(runs(skeleton[[a]])),
      c("22 local_or_none_mht", "53 tibolone", "225 local_or_none_mht")
    )
  }
})

test_that("the frozen layer counts the same woman as an unexposed control", {
  # The `before` side. The frozen codebook names F1 in no approach rule, so a
  # woman on tibolone alone reads as untreated in every week of follow-up. She
  # enters a systemic-MHT comparison as a control.
  skeleton <- grid(1L, "2010-01-04", 300L)
  suppressWarnings(add_lmed_v20250909(skeleton, livial(), verbose = FALSE))
  expect_identical(absorb(sum(skeleton$F1)), 53L)
  expect_identical(absorb(runs(skeleton$approach1)), "300 local_or_none_mht")
  expect_identical(
    absorb(runs(skeleton$rd_approach1_single)),
    "300 local_or_none_mht"
  )
})

# =========================================================== Q7, the clash ====

test_that("a clash ends with the treated episode, and she returns as a former user", {
  # 22 untreated weeks, then Divigel and Femanest start together. The clash runs
  # to the last week of that episode, which is 172 weeks. It does not stop when
  # Femanest runs out, because she is treated throughout.
  #
  # Her first untreated week is an ordinary stop, so the 67 weeks that follow
  # record her as a former user. The episode ran past the three-year minimum.
  skeleton <- approaches(clashing(), n = 400L)
  expect_identical(
    absorb(runs(skeleton$approach2)),
    c(
      "22 local_or_none_mht",
      "172 clashingprescriptions",
      "67 local_or_none_mht",
      "43 transdermal_estrogen",
      "96 local_or_none_mht"
    )
  )

  mht:::create_exposure_variables_v20260828(skeleton)
  expect_identical(
    absorb(runs(skeleton$rd_approach2_multiple)),
    c(
      "22 local_or_none_mht",
      "172 clashingprescriptions",
      "67 previous",
      "43 transdermal_estrogen",
      "96 exclude"
    )
  )
  # The `single` variant models one lifetime episode, so the second one is
  # excluded. That is the variant's own rule and not the clash.
  expect_identical(
    absorb(runs(skeleton$rd_approach2_single)),
    c(
      "22 local_or_none_mht",
      "172 clashingprescriptions",
      "67 previous",
      "139 exclude"
    )
  )
})

test_that("the frozen layer carries the same clash to the end of follow-up", {
  # The `before` side. 378 of 400 weeks, against 172 weeks of treatment. The
  # woman never returns to the cohort, and no week of hers is ever `previous`.
  skeleton <- grid(1L, "2010-01-04", 400L)
  suppressWarnings(add_lmed_v20250909(skeleton, clashing(), verbose = FALSE))
  expect_identical(
    absorb(runs(skeleton$approach2)),
    c("22 local_or_none_mht", "378 clashingprescriptions")
  )
  expect_identical(
    absorb(runs(skeleton$rd_approach2_multiple)),
    c("22 local_or_none_mht", "378 clashingprescriptions")
  )
})

test_that("approach 1 clashes now that tibolone is its own group", {
  # Approach 1 carried one treatment variable under the frozen codebook, so the
  # tie test could never be met and it could not clash at all. It carries two
  # now, and a woman who starts systemic MHT and tibolone in one week clashes.
  skeleton <- approaches(
    data.table(
      lopnr = 1L,
      produkt = c("Divigel", "Livial"),
      edatum = as.Date("2010-06-07"),
      fddd = c(200, 200)
    )
  )
  expect_identical(
    absorb(runs(skeleton$approach1)),
    c(
      "22 local_or_none_mht",
      "29 clashingprescriptions",
      "249 local_or_none_mht"
    )
  )
})

test_that("one woman's clash reaches no other woman", {
  # The clash carry groups by person and by treated episode. Drop the person
  # from that grouping and one woman's clash reaches every woman whose episode
  # carries the same number.
  #
  # Woman 1 starts Divigel and Femanest in one week, so she clashes. Woman 2
  # holds tibolone alone. Woman 3 starts Divigel, then adds tibolone a year
  # later, so her two runs differ and she never clashes.
  skeleton <- approaches(
    data.table(
      lopnr = c(1L, 1L, 2L, 3L, 3L),
      produkt = c("Divigel", "Femanest", "Livial", "Divigel", "Livial"),
      edatum = as.Date(c(
        "2010-06-07",
        "2010-06-07",
        "2010-06-07",
        "2010-06-07",
        "2011-06-06"
      )),
      fddd = c(200, 200, 365, 400, 200)
    ),
    ids = 1:3
  )
  expect_identical(
    absorb(runs(skeleton[id == 1L]$approach2)),
    c(
      "22 local_or_none_mht",
      "29 clashingprescriptions",
      "249 local_or_none_mht"
    )
  )
  expect_identical(
    absorb(runs(skeleton[id == 2L]$approach2)),
    c("22 local_or_none_mht", "53 tibolone", "225 local_or_none_mht")
  )
  # Two treatments that start in different weeks carry different run lengths,
  # so the resolver reports the one she started most recently.
  expect_identical(
    absorb(runs(skeleton[id == 3L]$approach2)),
    c(
      "22 local_or_none_mht",
      "52 transdermal_estrogen",
      "29 tibolone",
      "197 local_or_none_mht"
    )
  )
})

# ================================================== repair 10, the rd columns ====

rd_columns <- function(skeleton) {
  return(sort(grep("^rd_approach", names(skeleton), value = TRUE)))
}

eight_rd_columns <- sort(c(
  "rd_approach1_single",
  "rd_approach1_multiple",
  "rd_approach2_single",
  "rd_approach2_multiple",
  "rd_approach3_single",
  "rd_approach3_multiple",
  "rd_approach3b_single",
  "rd_approach3b_multiple"
))

test_that("create_rd defaults to TRUE and writes the eight exposure columns", {
  skeleton <- approaches(livial())
  mht:::create_exposure_variables_v20260828(skeleton)
  expect_identical(absorb(rd_columns(skeleton)), eight_rd_columns)
})

test_that("create_rd = FALSE writes no exposure column and deletes a stale one", {
  # A stale column reads as this run's answer and is not. It is a silent wrong
  # answer, so the skipped run must delete it rather than leave it.
  skeleton <- approaches(livial())
  skeleton[, rd_approach1_single := "STALE"]
  skeleton[, rd_approach3b_multiple := "STALE"]
  mht:::create_exposure_variables_v20260828(skeleton, create_rd = FALSE)

  expect_identical(absorb(rd_columns(skeleton)), character(0))
  # The approach columns survive: only the exposure layer is skipped.
  expect_identical(
    absorb(intersect(
      names(skeleton),
      c("approach1", "approach2", "approach3")
    )),
    c("approach1", "approach2", "approach3")
  )
})

test_that("create_rd = FALSE and create_rd = TRUE agree on the approach columns", {
  kept <- approaches(livial())
  skipped <- copy(kept)
  mht:::create_exposure_variables_v20260828(kept)
  mht:::create_exposure_variables_v20260828(skipped, create_rd = FALSE)
  expect_identical(
    absorb(kept[, .(approach1, approach2, approach3)]),
    skipped[, .(approach1, approach2, approach3)]
  )
})

# ==================================================== the `Utrogest` alias ====

test_that("Utrogest takes the Utrogestan rules, in both strength bands", {
  # The clinician stated on 2026-08-26 that the two are one product. The
  # codebook carries `Utrogestan` alone, so without the alias `Utrogest` reaches
  # no minimum-dose rule and keeps its raw fddd of 30 days.
  d <- mht:::lmed_durations_v20260828(
    data.table(
      lopnr = 1:4,
      produkt = c(
        "Utrogest 100 mg",
        "Utrogestan 100 mg",
        "Utrogest 200 mg",
        "Utrogestan 200 mg"
      ),
      lnmn = c(
        "Utrogest, kapsel, mjuk 100 mg",
        "Utrogestan, kapsel, mjuk 100 mg",
        "Utrogest, vaginalkapsel, mjuk 200 mg",
        "Utrogestan, vaginalkapsel, mjuk 200 mg"
      ),
      edatum = as.Date("2020-01-06"),
      fddd = 30
    ),
    verbose = FALSE
  )
  expect_identical(absorb(d$duration_days), c(28, 28, 56, 56))
  expect_identical(absorb(d$product_category), rep("C1", 4L))
})

test_that("the alias renames the rule and never the category", {
  expect_identical(
    absorb(mht:::lmed_rule_name_v20260828(c(
      "utrogestmg",
      "utrogestanmgcapsules",
      "divigel",
      NA_character_
    ))),
    c("utrogestanmg", "utrogestanmgcapsules", "divigel", NA_character_)
  )
  x <- data.table(produkt = c("Utrogest 100 mg", "Utrogestan 100 mg"))
  mht:::lmed_categorize_product_names_v20260828(x)
  expect_identical(absorb(x$produkt_clean), c("utrogestmg", "utrogestanmg"))
})

test_that("the rule reader still returns one row per codebook product", {
  # The alias adds no row. A second `Utrogestan` row would give one
  # prescription two rules of equal specificity.
  expect_identical(
    absorb(nrow(mht:::lmed_read_product_rules_v20260828())),
    116L
  )
})

test_that("an alias that names a codebook product is an error", {
  rules <- data.table(
    preparatnamn = c("Utrogest", "Utrogestan"),
    name_clean = c("utrogest", "utrogestan"),
    fddd_fixed = NA_real_,
    monthly_dose = c(28, 28),
    months_min = c(1, 1),
    strength_min = NA_real_,
    strength_max = NA_real_
  )
  expect_identical(
    absorb(mht:::lmed_assert_aliases_live_v20260828(rules)),
    paste0(
      "ERROR: the codebook now carries a rule named utrogest, which ",
      "lmed_rule_aliases_v20260828() also names. Drop the alias."
    )
  )
})

test_that("an alias whose target leaves the codebook is an error", {
  rules <- data.table(
    preparatnamn = "Crinone",
    name_clean = "crinone",
    fddd_fixed = NA_real_,
    monthly_dose = 12,
    months_min = 1,
    strength_min = NA_real_,
    strength_max = NA_real_
  )
  expect_identical(
    absorb(mht:::lmed_assert_aliases_live_v20260828(rules)),
    paste0(
      "ERROR: an alias of lmed_rule_aliases_v20260828() takes the rules of ",
      "utrogestan, which the codebook does not carry"
    )
  )
})

# ====================== the duration screen runs before the strength ====
#
# The alias made `Utrogest` strength-keyed. A strength and an `fddd` are missing
# together in the 2026 delivery. A strength read before the duration screen then
# turns a silently dropped row into a hard stop. 63,276 rows of the Utrogestan
# family carry neither, and 48,248 of the 48,846 `Utrogest` rows are among them.

durations <- function(...) {
  return(suppressWarnings(mht:::lmed_durations_v20260828(
    data.table(...),
    verbose = FALSE
  )))
}

test_that("a row with no strength and no fddd is dropped, not an error", {
  # The whole first batch stops without this. Both spellings are affected, so
  # the screen is not Utrogest-specific.
  expect_identical(
    absorb(nrow(durations(
      lopnr = 1L,
      produkt = c("Utrogest", "Utrogestan"),
      edatum = as.Date("2020-01-06"),
      fddd = NA_real_,
      lnmn = NA_character_
    ))),
    0L
  )
  # `lnmn` absent as a column reaches the other error, and must also be screened.
  expect_identical(
    absorb(nrow(durations(
      lopnr = 1L,
      produkt = c("Utrogest", "Utrogestan"),
      edatum = as.Date("2020-01-06"),
      fddd = NA_real_
    ))),
    0L
  )
})

test_that("no usable duration reaches the strength requirement, whatever the cause", {
  # A negative duration, an infinite one and a missing dispensing date each
  # leave the prescription with no interval. None of them needs a strength.
  for (case in list(
    list(fddd = -5, edatum = as.Date("2020-01-06")),
    list(fddd = 0, edatum = as.Date("2020-01-06")),
    list(fddd = Inf, edatum = as.Date("2020-01-06")),
    list(fddd = 30, edatum = as.Date(NA))
  )) {
    expect_identical(
      absorb(nrow(durations(
        lopnr = 1L,
        produkt = "Utrogest",
        edatum = case$edatum,
        fddd = case$fddd,
        lnmn = NA_character_
      ))),
      0L
    )
  }
})

test_that("a duration with no readable strength is still an error", {
  # The case the error exists for. This row has an interval, and the strength
  # decides which rule gives it, so a wrong duration is the alternative.
  expect_identical(
    absorb(durations(
      lopnr = 1L,
      produkt = "Utrogest",
      edatum = as.Date("2020-01-06"),
      fddd = 30,
      lnmn = NA_character_
    )),
    paste0(
      "ERROR: lnmn carries no strength in milligrams for 1 of 1 rows, whose ",
      "codebook rule is keyed on strength: NA"
    )
  )
  expect_identical(
    absorb(durations(
      lopnr = 1L,
      produkt = "Utrogest",
      edatum = as.Date("2020-01-06"),
      fddd = 30
    )),
    paste0(
      "ERROR: lnmn is required: it decides the rule for 1 of 1 rows, whose ",
      "codebook rule is keyed on strength (Utrogestan). Supply lnmn, the ",
      "register product name that carries the strength."
    )
  )
})

test_that("a Utrogest row with a duration and a strength reaches an interval", {
  d <- durations(
    lopnr = 1:2,
    produkt = c("Utrogest 100 mg", "Utrogest 200 mg"),
    edatum = as.Date("2020-01-06"),
    fddd = c(30, 300),
    lnmn = c(
      "Utrogest, kapsel, mjuk 100 mg",
      "Utrogest, vaginalkapsel, mjuk 200 mg"
    )
  )
  # 100 mg takes 28 days of supply per month, so floor(30/28) is 1 month.
  # 200 mg takes 12, so floor(300/12) is 25 months. A month is 28 days.
  expect_identical(absorb(d$duration_days), c(28, 700))
  expect_identical(absorb(d$product_category), c("C1", "C1"))
  expect_identical(absorb(d$start_isoyearweek), c("2020-02", "2020-02"))
  expect_identical(absorb(d$stop_isoyearweek), c("2020-05", "2021-48"))
})

test_that("a supply below one whole month is dropped after the strength is read", {
  # The second screen. The minimum-dose rule reduces this supply to zero, and
  # only the strength says which rule applies.
  expect_identical(
    absorb(nrow(durations(
      lopnr = 1L,
      produkt = "Utrogestan",
      edatum = as.Date("2020-01-06"),
      fddd = 10,
      lnmn = "Utrogestan, kapsel, mjuk 100 mg"
    ))),
    0L
  )
})

test_that("a fixed duration that depends on a strength is an error", {
  # A fixed duration SUPPLIES the duration the screen reads, so it is applied
  # before any strength is known. A codebook that keys one on strength cannot
  # be resolved in that order, and the guard says so rather than reading NA.
  rules <- data.table(
    preparatnamn = "Jaydess",
    name_clean = "jaydess",
    fddd_fixed = 1008,
    monthly_dose = NA_real_,
    months_min = NA_real_,
    strength_min = NA_real_,
    strength_max = 20
  )
  x <- data.table(produkt_clean = "jaydess", fddd = NA_real_)
  expect_identical(
    absorb(mht:::lmed_apply_fixed_durations_v20260828(x, rules)),
    paste0(
      "ERROR: a codebook rule carries a fixed duration and a strength band ",
      "at once: Jaydess. A fixed duration is read before the strength is, ",
      "so it must not depend on one."
    )
  )
})

test_that("the codebook keys no fixed duration on a strength today", {
  rules <- mht:::lmed_read_product_rules_v20260828()
  fixed <- rules[!is.na(rules$fddd_fixed)]
  expect_identical(absorb(nrow(fixed)), 6L)
  expect_identical(
    absorb(fixed$preparatnamn[
      !is.na(fixed$strength_min) | !is.na(fixed$strength_max)
    ]),
    character(0)
  )
})
