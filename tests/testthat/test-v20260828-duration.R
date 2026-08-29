# Pins the 2026-08-28 duration layer: the one applicable codebook rule, the
# strength-keyed Utrogestan pair, the intrauterine devices, and the rows that
# contribute nothing.
#
# EVERY NUMBER THE LAYER USES COMES FROM THE CODEBOOK. The first block reads
# the cells and asserts them, so every later expectation names a value this
# file has already shown the sheet to carry.

library(data.table)

absorb <- function(expr) {
  tryCatch(expr, error = function(e) paste0("ERROR: ", conditionMessage(e)))
}

codebook_rows <- function() {
  path <- system.file(
    "2023-mht",
    "dataDictionary20260828.xlsx",
    package = "mht"
  )
  expect_true(nzchar(path))
  wb <- suppressMessages(readxl::read_excel(
    path,
    sheet = "MHT_groups",
    col_types = "text"
  ))
  wb <- as.data.frame(wb, stringsAsFactors = FALSE)
  wb[!is.na(wb$Preparatnamn), , drop = FALSE]
}

durations <- function(...) {
  mht:::lmed_durations_v20260828(data.table(...), verbose = FALSE)
}

# ------------------------------------------------------------- the codebook ----

test_that("the codebook carries the numbers the layer reads", {
  wb <- codebook_rows()

  u <- wb[wb$Preparatnamn == "Utrogestan", ]
  expect_identical(nrow(u), 2L)
  expect_identical(u$strength_mg_min, c(NA, "150"))
  expect_identical(u$strength_mg_max, c("150", NA))
  expect_identical(as.numeric(u$minimum_monthly_dose), c(28, 12))
  expect_identical(as.numeric(u$minimum_months), c(1, 1))

  iud <- wb[!is.na(wb$FDDD), c("Preparatnamn", "Subgrupp", "FDDD")]
  expect_identical(
    iud$Preparatnamn,
    c("Jadelle", "Jaydess", "Kyleena", "Levosert", "Levosertone", "Mirena")
  )
  expect_identical(iud$Subgrupp, c("D3", rep("E1", 5L)))
  expect_identical(
    as.numeric(iud$FDDD),
    c(1680, 1008, 1680, 1680, 1680, 1680)
  )

  # Utrogest carried its own minimum_monthly_dose in the frozen codebook and
  # carries none here. That row is what let one prescription take two rules.
  expect_false("Utrogest" %in% wb$Preparatnamn[!is.na(wb$minimum_monthly_dose)])
})

test_that("the rule reader returns one row per codebook product", {
  rules <- mht:::lmed_read_product_rules_v20260828()
  expect_identical(nrow(rules), nrow(codebook_rows()))
  expect_true(all(nzchar(rules$name_clean)))
  # The normalisation is the ladder's own, so a rule and a rung cannot differ.
  expect_identical(
    rules$name_clean[rules$preparatnamn == "Primolut-Nor"],
    "primolutnor"
  )
  expect_identical(
    rules$name_clean[rules$preparatnamn == "Progesteron MIC APL"],
    "progesteronmicapl"
  )
})

# ---------------------------------------------------------- the strength read ----

test_that("the strength is the last milligram value, and a concentration is none", {
  strength <- mht:::lmed_product_strength_mg_v20260828
  expect_identical(
    strength(c(
      "Utrogestan, vaginalkapsel, mjuk 200 mg",
      "Utrogestan, kapsel, mjuk 100 mg",
      "Jaydess, intrauterint inlagg 13,5 mg",
      "UTROGESTAN 200 MG"
    )),
    c(200, 100, 13.5, 200)
  )
  # A concentration names milligrams per gram, not a unit strength.
  expect_identical(
    strength(c("Divigel, gel 1 mg/g", "Estradot, plaster 25 mikrogram/dygn")),
    c(NA_real_, NA_real_)
  )
  expect_identical(strength(NA_character_), NA_real_)
  # A latin1 byte declared "unknown" is what a direct fread() of the register
  # produces. The strip runs before tolower(), so it never reaches it.
  unknown_latin1 <- rawToChar(as.raw(c(
    0x31,
    0x30,
    0x30,
    0x20,
    0x6d,
    0x67,
    0xae
  )))
  expect_identical(Encoding(unknown_latin1), "unknown")
  expect_identical(absorb(strength(unknown_latin1)), 100)
})

# ------------------------------------------------------ one rule, applied once ----

test_that("Utrogestan takes one strength-keyed rule and never compounds", {
  d <- durations(
    lopnr = 1:4,
    produkt = "Utrogestan 100mg Capsules",
    lnmn = c(
      "Utrogestan, kapsel, mjuk 100 mg",
      "Utrogestan, kapsel, mjuk 100 mg",
      "Utrogestan, vaginalkapsel, mjuk 200 mg",
      "Utrogestan, vaginalkapsel, mjuk 200 mg"
    ),
    edatum = as.Date("2020-01-06"),
    fddd = c(30, 300, 30, 300)
  )
  # 100 mg takes 28 days of supply per month: floor(30/28) = 1 month, and
  # floor(300/28) = 10 months. 200 mg takes 12: floor(30/12) = 2 months, and
  # floor(300/12) = 25 months. A month of treatment is 28 days.
  expect_identical(d$duration_days, c(28, 280, 56, 700))
  expect_identical(d$product_category, rep("C1", 4L))
})

test_that("v20250909 compounds the same prescription and v20260828 does not", {
  # `before` is the frozen entry point end to end, `after` is the repaired
  # duration layer end to end. Both sides are asserted, so the pin reads as a
  # change and not as an assertion out of nowhere.
  weeks <- cstime::date_to_isoyearweek_c(
    seq(as.Date("2019-12-30"), as.Date("2021-12-27"), by = "week")
  )
  skeleton <- data.table(id = 1L, isoyearweek = weeks)
  lmed <- data.table(
    lopnr = 1L,
    produkt = "Utrogestan 100mg Capsules",
    lnmn = "Utrogestan, kapsel, mjuk 100 mg",
    edatum = as.Date("2020-01-06"),
    fddd = 30
  )

  # The frozen layer matched `Utrogest` and then `Utrogestan` in its frozen
  # codebook, so 30 days of supply became 84 and then 196.
  before <- copy(skeleton)
  suppressWarnings(add_lmed_v20250909(
    before,
    copy(lmed)[, lnmn := NULL],
    verbose = FALSE
  ))
  expect_identical(sum(before$C1), 29L)

  after <- copy(skeleton)
  d <- mht:::lmed_durations_v20260828(lmed, verbose = FALSE)
  expect_identical(d$duration_days, 28)
  mht:::apply_lmed_categories_to_skeleton_v20260828(
    after,
    d[, .(lopnr, start_isoyearweek, stop_isoyearweek, product_category)],
    verbose = FALSE
  )
  expect_identical(sum(after$C1), 4L)
})

test_that("the longest codebook name wins, and a merely contained name loses", {
  d <- durations(
    lopnr = 1:3,
    produkt = c("Levosertone 20 mikrogram", "Depo-Provera", "Provera"),
    edatum = as.Date("2020-01-06"),
    fddd = c(NA, 90, 90)
  )
  # Levosertone starts with `levosert` and with `levosertone`. Both rules give
  # 1680, and the point is that exactly one of them applies.
  # Depo-Provera CONTAINS Provera and does not start with it, so the Provera
  # minimum-dose rule leaves it alone. The frozen layer matched it there.
  # Provera takes floor(90/4) = 22 months, times 28 days.
  expect_identical(d$duration_days, c(1680, 90, 616))
  expect_identical(d$product_category, c("E1", "D1", "C4"))
})

test_that("two rules of equal specificity are an error, not a second application", {
  rules <- data.table(
    preparatnamn = c("Utrogestan", "Utrogestan"),
    name_clean = c("utrogestan", "utrogestan"),
    fddd_fixed = c(NA_real_, NA_real_),
    monthly_dose = c(8, 12),
    months_min = c(3, 1),
    strength_min = c(NA_real_, NA_real_),
    strength_max = c(NA_real_, NA_real_)
  )
  expect_identical(
    absorb(mht:::lmed_select_codebook_rule_v20260828(
      "utrogestanmgcapsules",
      NA_real_,
      rules
    )),
    paste0(
      "ERROR: the codebook offers more than one rule of equal specificity ",
      "for 1 of 1 rows: utrogestanmgcapsules. One prescription takes one ",
      "rule, so the codebook must separate them."
    )
  )
})

# ---------------------------------------------------- the intrauterine devices ----

test_that("Jaydess reads 1008 from the codebook whatever the register spells", {
  d <- durations(
    lopnr = 1:4,
    produkt = c("Jaydess", "JAYDESS 13,5 mg", "Jaydess(R) 13,5 mg", "Kyleena"),
    edatum = as.Date("2020-01-06"),
    fddd = NA_real_
  )
  expect_identical(d$product_category, c("E1", "E1", "E1", "E1"))
  expect_identical(d$duration_days, c(1008, 1008, 1008, 1680))
})

test_that("D3 and E1 keep the flat 1680 overwrite, which a missing fddd needs", {
  d <- durations(
    lopnr = 1:5,
    produkt = c("Jadelle", "Mirena", "Levosert", "Levosertone", "Kyleena"),
    edatum = as.Date("2020-01-06"),
    fddd = NA_real_
  )
  expect_identical(d$product_category, c("D3", "E1", "E1", "E1", "E1"))
  expect_identical(d$duration_days, rep(1680, 5L))
})

# ------------------------------------------------------------ lnmn is required ----

test_that("lnmn is required as soon as a strength-keyed rule names a product", {
  x <- data.table(
    lopnr = 1L,
    produkt = "Utrogestan",
    edatum = as.Date("2020-01-06"),
    fddd = 30
  )
  # absorb() turns the throw into a value, so the expectation FAILS with both
  # sides visible. An error would stop the test before the expectation ran,
  # and an error can come from anywhere, including a broken fixture.
  expect_identical(
    absorb(mht:::lmed_durations_v20260828(x, verbose = FALSE)),
    paste0(
      "ERROR: lnmn is required: it decides the rule for 1 of 1 rows, whose ",
      "codebook rule is keyed on strength (Utrogestan). Supply lnmn, the ",
      "register product name that carries the strength."
    )
  )
})

test_that("an lnmn with no strength in milligrams is an error too", {
  x <- data.table(
    lopnr = 1L,
    produkt = "Utrogestan",
    lnmn = "Utrogestan, kapsel, mjuk",
    edatum = as.Date("2020-01-06"),
    fddd = 30
  )
  expect_identical(
    absorb(mht:::lmed_durations_v20260828(x, verbose = FALSE)),
    paste0(
      "ERROR: lnmn carries no strength in milligrams for 1 of 1 rows, whose ",
      "codebook rule is keyed on strength: Utrogestan, kapsel, mjuk"
    )
  )
})

test_that("lnmn is optional where no strength-keyed rule names a product", {
  d <- durations(
    lopnr = 1:2,
    produkt = c("Divigel", "Mirena"),
    edatum = as.Date("2020-01-06"),
    fddd = c(10, NA)
  )
  expect_identical(d$duration_days, c(10, 1680))
})

# --------------------------------------------------- what contributes nothing ----

test_that("a duration that is missing or not positive contributes nothing", {
  x <- data.table(
    lopnr = 1:5,
    produkt = "Divigel",
    edatum = as.Date(c(rep("2020-01-06", 4L), NA)),
    fddd = c(-1, NA, 0, 10, 10)
  )
  expect_warning(
    d <- mht:::lmed_durations_v20260828(x, verbose = FALSE),
    "4 of 5 classified prescriptions dropped"
  )
  expect_identical(d$lopnr, 4L)
  expect_identical(d$duration_days, 10)
})

test_that("a missing fddd is never imputed", {
  # A prescription can carry no fddd. Where it is an intrauterine device the
  # category supplies the duration. Every other such prescription contributes
  # nothing, and no duration is invented for it.
  x <- data.table(
    lopnr = 1:2,
    produkt = c("Divigel", "Mirena"),
    edatum = as.Date("2020-01-06"),
    fddd = NA_real_
  )
  expect_warning(
    d <- mht:::lmed_durations_v20260828(x, verbose = FALSE),
    "1 of 2 classified prescriptions dropped"
  )
  expect_identical(d$produkt, "Mirena")
})

test_that("a product that reaches no category contributes nothing", {
  d <- durations(
    lopnr = 1:2,
    produkt = c("Ceranor", "Divigel"),
    edatum = as.Date("2020-01-06"),
    fddd = 10
  )
  expect_identical(d$produkt, "Divigel")
})

test_that("the caller's lmed is not modified", {
  x <- data.table(
    lopnr = 1L,
    produkt = "Divigel",
    edatum = as.Date("2020-01-06"),
    fddd = 10
  )
  mht:::lmed_durations_v20260828(x, verbose = FALSE)
  expect_identical(names(x), c("lopnr", "produkt", "edatum", "fddd"))
  expect_identical(x$fddd, 10)
})
