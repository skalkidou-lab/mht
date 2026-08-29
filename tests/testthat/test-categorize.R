# Pins the product-name ladder as it behaves TODAY, defects included.
#
# The known misclassifications below are FROZEN on purpose and tracked at
# skalkidou-lab/structural-mht-registry-data#1. Do not "correct" an expectation
# here: when the ladder is fixed, the diff of this file is the record of what
# changed.

categorize <- function(fn, produkt) {
  x <- data.table::data.table(produkt = produkt)
  fn(x)
  x$product_category
}

both_ladders <- list(
  v20230509 = mht:::lmed_categorize_product_names_v20230509,
  v20250909 = mht:::lmed_categorize_product_names_v20250909
)

# `both_ladders` holds the two ladders every block above asserts. The
# 2026-08-28 ladder takes its own binding, and the blocks at the end of this
# file assert both values side by side.
ladder_20260828 <- mht:::lmed_categorize_product_names_v20260828

test_that("products that classify correctly keep their category", {
  correct <- c(
    Divigel = "A1",
    Estradot = "A1",
    Progynon = "A2",
    Vagifem = "A3",
    Ovesterin = "A4",
    Oestriolaspen = "A5",
    Presomen = "A6",
    Neofollin = "A7",
    Kliogest = "B2",
    Femostonconti = "B4",
    Trisekvens = "B8",
    Crinone = "C1",
    Cerazette = "C3",
    Provera = "C4",
    Nexplanon = "D2",
    Jadelle = "D3",
    Mirena = "E1",
    Livial = "F1",
    Duavive = "G1",
    Nebido = "H1",
    MiniPe = "I1",
    Exlutena = "I2"
  )
  for (nm in names(both_ladders)) {
    expect_identical(
      categorize(both_ladders[[nm]], names(correct)),
      unname(correct),
      info = nm
    )
  }
})

test_that("a product name absent from the ladder is NA", {
  for (nm in names(both_ladders)) {
    expect_identical(
      categorize(both_ladders[[nm]], c("Paracetamol", "Ibuprofen")),
      c(NA_character_, NA_character_),
      info = nm
    )
  }
})

test_that("spaces are stripped from the product name but hyphens are not", {
  # `produkt_clean` is assigned twice: the hyphen-stripping assignment is
  # immediately overwritten by the space-stripping one, so only spaces survive.
  x <- data.table::data.table(produkt = c("Femoston conti", "Femoston-conti"))
  mht:::lmed_categorize_product_names_v20250909(x)
  expect_identical(x$produkt_clean, c("Femostonconti", "Femoston-conti"))
  expect_identical(x$product_category, c("B4", "B11"))
})

test_that("PINNED DEFECT: `Depo-Provera` falls through to C4, never D1", {
  # Issue #1. Two faults compound. The hyphen is never stripped, so the
  # `DepoProvera` pattern cannot match `Depo-Provera`; and even for the
  # unhyphenated spelling the `Provera` -> C4 rung sits ABOVE the
  # `DepoProvera` -> D1 rung, so D1 is unreachable for every input.
  for (nm in names(both_ladders)) {
    expect_identical(
      categorize(both_ladders[[nm]], "Depo-Provera®"),
      "C4",
      info = nm
    )
    expect_identical(
      categorize(both_ladders[[nm]], "DepoProvera"),
      "C4",
      info = nm
    )
  }
})

test_that("PINNED DEFECT: `Mini-Pe` is NA because the hyphen is never stripped", {
  # Issue #1. The ladder holds `MiniPe` -> I1, but `produkt_clean` still
  # carries the hyphen, so the register spelling never matches.
  for (nm in names(both_ladders)) {
    expect_identical(
      categorize(both_ladders[[nm]], "Mini-Pe®"),
      NA_character_,
      info = nm
    )
    # the unhyphenated spelling does reach I1, which is what makes the
    # hyphen the whole cause
    expect_identical(categorize(both_ladders[[nm]], "MiniPe"), "I1", info = nm)
  }
})

test_that("PINNED DEFECT: `Primolut-Nor` is NA (hyphen and registered sign)", {
  # Issue #1. `PrimolutNor` -> C4 exists, but neither the hyphen nor the
  # registered-trademark sign is removed from `produkt_clean`.
  for (nm in names(both_ladders)) {
    expect_identical(
      categorize(both_ladders[[nm]], "Primolut®-Nor"),
      NA_character_,
      info = nm
    )
    expect_identical(
      categorize(both_ladders[[nm]], "PrimolutNor"),
      "C4",
      info = nm
    )
  }
})

test_that("PINNED DEFECT: `Oestriol Aspen` is NA (matching is case-sensitive)", {
  # Issue #1. Stripping the space yields `OestriolAspen`, but the ladder
  # pattern is spelled `Oestriolaspen` and `stringr::str_detect()` is
  # case-sensitive by default.
  for (nm in names(both_ladders)) {
    expect_identical(
      categorize(both_ladders[[nm]], "Oestriol Aspen"),
      NA_character_,
      info = nm
    )
    expect_identical(
      categorize(both_ladders[[nm]], "Oestriolaspen"),
      "A5",
      info = nm
    )
  }
})

test_that("the 2023 and 2026 ladders return the same category for every probe", {
  probe <- c(
    "Depo-Provera®",
    "DepoProvera",
    "Mini-Pe®",
    "MiniPe",
    "Primolut®-Nor",
    "PrimolutNor",
    "Oestriol Aspen",
    "Oestriolaspen",
    "Divigel",
    "Vagifem",
    "Nexplanon",
    "Mirena",
    "Livial",
    "Kliogest",
    "Trisekvens",
    "Duphaston",
    "Provera",
    "Presomen",
    "Femoston conti",
    "Cerazette",
    "Jaydess",
    "Duavive",
    "Nebido",
    "Exlutena",
    "Paracetamol"
  )
  expect_identical(
    categorize(both_ladders$v20230509, probe),
    categorize(both_ladders$v20250909, probe)
  )
})

# ----------------------------------------------- the 2026-08-28 ladder ----

test_that("v20260828: every spelling this file probes reaches a category", {
  # One table, both values. `v20250909` is what the ladders of `both_ladders`
  # return; `v20260828` is what the 2026-08-28 ladder returns.
  #
  # One normaliser accounts for four of the six. It keeps the ASCII letters of
  # the name and lowercases them. The hyphen, the space, the registered sign and
  # the letter case then stop deciding the answer. `Depo-Provera` and
  # `Duphaston` also need the ladder to match with `startsWith()`, so that
  # `Provera` cannot answer for `Depo-Provera`.
  probe <- data.frame(
    produkt = c(
      "Depo-Provera\u00ae",
      "DepoProvera",
      "Mini-Pe\u00ae",
      "Primolut\u00ae-Nor",
      "Oestriol Aspen",
      "Duphaston"
    ),
    v20250909 = c("C4", "C4", NA, NA, NA, "C4"),
    v20260828 = c("D1", "D1", "I1", "C4", "A5", "C5"),
    stringsAsFactors = FALSE
  )
  for (nm in names(both_ladders)) {
    expect_identical(
      categorize(both_ladders[[nm]], probe$produkt),
      probe$v20250909,
      info = nm
    )
  }
  expect_identical(categorize(ladder_20260828, probe$produkt), probe$v20260828)
})

test_that("v20260828: the hyphen is stripped, so both spellings reach B4", {
  # `produkt_clean` is the same string for both spellings, and both reach one
  # category.
  x <- data.table::data.table(produkt = c("Femoston conti", "Femoston-conti"))
  ladder_20260828(x)
  expect_identical(x$produkt_clean, c("femostonconti", "femostonconti"))
  expect_identical(x$product_category, c("B4", "B4"))
})

test_that("v20260828: a product name absent from the ladder is NA", {
  expect_identical(
    categorize(ladder_20260828, c("Paracetamol", "Ibuprofen")),
    c(NA_character_, NA_character_)
  )
})
