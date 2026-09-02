# The 2026-09-02 layer looks a product up in a table. These pin what that
# changes: an unknown product stops the run, and a prefix no longer matches.

fixture <- function() {
  skeleton <- data.table::copy(mht::fake_skeleton_mht)
  lmed <- data.table::setDT(data.table::copy(mht::fake_lmed_2026))
  lmed[, lnmn := NA_character_]
  lmed[produkt == "Utrogestan", lnmn := "Utrogestan, kapsel, mjuk 100 mg"]
  # `Paracetamol` is a fixture-only name. The table is built from the real
  # deliveries, which write branded spellings instead.
  lmed <- lmed[produkt != "Paracetamol"]
  return(list(skeleton = skeleton, lmed = lmed))
}

test_that("a delivered product with no row stops the run and names it", {
  f <- fixture()
  f$lmed <- rbind(f$lmed, f$lmed[1][, produkt := "Nowhere In The Table"])
  expect_error(
    suppressWarnings(add_lmed_v20260902(f$skeleton, f$lmed, verbose = FALSE)),
    "Nowhere In The Table"
  )
})

test_that("the lookup is exact, so a name extending a key does not match", {
  # This is the behaviour change. The 2026-08-28 ladder matched a prefix, so
  # `Cyclogest Something` reached the `cyclogest` rung. Here it is unknown.
  f <- fixture()
  f$lmed <- rbind(f$lmed, f$lmed[1][, produkt := "Cyclogest Something Else"])
  expect_error(
    suppressWarnings(add_lmed_v20260902(f$skeleton, f$lmed, verbose = FALSE)),
    "have no row in the product table"
  )
})

test_that("the function removes nobody", {
  f <- fixture()
  before <- nrow(f$skeleton)
  ids_before <- data.table::uniqueN(f$skeleton$id)
  suppressWarnings(add_lmed_v20260902(f$skeleton, f$lmed, verbose = FALSE))
  expect_identical(nrow(f$skeleton), before)
  expect_identical(data.table::uniqueN(f$skeleton$id), ids_before)
})

test_that("the exclusion flag is person level and carries its reason", {
  f <- fixture()
  suppressWarnings(add_lmed_v20260902(f$skeleton, f$lmed, verbose = FALSE))
  expect_true(all(c("ri_mht_excluded_product", "ri_mht_excluded_reason") %in%
    names(f$skeleton)))
  # One value per person, constant across that person's weeks. That is what
  # the `ri_` prefix means.
  per_person <- f$skeleton[,
    .(n = data.table::uniqueN(ri_mht_excluded_product)),
    keyby = id
  ]
  expect_true(all(per_person$n == 1L))
  # A flagged person carries a reason; an unflagged one carries none.
  expect_true(all(!is.na(
    f$skeleton[ri_mht_excluded_product == TRUE]$ri_mht_excluded_reason
  )))
  expect_true(all(is.na(
    f$skeleton[ri_mht_excluded_product == FALSE]$ri_mht_excluded_reason
  )))
})

test_that("two reasons join in sorted order, whatever the row order", {
  tab <- mht:::lmed_read_product_table_v20260902()
  two <- tab[exclude_entire_person == TRUE][, .SD[1], keyby = exclusion_reason]
  skip_if(nrow(two) < 2L, "the table carries fewer than two exclusion reasons")
  two <- two[1:2]
  reads <- data.table::data.table(
    lopnr = c("p1", "p1"),
    produkt = c(two$produkt_clean[2], two$produkt_clean[1])
  )
  out <- mht:::lmed_person_exclusions_v20260902(reads)
  expect_identical(
    out$ri_mht_excluded_reason,
    paste(sort(two$exclusion_reason), collapse = "; ")
  )
})

test_that("a notmht product contributes no exposure", {
  tab <- mht:::lmed_read_product_table_v20260902()
  nm <- tab[classification == "notmht"][1]
  x <- data.table::data.table(produkt = nm$produkt_clean)
  mht:::lmed_categorize_product_names_v20260902(x)
  # notmht must reach the duration layer as NA, which is what makes the row
  # contribute nothing. A literal "notmht" category would materialise a column.
  expect_true(is.na(x$product_category))
})

# ---------------------------------------------------------------------------
# The positive path. Without these, replacing every category assignment with
# NA leaves the tests above green: none of them asserts that a known product
# reaches its expected category.
# ---------------------------------------------------------------------------

test_that("a known product reaches the category the table gives it", {
  tab <- mht:::lmed_read_product_table_v20260902()
  known <- tab[!classification %chin% c("notmht")][1:5]
  x <- data.table::data.table(produkt = known$produkt_clean)
  mht:::lmed_categorize_product_names_v20260902(x)
  expect_identical(x$product_category, known$classification)
  expect_false(any(is.na(x$product_category)))
})

test_that("classification reaches the skeleton as a lit category column", {
  f <- fixture()
  suppressWarnings(add_lmed_v20260902(f$skeleton, f$lmed, verbose = FALSE))
  # At least one materialised category column must be TRUE somewhere. A
  # no-op categoriser would leave every one of them FALSE.
  cats <- intersect(mht:::lmed_materialised_categories_v20260902(),
                    names(f$skeleton))
  expect_gt(length(cats), 0L)
  any_lit <- vapply(cats, function(k) any(f$skeleton[[k]]), logical(1))
  expect_true(any(any_lit))
})

test_that("someone is actually flagged, and it reaches the skeleton", {
  f <- fixture()
  suppressWarnings(add_lmed_v20260902(f$skeleton, f$lmed, verbose = FALSE))
  # A no-op exclusion leaves every flag FALSE, which every other assertion
  # in this file tolerates.
  expect_true(any(f$skeleton$ri_mht_excluded_product))
  expect_true(any(!is.na(f$skeleton$ri_mht_excluded_reason)))
})

test_that("the table rejects a classification that is neither legal nor notmht", {
  # `Al` for `A1` materialises no column, so the person reads as untreated.
  tab <- data.table::data.table(
    produkt_raw = "Fake", produkt_clean = "fake", classification = "Al",
    exclude_entire_person = "FALSE", exclusion_reason = NA_character_
  )
  expect_error(
    mht:::lmed_validate_product_table_v20260902(tab),
    "neither a materialised category nor notmht"
  )
})

test_that("the table rejects an exclusion cell that is not TRUE or FALSE", {
  tab <- data.table::data.table(
    produkt_raw = "Fake", produkt_clean = "fake", classification = "notmht",
    exclude_entire_person = "Yes", exclusion_reason = NA_character_
  )
  expect_error(
    mht:::lmed_validate_product_table_v20260902(tab),
    "Only TRUE and FALSE are read"
  )
})

test_that("the table rejects a key that is not its own raw name normalised", {
  tab <- data.table::data.table(
    produkt_raw = "Divigel", produkt_clean = "wrongkey", classification = "A1",
    exclude_entire_person = "FALSE", exclusion_reason = NA_character_
  )
  expect_error(
    mht:::lmed_validate_product_table_v20260902(tab),
    "not produkt_raw normalised"
  )
})

test_that("an unknown product errors even when only an outsider holds it", {
  f <- fixture()
  outsider <- data.table::copy(f$lmed[1])
  outsider[, `:=`(lopnr = "not_in_any_skeleton", produkt = "Outsider Only")]
  expect_error(
    suppressWarnings(add_lmed_v20260902(f$skeleton, rbind(f$lmed, outsider),
                                        verbose = FALSE)),
    "Outsider Only"
  )
})
