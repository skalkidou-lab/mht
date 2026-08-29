# Pins the 2026-08-28 product ladder: the normalisation, the prefix match, the
# rung ordering, and the twelve products the 2026-08-28 repair moved.
#
# The decision table lives outside this repository, so nothing here reads it.
# `dev/check-crosswalk.R` owns that side and fails when the ladder, the
# codebook and the decisions disagree.

classify_v20260828 <- function(produkt) {
  x <- data.table::data.table(produkt = produkt)
  mht:::lmed_categorize_product_names_v20260828(x)
  x$product_category
}

normalise_v20260828 <- function(produkt) {
  x <- data.table::data.table(produkt = produkt)
  mht:::lmed_categorize_product_names_v20260828(x)
  x$produkt_clean
}

# Every rung of the ladder, in source order, with its pattern and category.
# Read by parsing the function, so a line break inside the call cannot hide a
# rung.
rungs_v20260828 <- function() {
  fn <- utils::removeSource(mht:::lmed_categorize_product_names_v20260828)
  found <- NULL
  walk <- function(e) {
    if (!is.call(e)) {
      return(invisible(NULL))
    }
    if (identical(deparse(e[[1]]), "fcase") && is.null(found)) {
      found <<- e
    }
    for (i in seq_along(e)) {
      if (identical(e[i], list(quote(expr = )))) {
        next
      }
      walk(e[[i]])
    }
    invisible(NULL)
  }
  walk(body(fn))
  args <- as.list(found)[-1]
  idx <- seq(1L, length(args), by = 2L)
  data.frame(
    order = seq_along(idx),
    call = vapply(
      idx,
      function(k) paste(deparse(args[[k]]), collapse = ""),
      character(1)
    ),
    pattern = vapply(
      idx,
      function(k) {
        a <- args[[k]]
        if (is.call(a) && length(a) == 3L && is.character(a[[3]])) {
          a[[3]]
        } else {
          NA_character_
        }
      },
      character(1)
    ),
    category = vapply(
      idx,
      function(k) as.character(args[[k + 1L]]),
      character(1)
    ),
    stringsAsFactors = FALSE
  )
}

codebook_products <- function() {
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
  wb <- wb[!is.na(wb$Preparatnamn), c("Preparatnamn", "Subgrupp")]
  rownames(wb) <- NULL
  wb
}

# ------------------------------------------------------- the twelve repairs ----

test_that("the twelve repaired products reach their pinned categories", {
  # `before` is what the frozen v20250909 ladder returns, `after` is what the
  # repaired one returns. Both sides are asserted, so the pin reads as a
  # change and not as an assertion out of nowhere.
  pinned <- data.frame(
    produkt = c(
      "Primolut®-Nor",
      "Mini-Pe®",
      "Depo-Provera®",
      "Evorel® Micronor",
      "Cyclabil",
      "Prolutex",
      "Duphaston",
      "Lafamme",
      "Endovelle",
      "Testosteron depot",
      "Presomen 28 compositum",
      "Presomen"
    ),
    before = c(
      NA,
      NA,
      "C4",
      "A1",
      "B11",
      "C1",
      "C4",
      NA,
      NA,
      NA,
      "A6",
      "A6"
    ),
    after = c(
      "C4",
      "I1",
      "D1",
      "B1",
      "B12",
      "C2",
      "C5",
      "B5",
      # Endovelle: the codebook says C3 and Endovelle is not MHT. The
      # classification governs, so the ladder gives it no rung.
      NA,
      "H1",
      "B10",
      "A6"
    ),
    stringsAsFactors = FALSE
  )
  classify_frozen <- function(produkt) {
    x <- data.table::data.table(produkt = produkt)
    mht:::lmed_categorize_product_names_v20250909(x)
    x$product_category
  }
  expect_identical(classify_frozen(pinned$produkt), pinned$before)
  expect_identical(classify_v20260828(pinned$produkt), pinned$after)
})

# ---------------------------------------------------------- the normalisation ----

test_that("produkt_clean is the ASCII letters of produkt, lowercased", {
  expect_identical(
    normalise_v20260828(c(
      "Primolut®-Nor",
      "Estramon 100",
      "Presomen 28 compositum",
      "Estradiol SUN",
      "Extempore ATC-kod G03DA04 Progesteron"
    )),
    c(
      "primolutnor",
      "estramon",
      "presomencompositum",
      "estradiolsun",
      "extemporeatckodgdaprogesteron"
    )
  )
})

test_that("the ladder classifies a latin1 byte declared unknown", {
  # A direct data.table::fread() of the register declares every string
  # "unknown", and the register carries latin1 bytes. Strip the byte before
  # tolower() sees it, or R throws `invalid multibyte string`.
  #
  # absorb() turns that throw into a value, so the wrong order arrives at the
  # expectation and reports a FAILURE with both sides visible. An error would
  # stop the test before the expectation ran, and an error can come from
  # anywhere, including a broken fixture.
  absorb <- function(expr) {
    tryCatch(expr, error = function(e) paste0("ERROR: ", conditionMessage(e)))
  }
  bytes <- as.raw(c(0x4c, 0x69, 0x76, 0x69, 0x61, 0x6c, 0xae))
  unknown_latin1 <- rawToChar(bytes)
  expect_identical(Encoding(unknown_latin1), "unknown")
  expect_identical(absorb(classify_v20260828(unknown_latin1)), "F1")
  expect_identical(absorb(normalise_v20260828(unknown_latin1)), "livial")
})

test_that("tolower() before the strip is what fails on that byte", {
  # The witness for why the order is load-bearing. Asserted on the error's
  # existence, never on its text.
  skip_if_not(
    grepl("UTF-8", Sys.getlocale("LC_CTYPE"), fixed = TRUE),
    "byte validity is locale-dependent; pinned only in a UTF-8 locale"
  )
  unknown_latin1 <- rawToChar(as.raw(c(
    0x4c,
    0x69,
    0x76,
    0x69,
    0x61,
    0x6c,
    0xae
  )))
  expect_error(stringr::str_remove_all(tolower(unknown_latin1), "[^a-zA-Z]"))
  expect_error(suppressWarnings(gsub("[^a-zA-Z]", "", unknown_latin1)))
  expect_identical(
    tolower(stringr::str_remove_all(unknown_latin1, "[^a-zA-Z]")),
    "livial"
  )
})

# ------------------------------------------------------------- the prefix match ----

test_that("startsWith unshadows the three rungs a substring match swallowed", {
  # Under str_detect() `Provera` matched inside `Depo-Provera`, so D1 was
  # unreachable. C5 and B10 sat behind an identical earlier rung.
  expect_identical(
    classify_v20260828(c(
      "Depo-Provera",
      "Duphaston",
      "Presomen 28 compositum"
    )),
    c("D1", "C5", "B10")
  )
  # The mirror: a prefix match must NOT reach a rung it merely contains.
  expect_identical(
    classify_v20260828(c("Depo-Progevera", "Follistrel", "Premarin")),
    c(NA_character_, NA_character_, NA_character_)
  )
})

test_that("a NOT_MHT decision reaches no category that an approach rule reads", {
  # Testosterone and Endovelle are not MHT. Testosterone reaches H1, which no
  # post_grouping rule names, so it cannot move an approach variable. Endovelle
  # would reach C3, which the rules do name, so it gets no rung at all.
  path <- system.file(
    "2023-mht",
    "dataDictionary20260828.xlsx",
    package = "mht"
  )
  pg <- suppressMessages(readxl::read_excel(
    path,
    sheet = "post_grouping",
    col_types = "text"
  ))
  cols <- grep("^(includes|doesnotinclude)", names(pg), value = TRUE)
  read <- unlist(pg[, cols], use.names = FALSE)
  read <- unlist(strsplit(read[!is.na(read)], "[^A-Za-z0-9]+"))
  read <- sort(unique(read[nzchar(read)]))

  expect_false("H1" %in% read)
  expect_true("C3" %in% read)
  expect_identical(
    classify_v20260828(c(
      "Testosteron-Depot Jenapharm",
      "Testosteron Depot Panpharma",
      "Endovelle"
    )),
    c("H1", "H1", NA)
  )
})

test_that("a register spelling that extends a codebook name reaches its rung", {
  expect_identical(
    classify_v20260828(c(
      "Estradot 25 mikrogram/dygn",
      "Testoviron-Depot-250",
      "Divina® Plus",
      "Undestor Testocaps"
    )),
    c("A1", "H1", "B9", "H1")
  )
})

# ------------------------------------------------------------- the rung ordering ----

test_that("every rung is a startsWith on produkt_clean with a normalised pattern", {
  r <- rungs_v20260828()
  expect_gt(nrow(r), 100L)
  expect_false(any(is.na(r$pattern)))
  expect_true(all(startsWith(r$call, "startsWith(produkt_clean, ")))
  expect_true(all(grepl("^[a-z]+$", r$pattern)))
  expect_true(all(grepl("^[A-Z][0-9]+$", r$category)))
  expect_identical(anyDuplicated(r$pattern), 0L)
})

test_that("every prefix collision puts the longer rung first", {
  r <- rungs_v20260828()
  # Computed from the ladder itself, then compared with the declared list.
  # A rung that another rung shadows can never fire.
  found <- NULL
  for (i in seq_len(nrow(r))) {
    for (j in seq_len(nrow(r))) {
      if (i == j) {
        next
      }
      if (
        nchar(r$pattern[i]) < nchar(r$pattern[j]) &&
          startsWith(r$pattern[j], r$pattern[i])
      ) {
        found <- rbind(
          found,
          data.frame(
            short = r$pattern[i],
            long = r$pattern[j],
            shadowed = r$order[j] > r$order[i],
            stringsAsFactors = FALSE
          )
        )
      }
    }
  }
  expect_identical(
    sort(paste0(found$short, "|", found$long)),
    sort(c(
      "estalis|estalissekvens",
      "evorel|evorelmicronor",
      "femoston|femostonconti",
      "levosert|levosertone",
      "premelle|premellesekvens",
      "presomen|presomencompositum",
      "progesteron|progesteronmicapl",
      "undestor|undestortestocaps",
      "utrogest|utrogestan"
    ))
  )
  expect_identical(found$long[found$shadowed], character(0))
})

# ------------------------------------------------------------------ the codebook ----

test_that("every codebook product reaches its own subgroup, bar two declared rows", {
  wb <- codebook_products()
  got <- classify_v20260828(wb$Preparatnamn)
  disagree <- which(is.na(got) | got != wb$Subgrupp)
  # In codebook row order: Presomen is row 58, Endovelle is row 72.
  expect_identical(wb$Preparatnamn[disagree], c("Presomen", "Endovelle"))
  expect_identical(wb$Subgrupp[disagree], c("B10", "C3"))
  # The codebook names Presomen twice, and the register tells the two apart, so
  # the bare name reaches the A6 row. Endovelle reaches nothing: it is not MHT,
  # and that beats the codebook row.
  expect_identical(got[disagree], c("A6", NA))
  # dev/check-crosswalk.R holds both rows on EXEMPT_PRODUCTS, with a reason.
})

test_that("the person-week grid materialises every category the ladder returns, bar G1", {
  fn <- utils::removeSource(mht:::apply_lmed_categories_to_skeleton_v20260828)
  grid <- eval(body(fn)[[grep(
    "^product_categories <-",
    vapply(
      as.list(body(fn)),
      function(e) paste(deparse(e), collapse = " "),
      character(1)
    )
  )]][[3]])
  produced <- sort(unique(rungs_v20260828()$category))
  expect_identical(setdiff(produced, grid), "G1")
  expect_identical(setdiff(grid, produced), "D4")
})
