# Pins the exposure pipeline as it behaves TODAY, defects included.
#
# Every expectation in this file was OBTAINED BY RUNNING the exported entry
# points, never derived from the codebook, the ladder source or any
# re-implementation. Several pinned values are known defects. They are frozen
# on purpose: when a repair lands, the diff of this file is the record of what
# changed. Do not "correct" an expectation here.
#
# The file is organised by pipeline stage:
#   input filter -> normaliser -> ladder -> materialisation ->
#   rule evaluation -> approach resolution -> downstream projection/contract.

# ---------------------------------------------------------------- harness ----

# One helper parses the ladder out of the PARSED FUNCTION BODY (never the file
# on disk, which is absent from an installed package). It is used only to
# enumerate rungs; every conclusion drawn from it is cross-checked by running
# the real classifier on the same strings, in
# "every ladder rung classifies as its own category, except three".
ladder_rungs <- function(fn) {
  find_fcase <- function(e) {
    if (!is.call(e)) {
      return(NULL)
    }
    h <- e[[1]]
    if (identical(h, quote(fcase)) || identical(h, quote(data.table::fcase))) {
      return(e)
    }
    parts <- as.list(e)
    for (i in seq_along(parts)) {
      # `x[, j]` puts an empty symbol in the call; skip it without binding it
      if (identical(parts[i], list(quote(expr = )))) {
        next
      }
      hit <- find_fcase(parts[[i]])
      if (!is.null(hit)) {
        return(hit)
      }
    }
    NULL
  }
  fc <- find_fcase(body(utils::removeSource(fn)))
  stopifnot(!is.null(fc))
  a <- as.list(fc)[-1]
  stopifnot(length(a) %% 2 == 0)
  data.table::data.table(
    pattern = vapply(a[seq(1, length(a), 2)], function(z) as.character(z[[3]]), ""),
    declared = vapply(a[seq(2, length(a), 2)], as.character, "")
  )
}

ladders <- list(
  v20230509 = mht:::lmed_categorize_product_names_v20230509,
  v20250909 = mht:::lmed_categorize_product_names_v20250909
)

classify <- function(fn, produkt) {
  x <- data.table::data.table(produkt = produkt)
  fn(x)
  x$product_category
}

normalise <- function(fn, produkt) {
  x <- data.table::data.table(produkt = produkt)
  fn(x)
  x$produkt_clean
}

# The authoritative product universe: sheet `MHT_groups`, column `Preparatnamn`
# of the codebook both entry points read.
workbook_products <- function() {
  wb <- suppressMessages(suppressWarnings(readxl::read_excel(
    system.file("2023-mht", "dataDictionary20241105.xlsx", package = "mht"),
    sheet = "MHT_groups"
  )))
  data.table::setDT(wb)
  unique(stats::na.omit(wb$Preparatnamn))
}

weeks_between <- function(from, to) {
  unique(cstime::date_to_isoyearweek_c(
    seq(as.Date(from), as.Date(to), by = "week")
  ))
}

# Drive the real 2026 entry point and hand back the mutated skeleton.
run_2026 <- function(lmed, ids, weeks) {
  skeleton <- data.table::CJ(id = ids, isoyearweek = weeks, sorted = TRUE)
  suppressWarnings(add_lmed_v20250909(skeleton, lmed, verbose = FALSE))
  skeleton
}

# Drive the real 2023 entry point and hand back the mutated skeleton.
run_2023 <- function(lmed, ids, weeks) {
  skeleton <- data.table::CJ(id = ids, isoyearweek = weeks, sorted = TRUE)
  suppressMessages(suppressWarnings(add_lmed_v20230509(skeleton, lmed)))
  skeleton
}

# The only observable of the dispensed duration through an exported entry
# point: the ISO weeks in which the product's category column is TRUE.
#
# SENSITIVITY LIMIT, verified by mutation: because the observable is weekly,
# every duration pin below is blind to a change of fewer than seven days.
# Changing the IUD duration from 1680 to 1681, or the Jaydess duration from
# 1008 to 1009, leaves every test in this file green. Changing them to 1673
# and 1001 turns the relevant tests red. Nothing in the exported surface
# exposes the duration in days, so this limit cannot be closed from a test.
span_2026 <- function(produkt, fddd, category, weeks) {
  skeleton <- run_2026(
    data.table::data.table(
      lopnr = 1L,
      produkt = produkt,
      edatum = as.Date("2016-01-04"),
      fddd = fddd
    ),
    ids = 1L,
    weeks = weeks
  )
  w <- skeleton[get(category) == TRUE]$isoyearweek
  list(first = w[1], last = w[length(w)], n = length(w))
}

# ------------------------------------------------------------ input filter ----

test_that("a prescription whose id is absent from the skeleton reaches nothing", {
  weeks <- weeks_between("2015-01-05", "2016-12-26")
  skeleton <- run_2026(
    data.table::data.table(
      lopnr = c(1L, 99L),
      produkt = "Divigel",
      edatum = as.Date("2015-02-02"),
      fddd = 90
    ),
    ids = 1:2,
    weeks = weeks
  )
  expect_identical(sort(unique(skeleton[A1 == TRUE]$id)), 1L)
})

test_that("a negative or missing fddd drops the row and warns", {
  weeks <- weeks_between("2015-01-05", "2016-12-26")
  for (bad in list(-10, NA_real_)) {
    skeleton <- data.table::CJ(id = 1L, isoyearweek = weeks, sorted = TRUE)
    expect_warning(
      add_lmed_v20250909(
        skeleton,
        data.table::data.table(
          lopnr = 1L,
          produkt = "Divigel",
          edatum = as.Date("2015-02-02"),
          fddd = bad
        ),
        verbose = FALSE
      ),
      "dropped"
    )
    expect_false(any(skeleton$A1))
  }
})

test_that("the caller's lmed keeps its values but gains an index attribute", {
  # The documentation says `lmed` is NOT modified. Its VALUES are not, but the
  # subsetting in the entry point attaches a secondary-index cache to the
  # caller's own object by reference.
  weeks <- weeks_between("2015-01-05", "2016-12-26")
  lmed <- data.table::data.table(
    lopnr = c(1L, 99L),
    produkt = "Divigel",
    edatum = as.Date("2015-02-02"),
    fddd = 90
  )
  before <- data.table::copy(lmed)
  run_2026(lmed, ids = 1:2, weeks = weeks)

  expect_identical(as.data.frame(lmed), as.data.frame(before))
  expect_identical(names(lmed), names(before))
  expect_false("index" %in% names(attributes(before)))
  expect_true("index" %in% names(attributes(lmed)))
})

# -------------------------------------------------------------- normaliser ----

test_that("the normaliser strips spaces and keeps hyphens, in both ladders", {
  # `produkt_clean` is assigned twice; the hyphen-stripping assignment is
  # immediately overwritten by the space-stripping one, so only spaces survive.
  for (nm in names(ladders)) {
    expect_identical(
      normalise(ladders[[nm]], c("Femoston conti", "Femoston-conti", "Mini-Pe")),
      c("Femostonconti", "Femoston-conti", "Mini-Pe"),
      info = nm
    )
  }
})

# ---------------------------------------------------------- ladder / rungs ----

test_that("every ladder rung classifies as its own category, except three", {
  # This is the cross-check between the parsing helper and the executable
  # classifier: each rung's own pattern literal is fed through the real ladder.
  # A literal that comes back as a DIFFERENT category proves that rung is
  # shadowed by an earlier one, and therefore unreachable for every input.
  for (nm in names(ladders)) {
    r <- ladder_rungs(ladders[[nm]])
    expect_identical(nrow(r), 107L, info = nm)

    r[, observed := classify(ladders[[nm]], pattern)]
    expect_false(any(is.na(r$observed)), info = nm)

    shadowed <- r[observed != declared]
    expect_identical(shadowed$pattern, c("Presomen", "Duphaston", "DepoProvera"), info = nm)
    expect_identical(shadowed$declared, c("B10", "C5", "D1"), info = nm)
    expect_identical(shadowed$observed, c("A6", "C4", "C4"), info = nm)
  }
})

test_that("PINNED DEFECT: B10, C5 and D1 are unreachable for every input", {
  # The three shadowed rungs above, asserted directly against the classifier
  # so that the property does not depend on the parsing helper at all.
  for (nm in names(ladders)) {
    expect_identical(
      classify(ladders[[nm]], c("Presomen", "Duphaston", "DepoProvera")),
      c("A6", "C4", "C4"),
      info = nm
    )
    # a longer register-style spelling reaches the same shadowing rung
    expect_identical(
      classify(ladders[[nm]], c("Presomen 0,3 mg", "Duphaston 10 mg", "DepoProvera 150 mg")),
      c("A6", "C4", "C4"),
      info = nm
    )
  }
})

test_that("the workbook product universe yields 27 categories and 6 misses", {
  prods <- workbook_products()
  expect_identical(length(prods), 106L)
  for (nm in names(ladders)) {
    got <- classify(ladders[[nm]], prods)
    expect_identical(
      sort(unique(stats::na.omit(got))),
      c(
        "A1", "A2", "A3", "A4", "A5", "A6", "A7",
        "B1", "B11", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B9",
        "C1", "C3", "C4", "D2", "D3", "E1", "F1", "G1", "H1", "I2"
      ),
      info = nm
    )
    expect_identical(
      prods[is.na(got)],
      c(
        "Lafamme",
        "Extrempore progesteron",
        "Endovelle",
        "Primolut-Nor",
        "Testosteron depot",
        "Mini-Pe"
      ),
      info = nm
    )
  }
})

# ---------------------------------------------------------- materialisation ----

test_that("both entry points create the same 33 category columns, and G1 is not one", {
  weeks <- weeks_between("2015-01-05", "2016-03-28")
  lmed_2026 <- data.table::data.table(
    lopnr = 1L, produkt = "Divigel", edatum = as.Date("2015-02-02"), fddd = 90
  )
  lmed_2023 <- data.table::data.table(
    p1163_lopnr_personnr = 1L, produkt = "Divigel",
    edatum = as.Date("2015-02-02"), fddd = 90
  )
  expected <- c(
    "A1", "A2", "A3", "A4", "A5", "A6", "A7",
    "B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B9", "B10", "B11", "B12",
    "C1", "C2", "C3", "C4", "C5",
    "D1", "D2", "D3", "D4",
    "E1", "F1", "H1", "I1", "I2"
  )
  for (skeleton in list(
    run_2026(lmed_2026, ids = 1L, weeks = weeks),
    run_2023(lmed_2023, ids = 1L, weeks = weeks)
  )) {
    expect_identical(intersect(names(skeleton), c(expected, "G1")), expected)
    expect_false("G1" %in% names(skeleton))
  }
})

test_that("PINNED DEFECT: seven category columns can never become TRUE", {
  # Driven over the whole authoritative product universe, one product per
  # person, so nothing but the pipeline decides which columns light up.
  prods <- workbook_products()
  weeks <- weeks_between("2015-01-05", "2016-03-28")
  lmed <- data.table::data.table(
    lopnr = seq_along(prods),
    produkt = prods,
    edatum = as.Date("2015-02-02"),
    fddd = 365
  )
  skeleton <- run_2026(lmed, ids = seq_along(prods), weeks = weeks)
  cats <- setdiff(names(skeleton), c(
    "id", "isoyearweek", "approach1", "approach2", "approach3",
    grep("^rd_approach", names(skeleton), value = TRUE)
  ))
  lit <- vapply(cats, function(k) any(skeleton[[k]]), logical(1))
  expect_identical(
    names(lit)[!lit],
    c("B10", "B12", "C2", "C5", "D1", "D4", "I1")
  )
  expect_identical(
    names(lit)[lit],
    c(
      "A1", "A2", "A3", "A4", "A5", "A6", "A7",
      "B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B9", "B11",
      "C1", "C3", "C4", "D2", "D3", "E1", "F1", "H1", "I2"
    )
  )
})

test_that("PINNED DEFECT: a G1 product is classified and then reaches no column", {
  weeks <- weeks_between("2015-01-05", "2016-03-28")
  for (nm in names(ladders)) {
    expect_identical(classify(ladders[[nm]], "Duavive"), "G1", info = nm)
  }
  skeleton <- run_2026(
    data.table::data.table(
      lopnr = 1L, produkt = "Duavive", edatum = as.Date("2015-02-02"), fddd = 365
    ),
    ids = 1L,
    weeks = weeks
  )
  expect_false("G1" %in% names(skeleton))
  cats <- setdiff(names(skeleton), c(
    "id", "isoyearweek", "approach1", "approach2", "approach3",
    grep("^rd_approach", names(skeleton), value = TRUE)
  ))
  expect_false(any(vapply(cats, function(k) any(skeleton[[k]]), logical(1))))
})

# --------------------------------------------------------- rule evaluation ----

test_that("a product matching one workbook row applies that row once", {
  weeks <- weeks_between("2016-01-04", "2020-12-28")
  # `Utrogest 100mg` matches the `Utrogest` row only (8 mg/month, min 3 months):
  # floor(30/8) = 3 months -> 3 * 28 = 84 days.
  expect_identical(
    span_2026("Utrogest 100mg", 30, "C1", weeks),
    span_2026("Divigel", 84, "A1", weeks)
  )
  expect_identical(
    span_2026("Utrogest 100mg", 30, "C1", weeks),
    list(first = "2016-01", last = "2016-13", n = 13L)
  )
})

test_that("PINNED DEFECT: two matching workbook rows compound the minimum-dose rule", {
  # `Utrogest` is a prefix of `Utrogestan`, so `Utrogestan 100mg Capsules`
  # matches BOTH workbook rows. The loop writes `fddd :=` once per matching
  # row and each iteration reads the previous result, so from fddd = 30 the
  # duration compounds 30 -> 84 -> 196. Applied singly it would be 84 or 56.
  weeks <- weeks_between("2016-01-04", "2020-12-28")
  observed <- span_2026("Utrogestan 100mg Capsules", 30, "C1", weeks)
  expect_identical(observed, span_2026("Divigel", 196, "A1", weeks))
  expect_identical(observed, list(first = "2016-01", last = "2016-29", n = 29L))
  expect_false(identical(observed, span_2026("Divigel", 84, "A1", weeks)))
  expect_false(identical(observed, span_2026("Divigel", 56, "A1", weeks)))
})

# ---------------------------------------------------- duration precedence ----

test_that("the IUD rule overrides the dispensed fddd", {
  weeks <- weeks_between("2016-01-04", "2050-12-26")
  reference <- span_2026("Divigel", 1680, "A1", weeks)
  expect_identical(span_2026("Mirena", 1, "E1", weeks), reference)
  expect_identical(span_2026("Jadelle", 1, "D3", weeks), reference)
  expect_identical(reference, list(first = "2016-01", last = "2020-33", n = 241L))
})

test_that("the Jaydess rule overrides the IUD rule", {
  weeks <- weeks_between("2016-01-04", "2050-12-26")
  observed <- span_2026("Jaydess", 1, "E1", weeks)
  expect_identical(observed, span_2026("Divigel", 1008, "A1", weeks))
  expect_identical(observed, list(first = "2016-01", last = "2018-41", n = 145L))
  expect_false(identical(observed, span_2026("Divigel", 1680, "A1", weeks)))
})

test_that("PINNED: the minimum-dose loop overrides both the IUD and Jaydess rules", {
  # Precedence is decided by write order, and the minimum-dose loop writes
  # LAST, so it wins over both earlier rules rather than losing to them.
  #
  # `Mirena Primolut-Nor` uses only register spellings. The ladder never
  # strips the hyphen, so `produkt_clean` is `MirenaPrimolut-Nor` and the
  # `Mirena` rung fires: category E1, fddd := 1680. The minimum-dose loop then
  # matches the RAW product name against the `Primolut-Nor` workbook row
  # (4.66 mg/month, min 3 months): floor(1680/4.66) = 360 -> 360 * 28 = 10080.
  weeks <- weeks_between("2016-01-04", "2050-12-26")

  iud <- span_2026("Mirena Primolut-Nor", 1, "E1", weeks)
  expect_identical(iud, span_2026("Divigel", 10080, "A1", weeks))
  expect_identical(iud, list(first = "2016-01", last = "2043-33", n = 1441L))
  expect_false(identical(iud, span_2026("Divigel", 1680, "A1", weeks)))

  # `JaydessProvera` reaches the `Provera` rung (C4, above E1), takes the
  # Jaydess duration of 1008, and is then rewritten by the `Provera` row
  # (4 mg/month, min 3 months): floor(1008/4) = 252 -> 252 * 28 = 7056.
  jay <- span_2026("JaydessProvera", 1, "C4", weeks)
  expect_identical(jay, span_2026("Divigel", 7056, "A1", weeks))
  expect_identical(jay, list(first = "2016-01", last = "2035-18", n = 1009L))
  expect_false(identical(jay, span_2026("Divigel", 1008, "A1", weeks)))
})

test_that("the minimum-dose loop matches the raw name, not the normalised one", {
  # `Primolut-Nor` is NA to the ladder, because the hyphen survives
  # normalisation, yet the same spelling still fires its workbook row above.
  # The two stages therefore disagree about what a product name is.
  for (nm in names(ladders)) {
    expect_identical(classify(ladders[[nm]], "Mirena Primolut-Nor"), "E1", info = nm)
    expect_identical(
      normalise(ladders[[nm]], "Mirena Primolut-Nor"),
      "MirenaPrimolut-Nor",
      info = nm
    )
  }
})

# ------------------------------------------------------------ encoding trap ----

test_that("the ladder classifies a registered sign under every declared encoding", {
  # The raw batch declares Encoding() == "latin1"; a direct fread() gives
  # "unknown". The pinned case is the LAST one: latin1 bytes declared
  # "unknown", which is what production sees and what a test written against
  # "latin1" alone never exercises.
  utf8 <- "Livial®"
  latin1 <- iconv(utf8, "UTF-8", "latin1")
  Encoding(latin1) <- "latin1"
  unknown_utf8 <- utf8
  Encoding(unknown_utf8) <- "unknown"
  unknown_latin1 <- latin1
  Encoding(unknown_latin1) <- "unknown"

  expect_identical(Encoding(unknown_latin1), "unknown")
  expect_identical(charToRaw(unknown_latin1), charToRaw(latin1))

  for (nm in names(ladders)) {
    for (x in list(utf8, latin1, unknown_utf8, unknown_latin1)) {
      expect_identical(
        suppressWarnings(classify(ladders[[nm]], x)),
        "F1",
        info = paste(nm, Encoding(x))
      )
    }
  }
})

test_that("base gsub fails on the unknown-declared bytes the ladder survives", {
  # Witness for WHY the test above must use "unknown" and not "latin1": a
  # normaliser written with base gsub passes the latin1 case and errors on the
  # unknown one. Asserted on the error's EXISTENCE, never on its text.
  skip_if_not(
    grepl("UTF-8", Sys.getlocale("LC_CTYPE"), fixed = TRUE),
    "byte validity is locale-dependent; pinned only in a UTF-8 locale"
  )
  latin1 <- iconv("Livial®", "UTF-8", "latin1")
  Encoding(latin1) <- "latin1"
  unknown_latin1 <- latin1
  Encoding(unknown_latin1) <- "unknown"

  expect_identical(gsub(" ", "", latin1), latin1)
  expect_error(suppressWarnings(gsub(" ", "", unknown_latin1)))
  expect_identical(suppressWarnings(classify(ladders$v20250909, unknown_latin1)), "F1")
})

# ------------------------------------------------------ approach resolution ----

test_that("each category alone resolves to the pinned approach values", {
  prods <- workbook_products()
  weeks <- weeks_between("2015-01-05", "2016-03-28")
  cat_of <- classify(ladders$v20250909, prods)
  skeleton <- run_2026(
    data.table::data.table(
      lopnr = seq_along(prods),
      produkt = prods,
      edatum = as.Date("2015-02-02"),
      fddd = 365
    ),
    ids = seq_along(prods),
    weeks = weeks
  )
  key <- data.table::data.table(id = seq_along(prods), cat = cat_of)
  got <- skeleton[key, on = "id"][
    !is.na(cat),
    .(
      a1 = paste(sort(unique(approach1)), collapse = "|"),
      a3 = paste(sort(unique(approach3)), collapse = "|")
    ),
    keyby = .(cat)
  ]

  systemic <- c(
    "A1", "A2", "A6", "A7",
    "B1", "B11", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B9"
  )
  expect_identical(got[a1 != "local_or_none_mht"]$cat, systemic)
  expect_true(all(got[a1 != "local_or_none_mht"]$a1 == "local_or_none_mht|systemic_mht"))

  # every remaining classified category resolves to local_or_none_mht alone,
  # on all three approaches
  expect_identical(
    got[a1 == "local_or_none_mht"]$cat,
    c("A3", "A4", "A5", "C1", "C3", "C4", "D2", "D3", "E1", "F1", "G1", "H1", "I2")
  )
  expect_true(all(got[a1 == "local_or_none_mht"]$a3 == "local_or_none_mht"))
})

test_that("PINNED DEFECT: F1, G1 and H1 are read by no approach rule", {
  # Adding one of these to an A1 person changes nothing anywhere, while every
  # progestogen category in the next test does change approach3. That is the
  # executable proof that these three are classified and then read by nothing.
  weeks <- weeks_between("2015-01-05", "2016-12-26")
  rep_product <- c(A1 = "Divigel", F1 = "Livial", G1 = "Duavive", H1 = "Nebido")
  expect_identical(
    classify(ladders$v20250909, unname(rep_product)),
    unname(names(rep_product))
  )

  alone <- run_2026(
    data.table::data.table(
      lopnr = 1L, produkt = "Divigel", edatum = as.Date("2015-02-02"), fddd = 365
    ),
    ids = 1L,
    weeks = weeks
  )
  approaches <- c("approach1", "approach2", "approach3")
  for (k in c("F1", "G1", "H1")) {
    with_k <- run_2026(
      data.table::data.table(
        lopnr = 1L,
        produkt = c("Divigel", unname(rep_product[k])),
        edatum = as.Date("2015-02-02"),
        fddd = 365
      ),
      ids = 1L,
      weeks = weeks
    )
    expect_identical(with_k[, ..approaches], alone[, ..approaches], info = k)
  }
})

test_that("the progestogen categories DO change approach3 when combined with A1", {
  weeks <- weeks_between("2015-01-05", "2016-12-26")
  rep_product <- c(
    C1 = "Crinone", C3 = "Cerazette", C4 = "Provera",
    D2 = "Nexplanon", D3 = "Jadelle", E1 = "Mirena", I2 = "Exlutena"
  )
  expect_identical(
    classify(ladders$v20250909, unname(rep_product)),
    unname(names(rep_product))
  )
  expected <- c(
    C1 = "estrogen_progesterone_bioidentical",
    C3 = "estrogen_progesterone_synthetic",
    C4 = "estrogen_progesterone_synthetic",
    D2 = "estrogen_progesterone_synthetic",
    D3 = "estrogen_progesterone_synthetic",
    E1 = "estrogen_progesterone_synthetic",
    I2 = "estrogen_progesterone_synthetic"
  )
  for (k in names(rep_product)) {
    skeleton <- run_2026(
      data.table::data.table(
        lopnr = 1L,
        produkt = c("Divigel", unname(rep_product[k])),
        edatum = as.Date("2015-02-02"),
        fddd = 365
      ),
      ids = 1L,
      weeks = weeks
    )
    expect_identical(
      sort(unique(skeleton$approach3)),
      sort(c(unname(expected[k]), "local_or_none_mht")),
      info = k
    )
  }
})

# ----------------------------------------------- downstream / the contract ----

test_that("PINNED: an entry point returns a logical flag, not the skeleton", {
  # Both entry points end with data.table::shouldPrint(skeleton), so the
  # returned value is a length-1 logical. Never assign the result.
  weeks <- weeks_between("2015-01-05", "2016-03-28")
  skeleton <- data.table::CJ(id = 1L, isoyearweek = weeks, sorted = TRUE)
  out <- add_lmed_v20250909(
    skeleton,
    data.table::data.table(
      lopnr = 1L, produkt = "Divigel", edatum = as.Date("2015-02-02"), fddd = 90
    ),
    verbose = FALSE
  )
  expect_type(out, "logical")
  expect_length(out, 1L)
  expect_false(data.table::is.data.table(out))

  skeleton23 <- data.table::CJ(id = 1L, isoyearweek = weeks, sorted = TRUE)
  out23 <- suppressMessages(add_lmed_v20230509(
    skeleton23,
    data.table::data.table(
      p1163_lopnr_personnr = 1L, produkt = "Divigel",
      edatum = as.Date("2015-02-02"), fddd = 90
    )
  ))
  expect_type(out23, "logical")
  expect_length(out23, 1L)
})

test_that("PINNED: the entry point reorders the caller's skeleton by reference", {
  # `setorder()` runs on the caller's own object, so "only adds columns" is
  # false: the row order changes too.
  weeks <- weeks_between("2015-01-05", "2016-03-28")
  skeleton <- data.table::CJ(id = 1:3, isoyearweek = weeks, sorted = TRUE)
  data.table::setorder(skeleton, -id, -isoyearweek)
  before <- data.table::copy(skeleton[, .(id, isoyearweek)])

  suppressWarnings(add_lmed_v20250909(
    skeleton,
    data.table::data.table(
      lopnr = 1L, produkt = "Divigel", edatum = as.Date("2015-02-02"), fddd = 90
    ),
    verbose = FALSE
  ))
  after <- skeleton[, .(id, isoyearweek)]

  expect_false(identical(before, after))
  expect_identical(after, before[order(id, isoyearweek)])
  expect_identical(after$id[1], 1L)
})

test_that("PINNED: the entry point deletes caller columns that collide with internals", {
  # The approach loop creates and then deletes its working columns. A caller
  # column of the same name is destroyed, so "only adds columns" is false in a
  # second way.
  weeks <- weeks_between("2015-01-05", "2016-03-28")
  skeleton <- data.table::CJ(id = 1L, isoyearweek = weeks, sorted = TRUE)
  skeleton[, systemic_mht := TRUE]
  skeleton[, local_or_none_mht := TRUE]
  skeleton[, row_min := 42]
  skeleton[, run_systemic_mht := 7]
  skeleton[, keepme := 1L]

  suppressWarnings(add_lmed_v20250909(
    skeleton,
    data.table::data.table(
      lopnr = 1L, produkt = "Divigel", edatum = as.Date("2015-02-02"), fddd = 90
    ),
    verbose = FALSE
  ))

  for (gone in c("systemic_mht", "local_or_none_mht", "row_min", "run_systemic_mht")) {
    expect_false(gone %in% names(skeleton), info = gone)
  }
  expect_true("keepme" %in% names(skeleton))
  expect_identical(unique(skeleton$keepme), 1L)
})

test_that("PINNED: the 2023 entry point leaves a key on the caller's skeleton", {
  weeks <- weeks_between("2015-01-05", "2016-03-28")
  skeleton <- data.table::data.table(
    id = 1L,
    isoyearweek = weeks
  )
  expect_null(data.table::key(skeleton))
  suppressMessages(suppressWarnings(add_lmed_v20230509(
    skeleton,
    data.table::data.table(
      p1163_lopnr_personnr = 1L, produkt = "Divigel",
      edatum = as.Date("2015-02-02"), fddd = 90
    )
  )))
  expect_identical(data.table::key(skeleton), c("id", "isoyearweek"))
})
