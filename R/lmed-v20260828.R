#' Reduce a product name to its ASCII letters, lowercased
#'
#' The one normalisation the 2026-08-28 layer uses. The product ladder and the
#' codebook rules both call it, so a rung and a rule cannot normalise
#' differently.
#'
#' @details
#' The order is load-bearing. `tolower()` on a latin1 byte declared `"unknown"`
#' throws `invalid multibyte string`, and a direct `data.table::fread()` of the
#' register declares every string `"unknown"`. Strip first and the byte is gone
#' before `tolower()` sees it. The strip uses `stringr`, because base `gsub()`
#' errors on the same input.
#'
#' @param x A character vector of product names.
#' @return A character vector of the same length.
#' @noRd
lmed_normalize_product_name_v20260828 <- function(x) {
  return(tolower(stringr::str_remove_all(x, "[^a-zA-Z]")))
}

#' Categorise 2026-08-28 LMED product names into MHT groups
#'
#' Adds `produkt_clean` and `product_category` to `x` by reference. One rung
#' per codebook product name, matched as a prefix of the normalised name.
#'
#' @details
#' `produkt_clean` is `produkt` reduced to its ASCII letters, then lowercased.
#' The order is load-bearing. `tolower()` on a latin1 byte declared `"unknown"`
#' throws `invalid multibyte string`, and a direct `data.table::fread()` of the
#' register declares every string `"unknown"`. Strip first and the byte is gone
#' before `tolower()` sees it. The strip uses `stringr`, because base `gsub()`
#' errors on the same input.
#'
#' Normalisation removes the registered-trademark sign, the hyphen, the space
#' and every digit. `Primolut(R)-Nor` and `Estramon 100` become `primolutnor`
#' and `estramon`.
#'
#' A rung fires on `startsWith()`, so a register spelling that extends a
#' codebook name reaches the same rung. `Estradot 25 mikrogram/dygn` reaches
#' `estradot`. Nine rungs are prefixes of another rung. The longer rung of each
#' pair sits first, or the shorter one would shadow it.
#' `dev/check-crosswalk.R` enumerates all nine and asserts that ordering.
#'
#' @param x A `data.table` of dispensed prescriptions with a `produkt` column.
#' @return `x`, modified by reference.
#' @noRd
lmed_categorize_product_names_v20260828 <- function(x) {
  # Declare variables for data.table non-standard evaluation
  produkt_clean <- product_category <- produkt <- NULL

  x[, produkt_clean := lmed_normalize_product_name_v20260828(produkt)]
  x[,
    product_category := fcase(
      # -- cross-category prefix pairs, hoisted -----------------------------
      # Each of these three extends a shorter rung that carries a DIFFERENT
      # category, and that shorter rung sits in a later block. Leave one here
      # and the shorter rung answers for both products.
      startsWith(produkt_clean, "evorelmicronor")                , "B1"  , # before evorel, A1
      startsWith(produkt_clean, "presomencompositum")            , "B10" , # before presomen, A6
      startsWith(produkt_clean, "femostonconti")                 , "B4"  , # before femoston, B11

      # -- A1 oestrogen, transdermal ----------------------------------------
      startsWith(produkt_clean, "divigel")                       , "A1"  ,
      startsWith(produkt_clean, "estradot")                      , "A1"  ,
      startsWith(produkt_clean, "estrogel")                      , "A1"  ,
      startsWith(produkt_clean, "lenzetto")                      , "A1"  ,
      startsWith(produkt_clean, "dermestril")                    , "A1"  ,
      startsWith(produkt_clean, "evorel")                        , "A1"  ,
      startsWith(produkt_clean, "oesclim")                       , "A1"  ,
      startsWith(produkt_clean, "climara")                       , "A1"  ,
      startsWith(produkt_clean, "evopad")                        , "A1"  ,
      startsWith(produkt_clean, "femseven")                      , "A1"  ,
      # one rung for Estramon 100, Estramon 75 and Estramon 25: the digits go
      startsWith(produkt_clean, "estramon")                      , "A1"  ,

      # -- A2 oestrogen, peroral --------------------------------------------
      startsWith(produkt_clean, "progynon")                      , "A2"  ,
      startsWith(produkt_clean, "femanest")                      , "A2"  ,

      # -- A3 oestradiol, vaginal, local effect -----------------------------
      startsWith(produkt_clean, "oestring")                      , "A3"  ,
      startsWith(produkt_clean, "vagidonna")                     , "A3"  ,
      startsWith(produkt_clean, "vagifem")                       , "A3"  ,
      startsWith(produkt_clean, "vagirux")                       , "A3"  ,
      startsWith(produkt_clean, "estradiolsun")                  , "A3"  ,
      startsWith(produkt_clean, "menovag")                       , "A3"  ,

      # -- A4 oestriol, vaginal, local effect -------------------------------
      startsWith(produkt_clean, "blissel")                       , "A4"  ,
      startsWith(produkt_clean, "estrokad")                      , "A4"  ,
      startsWith(produkt_clean, "ovesterin")                     , "A4"  ,
      startsWith(produkt_clean, "gelistrol")                     , "A4"  ,
      startsWith(produkt_clean, "intrarosa")                     , "A4"  ,
      startsWith(produkt_clean, "gynoflor")                      , "A4"  ,

      # -- A5 oestriol, peroral ---------------------------------------------
      startsWith(produkt_clean, "oestriolaspen")                 , "A5"  ,

      # -- A6 conjugated oestrogens, peroral --------------------------------
      startsWith(produkt_clean, "premarina")                     , "A6"  ,
      startsWith(produkt_clean, "presomen")                      , "A6"  ,
      startsWith(produkt_clean, "climopaxmono")                  , "A6"  ,

      # -- A7 oestrogen, injection ------------------------------------------
      startsWith(produkt_clean, "delestrogen")                   , "A7"  ,
      startsWith(produkt_clean, "neofollin")                     , "A7"  ,

      # -- B1 oestrogen with progestogen, transdermal -----------------------
      startsWith(produkt_clean, "estalissekvens")                , "B1"  ,
      startsWith(produkt_clean, "estalis")                       , "B1"  ,
      startsWith(produkt_clean, "combipatch")                    , "B1"  ,

      # -- B2 oestradiol with norethisterone, peroral -----------------------
      startsWith(produkt_clean, "activelle")                     , "B2"  ,
      startsWith(produkt_clean, "cliovelle")                     , "B2"  ,
      startsWith(produkt_clean, "eviana")                        , "B2"  ,
      startsWith(produkt_clean, "femanor")                       , "B2"  ,
      startsWith(produkt_clean, "noresmea")                      , "B2"  ,
      startsWith(produkt_clean, "kliogest")                      , "B2"  ,

      # -- B3 oestradiol with medroxyprogesterone, peroral ------------------
      startsWith(produkt_clean, "indivina")                      , "B3"  ,
      startsWith(produkt_clean, "duova")                         , "B3"  ,
      startsWith(produkt_clean, "premellesekvens")               , "B3"  ,
      startsWith(produkt_clean, "premelle")                      , "B3"  ,

      # -- B5 oestradiol with dienogest, peroral ----------------------------
      startsWith(produkt_clean, "lafamme")                       , "B5"  ,
      startsWith(produkt_clean, "climodien")                     , "B5"  ,

      # -- B6 oestradiol with drospirenone, peroral -------------------------
      startsWith(produkt_clean, "angemin")                       , "B6"  ,

      # -- B7 sequential, transdermal ---------------------------------------
      startsWith(produkt_clean, "sequidot")                      , "B7"  ,

      # -- B8 sequential, peroral -------------------------------------------
      startsWith(produkt_clean, "femasekvens")                   , "B8"  ,
      startsWith(produkt_clean, "trisekvens")                    , "B8"  ,
      startsWith(produkt_clean, "novofem")                       , "B8"  ,

      # -- B9 sequential with medroxyprogesterone, peroral ------------------
      startsWith(produkt_clean, "divinaplus")                    , "B9"  ,
      startsWith(produkt_clean, "trivina")                       , "B9"  ,

      # -- B11 sequential with dydrogesterone, peroral ----------------------
      startsWith(produkt_clean, "femoston")                      , "B11" ,

      # -- B12 sequential with cyproterone, peroral -------------------------
      startsWith(produkt_clean, "cyclabil")                      , "B12" ,

      # -- C1 progesterone, vaginal -----------------------------------------
      startsWith(produkt_clean, "crinone")                       , "C1"  ,
      startsWith(produkt_clean, "cyclogest")                     , "C1"  ,
      startsWith(produkt_clean, "lugesteron")                    , "C1"  ,
      startsWith(produkt_clean, "lutinus")                       , "C1"  ,
      startsWith(produkt_clean, "utrogestan")                    , "C1"  ,
      startsWith(produkt_clean, "utrogest")                      , "C1"  ,
      startsWith(produkt_clean, "extemporeprogesteron")          , "C1"  ,
      # the register writes the extemporaneous product with its ATC code in
      # the name, and normalisation drops the digits of that code
      startsWith(produkt_clean, "extemporeatckodgdaprogesteron") , "C1"  ,
      startsWith(produkt_clean, "progesteronmicapl")             , "C1"  ,
      startsWith(produkt_clean, "progesteron")                   , "C1"  ,

      # -- C2 progesterone, intramuscular -----------------------------------
      startsWith(produkt_clean, "prolutex")                      , "C2"  ,

      # -- C3 progestogen, peroral ------------------------------------------
      startsWith(produkt_clean, "visanne")                       , "C3"  ,
      startsWith(produkt_clean, "dienosis")                      , "C3"  ,
      # Endovelle has no rung. The codebook gives it C3 and the 2026-08-26
      # clinician decision is NOT MHT. The decision governs, so the product
      # reaches no category. dev/check-crosswalk.R holds the codebook row on
      # EXEMPT_PRODUCTS.
      startsWith(produkt_clean, "desogestrel")                   , "C3"  ,
      startsWith(produkt_clean, "cerazette")                     , "C3"  ,
      startsWith(produkt_clean, "azalia")                        , "C3"  ,
      startsWith(produkt_clean, "gestrina")                      , "C3"  ,
      startsWith(produkt_clean, "velavel")                       , "C3"  ,
      startsWith(produkt_clean, "vinelle")                       , "C3"  ,
      startsWith(produkt_clean, "zarelle")                       , "C3"  ,
      startsWith(produkt_clean, "slinda")                        , "C3"  ,

      # -- C4 progestogen, peroral ------------------------------------------
      startsWith(produkt_clean, "primolutnor")                   , "C4"  ,
      startsWith(produkt_clean, "provera")                       , "C4"  ,
      startsWith(produkt_clean, "orgametril")                    , "C4"  ,
      startsWith(produkt_clean, "gestapuran")                    , "C4"  ,

      # -- C5 dydrogesterone, peroral ---------------------------------------
      startsWith(produkt_clean, "duphaston")                     , "C5"  ,

      # -- D1 contraceptive progestogen, intramuscular ----------------------
      startsWith(produkt_clean, "depoprovera")                   , "D1"  ,

      # -- D2 contraceptive progestogen, subcutaneous -----------------------
      startsWith(produkt_clean, "nexplanon")                     , "D2"  ,
      startsWith(produkt_clean, "implanon")                      , "D2"  ,
      # The codebook spells this product with one l. The register spells it
      # Follistrel, with two, and that reaches no rung. See the FINDINGS block
      # of dev/check-crosswalk.R.
      startsWith(produkt_clean, "folistrel")                     , "D2"  ,

      # -- D3 contraceptive progestogen, subcutaneous implant ---------------
      startsWith(produkt_clean, "jadelle")                       , "D3"  ,

      # -- E1 levonorgestrel intrauterine device ----------------------------
      startsWith(produkt_clean, "jaydess")                       , "E1"  ,
      startsWith(produkt_clean, "kyleena")                       , "E1"  ,
      startsWith(produkt_clean, "levosertone")                   , "E1"  ,
      startsWith(produkt_clean, "levosert")                      , "E1"  ,
      startsWith(produkt_clean, "mirena")                        , "E1"  ,

      # -- F1 tibolone, peroral ---------------------------------------------
      startsWith(produkt_clean, "livial")                        , "F1"  ,
      startsWith(produkt_clean, "tibelia")                       , "F1"  ,
      startsWith(produkt_clean, "tibocina")                      , "F1"  ,
      startsWith(produkt_clean, "tibolonaristo")                 , "F1"  ,
      startsWith(produkt_clean, "tibolonmylan")                  , "F1"  ,
      startsWith(produkt_clean, "tibolonorifarm")                , "F1"  ,
      startsWith(produkt_clean, "boltin")                        , "F1"  ,

      # -- G1 conjugated oestrogens with bazedoxifene, peroral --------------
      startsWith(produkt_clean, "duavive")                       , "G1"  ,

      # -- H1 androgen ------------------------------------------------------
      startsWith(produkt_clean, "nebido")                        , "H1"  ,
      startsWith(produkt_clean, "testogel")                      , "H1"  ,
      startsWith(produkt_clean, "undestortestocaps")             , "H1"  ,
      startsWith(produkt_clean, "undestor")                      , "H1"  ,
      startsWith(produkt_clean, "testosterondepot")              , "H1"  ,
      startsWith(produkt_clean, "intrinsa")                      , "H1"  ,
      startsWith(produkt_clean, "testavan")                      , "H1"  ,
      startsWith(produkt_clean, "testim")                        , "H1"  ,
      startsWith(produkt_clean, "testovirondepot")               , "H1"  ,
      startsWith(produkt_clean, "tostran")                       , "H1"  ,
      startsWith(produkt_clean, "tostrex")                       , "H1"  ,

      # -- I1 contraceptive progestogen, peroral ----------------------------
      startsWith(produkt_clean, "minipe")                        , "I1"  ,

      # -- I2 lynestrenol, peroral ------------------------------------------
      startsWith(produkt_clean, "exlutena")                      , "I2"
    )
  ]
  return(invisible(x))
}

#' Flag each person-week with the 2026-08-28 MHT product categories in force
#'
#' Adds one logical column per product category to `skeleton` by reference.
#' Uses a `data.table::foverlaps()` interval join, and reads the identifier
#' from `lopnr`.
#'
#' @details
#' `product_categories` is what the person-week grid materialises. `G1` is
#' absent from it on purpose. `G1` is Duavive alone, the coauthors decided that
#' Duavive counts as no MHT, and a category the grid never materialises reaches
#' no approach rule. Classify, then discard. Do not add `G1` here.
#'
#' Prescription rows whose interval is missing or inverted are dropped with a
#' warning.
#'
#' The function asserts that each person holds each ISO week once, with no gap.
#' Gap bridging and the three-year minimum-duration rule both count rows, and a
#' row equals one elapsed week only under that assertion.
#'
#' An episode already running at a person's first retained week is CLIPPED by
#' the interval join. Duration means weeks observed under follow-up, so the
#' layer counts observed weeks only and never reaches behind the first retained
#' week. The 2026 delivery is windowed to 2006 through 2024, so a treatment that
#' started earlier reads as starting in the first observed week.
#'
#' @param skeleton A person-week `data.table` with `id` and `isoyearweek`.
#' @param LMED A `data.table` carrying `lopnr`, `start_isoyearweek`,
#'   `stop_isoyearweek` and `product_category`.
#' @param verbose Logical. If `TRUE`, report each category with `message()`.
#' @return `skeleton`, modified by reference.
#' @noRd
apply_lmed_categories_to_skeleton_v20260828 <- function(
  skeleton,
  LMED,
  verbose = TRUE
) {
  # Declare variables for data.table non-standard evaluation
  . <- NULL
  start_isoyearweek <- stop_isoyearweek <- isoyearweek <- product_category <- id <- NULL
  lopnr <- iyw_int <- iyw_int_end <- start_int <- stop_int <- NULL

  # Every consumer downstream counts ROWS. This is what makes a row one
  # observed ISO week.
  assert_person_weeks_v20260828(skeleton)

  # G1 is deliberately absent. See the details section above.
  product_categories <- c(
    "A1",
    "A2",
    "A3",
    "A4",
    "A5",
    "A6",
    "A7",
    "B1",
    "B2",
    "B3",
    "B4",
    "B5",
    "B6",
    "B7",
    "B8",
    "B9",
    "B10",
    "B11",
    "B12",
    "C1",
    "C2",
    "C3",
    "C4",
    "C5",
    "D1",
    "D2",
    "D3",
    "D4",
    "E1",
    "F1",
    "H1",
    "I1",
    "I2"
  )

  # Initialize all product columns to FALSE
  for (product in product_categories) {
    skeleton[, (product) := FALSE]
  }

  if (nrow(LMED) > 0) {
    # Prepare LMED intervals with id column matching skeleton
    lmed_intervals <- LMED[, .(
      id = lopnr,
      start_isoyearweek,
      stop_isoyearweek,
      product_category
    )]

    # Prepare skeleton point-intervals for foverlaps
    # foverlaps requires numeric interval columns, so map isoyearweek to integer rank
    all_weeks <- sort(unique(c(
      skeleton$isoyearweek,
      lmed_intervals$start_isoyearweek,
      lmed_intervals$stop_isoyearweek
    )))
    week_to_int <- stats::setNames(seq_along(all_weeks), all_weeks)

    skel_pts <- unique(skeleton[, .(id, isoyearweek)])
    skel_pts[, iyw_int := week_to_int[isoyearweek]]
    skel_pts[, iyw_int_end := iyw_int]
    data.table::setkey(skel_pts, id, iyw_int, iyw_int_end)

    # foverlaps: find all skeleton points within LMED intervals
    lmed_intervals[, start_int := week_to_int[start_isoyearweek]]
    lmed_intervals[, stop_int := week_to_int[stop_isoyearweek]]
    # Remove rows with NA or inverted intervals (NA fddd, negative fddd)
    n_before <- nrow(lmed_intervals)
    lmed_intervals <- lmed_intervals[
      !is.na(start_int) & !is.na(stop_int) & start_int <= stop_int
    ]
    n_dropped <- n_before - nrow(lmed_intervals)
    if (n_dropped > 0) {
      warning(
        n_dropped,
        " LMED rows dropped due to NA or negative fddd ",
        "(start_isoyearweek > stop_isoyearweek or missing dates)",
        call. = FALSE
      )
    }
    data.table::setkey(lmed_intervals, id, start_int, stop_int)
    matches <- data.table::foverlaps(
      lmed_intervals,
      skel_pts,
      type = "any",
      nomatch = NULL
    )
    matches <- unique(matches[, .(id, isoyearweek, product_category)])

    # Bulk update per product
    for (product in product_categories) {
      if (verbose) {
        message(Sys.time(), " ", product)
      }
      skeleton[
        matches[product_category == product],
        on = .(id, isoyearweek),
        (product) := TRUE
      ]
    }
  }

  return(setorder(skeleton, id, isoyearweek))
}

# ============================================================ duration layer ====
#
# One prescription, one exposure interval. The layer reads every product rule
# from the 2026-08-28 codebook, applies exactly one of them, and converts the
# result to a pair of ISO weeks. A prescription with no positive duration
# contributes nothing.

#' The register spellings that take another product's codebook rules
#'
#' Returns a named character vector. The name is a normalised register
#' spelling. The value is the normalised codebook name whose rules it takes.
#'
#' @details
#' `Utrogest 100 mg` and `Utrogestan 100 mg` are the same product. The clinician
#' stated that on 2026-08-26. The 2026-08-28 codebook carries `Utrogestan` and
#' carries no `Utrogest` row. The register spelling `Utrogest` therefore reaches
#' no duration rule, and keeps its raw `fddd`. Measured at `fddd = 30`, that is
#' 30 days. `Utrogestan 100 mg` gives 28.
#'
#' The alias lives here and not in the codebook. `dataDictionary20260828.xlsx`
#' is about to freeze. A second `Utrogest` row also gives one prescription two
#' rules of equal specificity, which is an error.
#'
#' `lmed_read_product_rules_v20260828()` checks both directions of every entry.
#' An alias that names a codebook product is an error. So is an alias whose
#' target names no codebook rule.
#'
#' @return A named character vector.
#' @noRd
lmed_rule_aliases_v20260828 <- function() {
  return(c(utrogest = "utrogestan"))
}

#' Rewrite a normalised product name onto the name whose rules it takes
#'
#' Applies every alias of `lmed_rule_aliases_v20260828()`, once each. A name
#' that matches no alias comes back unchanged.
#'
#' @details
#' An alias fires on a prefix, like a rule itself. It does not fire where the
#' target name already prefixes the input, so `utrogestanmgcapsules` stays as it
#' is.
#'
#' This is the rule-matching name alone. `produkt_clean` and the product ladder
#' never see it, so no alias can change a category.
#'
#' @param produkt_clean A character vector of normalised product names.
#' @return A character vector of the same length.
#' @noRd
lmed_rule_name_v20260828 <- function(produkt_clean) {
  out <- produkt_clean
  aliases <- lmed_rule_aliases_v20260828()
  for (from in names(aliases)) {
    to <- aliases[[from]]
    hit <- startsWith(out, from) & !startsWith(out, to)
    hit[is.na(hit)] <- FALSE
    out[hit] <- paste0(to, substring(out[hit], nchar(from) + 1L))
  }
  return(out)
}

#' Read the product rules of the 2026-08-28 codebook
#'
#' Returns one row per `MHT_groups` product, with its normalised name and the
#' four rule cells the duration layer reads.
#'
#' @details
#' The columns are `FDDD`, a fixed duration in days, `minimum_monthly_dose` and
#' `minimum_months`, the whole-months rule, and `strength_mg_min` and
#' `strength_mg_max`, which key a rule to a strength band.
#'
#' Every number stays in the sheet. The code reads the cells and never repeats
#' a value, so a codebook edit changes the answer with no code change.
#'
#' @return A `data.table` with one row per codebook product.
#' @noRd
lmed_read_product_rules_v20260828 <- function() {
  path <- system.file(
    "2023-mht",
    "dataDictionary20260828.xlsx",
    package = "mht"
  )
  if (!nzchar(path)) {
    stop("dataDictionary20260828.xlsx is not installed", call. = FALSE)
  }
  wb <- suppressMessages(suppressWarnings(readxl::read_excel(
    path,
    sheet = "MHT_groups",
    col_types = "text"
  )))
  wb <- as.data.frame(wb, stringsAsFactors = FALSE)
  wb <- wb[!is.na(wb$Preparatnamn), , drop = FALSE]
  rules <- data.table::data.table(
    preparatnamn = wb$Preparatnamn,
    name_clean = lmed_normalize_product_name_v20260828(wb$Preparatnamn),
    fddd_fixed = as.numeric(wb$FDDD),
    monthly_dose = as.numeric(wb$minimum_monthly_dose),
    months_min = as.numeric(wb$minimum_months),
    strength_min = as.numeric(wb$strength_mg_min),
    strength_max = as.numeric(wb$strength_mg_max)
  )
  # An empty normalised name prefixes every product, so it would answer for
  # all of them. startsWith(x, "") is TRUE.
  rules <- rules[nzchar(rules$name_clean)]

  lmed_assert_aliases_live_v20260828(rules)
  return(rules)
}

#' Assert that every rule alias still names a live disagreement
#'
#' Errors unless each alias of `lmed_rule_aliases_v20260828()` names a product
#' the codebook leaves out, and takes the rules of a product it carries.
#'
#' @details
#' The check runs in both directions, so no alias outlives its reason. A
#' codebook that gains a `Utrogest` row makes the alias a second rule of equal
#' specificity. A codebook that loses `Utrogestan` leaves the alias with no
#' target. The 2026-08-26 clinician decision holds only while both hold.
#'
#' @param rules The rule table from `lmed_read_product_rules_v20260828()`.
#' @return `invisible(TRUE)`.
#' @noRd
lmed_assert_aliases_live_v20260828 <- function(rules) {
  aliases <- lmed_rule_aliases_v20260828()
  taken <- intersect(names(aliases), rules$name_clean)
  if (length(taken) > 0L) {
    stop(
      "the codebook now carries a rule named ",
      paste(taken, collapse = ", "),
      ", which lmed_rule_aliases_v20260828() also names. Drop the alias.",
      call. = FALSE
    )
  }
  absent <- setdiff(unname(aliases), rules$name_clean)
  if (length(absent) > 0L) {
    stop(
      "an alias of lmed_rule_aliases_v20260828() takes the rules of ",
      paste(absent, collapse = ", "),
      ", which the codebook does not carry",
      call. = FALSE
    )
  }
  return(invisible(TRUE))
}

#' The first few values of a vector, for a diagnostic message
#'
#' Base `head()` lives in `utils`, which this package does not import.
#'
#' @param x A vector.
#' @return At most the first five values of `x`.
#' @noRd
first_few_v20260828 <- function(x) {
  return(x[seq_len(min(5L, length(x)))])
}

#' Report which products a set of codebook names prefixes
#'
#' @param produkt_clean A character vector of normalised product names.
#' @param names A character vector of normalised codebook names.
#' @return A logical vector as long as `produkt_clean`.
#' @noRd
lmed_name_matches_any_v20260828 <- function(produkt_clean, names) {
  out <- rep(FALSE, length(produkt_clean))
  for (nm in names) {
    hit <- startsWith(produkt_clean, nm)
    hit[is.na(hit)] <- FALSE
    out <- out | hit
  }
  return(out)
}

#' Read the strength in milligrams out of a register product name
#'
#' Returns the last strength written in milligrams, or `NA` where the name
#' carries none.
#'
#' @details
#' `lnmn` is the register's full product name, which carries the dosage form and
#' the strength. `Utrogestan, vaginalkapsel, mjuk 200 mg` gives 200.
#'
#' The parser strips every byte that is not an ASCII letter, digit, comma, full
#' stop, solidus or space, then lowercases. That order matters for the same
#' reason it matters in the ladder: a latin1 byte declared `"unknown"` throws
#' `invalid multibyte string` once `tolower()` reaches it.
#'
#' A concentration reads as no strength. `1 mg/g` names milligrams per gram, not
#' a unit strength, so the parser rejects a `mg` followed by a solidus.
#'
#' @param lnmn A character vector of register product names.
#' @return A numeric vector as long as `lnmn`.
#' @noRd
lmed_product_strength_mg_v20260828 <- function(lnmn) {
  s <- stringr::str_remove_all(as.character(lnmn), "[^0-9A-Za-z,./ ]")
  s <- stringr::str_replace_all(tolower(s), ",", ".")
  hits <- stringr::str_extract_all(
    s,
    "[0-9]+(?:\\.[0-9]+)?(?=[ ]*mg\\b(?![ ]*/))"
  )
  out <- vapply(
    hits,
    function(h) {
      h <- h[!is.na(h)]
      if (length(h) == 0L) {
        return(NA_real_)
      }
      return(as.numeric(h[length(h)]))
    },
    numeric(1)
  )
  return(out)
}

#' Resolve the strength every strength-keyed rule needs
#'
#' Returns one strength per prescription, and errors where a strength-keyed rule
#' names the product and no strength is available.
#'
#' @details
#' `lnmn` is REQUIRED as soon as one strength-keyed codebook rule names a
#' product in `x`. Its absence is an error. A warning plus a knowingly wrong
#' duration is not acceptable in a function that freezes.
#'
#' Absence is tolerated only where no strength-keyed rule names any product in
#' `x`. The strength is then `NA` everywhere and no rule reads it.
#'
#' @param x A `data.table` carrying `produkt_clean`, and `lnmn` where a
#'   strength-keyed rule applies.
#' @param rules The rule table from `lmed_read_product_rules_v20260828()`.
#' @return A numeric vector as long as `nrow(x)`.
#' @noRd
lmed_strength_for_rules_v20260828 <- function(x, rules) {
  keyed <- !is.na(rules$strength_min) | !is.na(rules$strength_max)
  needs <- lmed_name_matches_any_v20260828(
    lmed_rule_name_v20260828(x$produkt_clean),
    rules$name_clean[keyed]
  )
  if (!any(needs)) {
    return(rep(NA_real_, nrow(x)))
  }
  if (!"lnmn" %in% names(x)) {
    stop(
      "lnmn is required: it decides the rule for ",
      sum(needs),
      " of ",
      nrow(x),
      " rows, whose codebook rule is keyed on strength (",
      paste(unique(rules$preparatnamn[keyed]), collapse = ", "),
      "). Supply lnmn, the register product name that carries the strength.",
      call. = FALSE
    )
  }
  strength <- lmed_product_strength_mg_v20260828(x$lnmn)
  bad <- needs & is.na(strength)
  if (any(bad)) {
    shown <- first_few_v20260828(unique(as.character(x$lnmn[bad])))
    stop(
      "lnmn carries no strength in milligrams for ",
      sum(bad),
      " of ",
      nrow(x),
      " rows, whose codebook rule is keyed on strength: ",
      paste(shown, collapse = " | "),
      call. = FALSE
    )
  }
  return(strength)
}

#' Choose the one codebook rule that applies to each prescription
#'
#' Returns the index of the applicable rule, or `NA` where no rule applies.
#'
#' @details
#' A rule applies when its normalised name is a prefix of the normalised product
#' name, and the strength falls in the rule's band. `strength_mg_min` is
#' inclusive and `strength_mg_max` is exclusive, so a pair of rows written
#' `NA, 150` and `150, NA` covers every strength exactly once.
#'
#' The longest name wins. `Levosertone 20 mikrogram` starts with both `levosert`
#' and `levosertone`, and the longer name is the more specific product.
#'
#' The match runs on the aliased name, which `lmed_rule_name_v20260828()`
#' returns. A register spelling the codebook does not carry can therefore take
#' another product's rules.
#'
#' Two rules of equal name length that both apply are an error. That is a
#' codebook the code cannot resolve, and it is the exact shape that made `fddd`
#' compound: `Utrogestan 100mg Capsules` once matched a `Utrogest` rule and an
#' `Utrogestan` rule, and the layer applied both.
#'
#' @param produkt_clean A character vector of normalised product names.
#' @param strength_mg A numeric vector of strengths in milligrams.
#' @param rules The rule table to choose from.
#' @return An integer vector as long as `produkt_clean`.
#' @noRd
lmed_select_codebook_rule_v20260828 <- function(
  produkt_clean,
  strength_mg,
  rules
) {
  n <- length(produkt_clean)
  # Match on the aliased name. Every message below names what the caller passed.
  rule_name <- lmed_rule_name_v20260828(produkt_clean)
  chosen <- rep(NA_integer_, n)
  chosen_nchar <- rep(-1L, n)
  tied <- rep(FALSE, n)
  lower <- rules$strength_min
  lower[is.na(lower)] <- -Inf
  upper <- rules$strength_max
  upper[is.na(upper)] <- Inf
  keyed <- !is.na(rules$strength_min) | !is.na(rules$strength_max)

  for (i in seq_len(nrow(rules))) {
    hit <- startsWith(rule_name, rules$name_clean[i])
    hit[is.na(hit)] <- FALSE
    if (keyed[i]) {
      in_band <- !is.na(strength_mg) &
        strength_mg >= lower[i] &
        strength_mg < upper[i]
      hit <- hit & in_band
    }
    width <- nchar(rules$name_clean[i])
    better <- hit & width > chosen_nchar
    chosen[better] <- i
    tied[better] <- FALSE
    tied[hit & width == chosen_nchar] <- TRUE
    chosen_nchar[better] <- width
  }

  if (any(tied)) {
    shown <- first_few_v20260828(unique(produkt_clean[tied]))
    stop(
      "the codebook offers more than one rule of equal specificity for ",
      sum(tied),
      " of ",
      n,
      " rows: ",
      paste(shown, collapse = ", "),
      ". One prescription takes one rule, so the codebook must separate them.",
      call. = FALSE
    )
  }
  return(chosen)
}

#' Overwrite `fddd` with the codebook's own fixed duration
#'
#' Adds nothing. Rewrites `fddd` by reference for every product whose codebook
#' row carries an `FDDD` cell.
#'
#' @details
#' The intrauterine devices are the products with a fixed duration. The category
#' overwrite ahead of this call sets `D3` and `E1` to 1680 days, and this call
#' then reads each product's own cell. Jaydess is 1008 days there, and it is the
#' one product whose cell differs from its category.
#'
#' The call reads no strength, and it errors where the codebook asks it to. A
#' product with a fixed duration carries no dispensed quantity, so this call
#' SUPPLIES the duration that `lmed_durations_v20260828()` screens on. The
#' screen runs before the strength is read, so a fixed duration that depended
#' on a strength could not be resolved in time.
#'
#' @param x A `data.table` carrying `produkt_clean` and `fddd`.
#' @param rules The rule table from `lmed_read_product_rules_v20260828()`.
#' @return `x`, modified by reference.
#' @noRd
lmed_apply_fixed_durations_v20260828 <- function(x, rules) {
  fddd <- NULL

  fixed <- rules[!is.na(rules$fddd_fixed)]
  if (nrow(fixed) == 0L) {
    return(invisible(x))
  }
  keyed <- !is.na(fixed$strength_min) | !is.na(fixed$strength_max)
  if (any(keyed)) {
    stop(
      "a codebook rule carries a fixed duration and a strength band at once: ",
      paste(unique(fixed$preparatnamn[keyed]), collapse = ", "),
      ". A fixed duration is read before the strength is, so it must not ",
      "depend on one.",
      call. = FALSE
    )
  }
  idx <- lmed_select_codebook_rule_v20260828(
    x$produkt_clean,
    rep(NA_real_, nrow(x)),
    fixed
  )
  x[, fddd := data.table::fifelse(is.na(idx), fddd, fixed$fddd_fixed[idx])]
  return(invisible(x))
}

#' Convert a sparse supply into whole months of treatment
#'
#' Rewrites `fddd` by reference for every product whose codebook row carries a
#' `minimum_monthly_dose`.
#'
#' @details
#' A sequential progestogen is taken on some days of each month, not every day.
#' The codebook gives the days of supply that make one month, in
#' `minimum_monthly_dose`, and the months a prescription must reach, in
#' `minimum_months`. A supply below that reaches zero and contributes nothing.
#' A supply above it rounds DOWN to whole months.
#'
#' Exactly one rule applies to each prescription. The frozen layer looped over
#' every matching codebook row and wrote `fddd` back each time, so a product
#' matching two rows compounded.
#'
#' @param x A `data.table` carrying `produkt_clean` and `fddd`.
#' @param rules The rule table from `lmed_read_product_rules_v20260828()`.
#' @param strength_mg A numeric vector of strengths in milligrams.
#' @return `x`, modified by reference.
#' @noRd
lmed_apply_minimum_dose_v20260828 <- function(x, rules, strength_mg) {
  fddd <- NULL

  dosed <- rules[!is.na(rules$monthly_dose)]
  if (nrow(dosed) == 0L) {
    return(invisible(x))
  }
  idx <- lmed_select_codebook_rule_v20260828(
    x$produkt_clean,
    strength_mg,
    dosed
  )

  # The LENGTH of a treatment month, in days. The codebook states it once, in
  # Rules item 5, "One month=28 days", and the person-week grid counts whole
  # weeks, so a treatment month is four weeks of seven days. This is not a
  # dose. Every dose in this function comes from minimum_monthly_dose, which
  # the sheet carries per product.
  days_per_treatment_month <- 4L * 7L

  whole_months <- floor(x$fddd / dosed$monthly_dose[idx])
  reaches_minimum <- whole_months >= dosed$months_min[idx]
  x[,
    fddd := data.table::fifelse(
      is.na(idx),
      fddd,
      data.table::fifelse(
        reaches_minimum,
        whole_months * days_per_treatment_month,
        0
      )
    )
  ]
  return(invisible(x))
}

#' Turn 2026-08-28 dispensed prescriptions into ISO-week exposure intervals
#'
#' Reads dispensed prescriptions and returns one closed interval per
#' prescription that contributes exposure. The caller's `lmed` is never
#' modified.
#'
#' @details
#' The function reads five columns and copies no others:
#' \itemize{
#'   \item `lopnr`, the person identifier
#'   \item `produkt`, the product name the ladder classifies
#'   \item `edatum`, the `Date` of dispensing
#'   \item `fddd`, the dispensed duration in days
#'   \item `lnmn`, the register product name, REQUIRED where a strength-keyed
#'     codebook rule names a product that KEEPS a usable duration
#' }
#'
#' The interval is CLOSED and INCLUSIVE. A prescription dispensed on `edatum`
#' with a duration of `d` days covers `edatum` to `edatum + d - 1`. One day of
#' supply covers one day, and the frozen `edatum + d` covered two.
#'
#' A prescription contributes nothing when it reaches no category, when it
#' carries no dispensing date, or when its rounded duration is missing, is not
#' positive, or is not finite. The negative case is real. 25,678 prescriptions
#' across 20,911 women carry a negative `fddd` in the 2026 delivery. The base is
#' all G02 and G03 prescriptions. The function drops them BEFORE the ISO
#' conversion and reports one aggregate warning.
#'
#' THE ORDER OF THE FOUR STEPS IS LOAD-BEARING:
#' \enumerate{
#'   \item Classify the product.
#'   \item Apply the fixed durations, which SUPPLY a duration where the
#'     register carries none.
#'   \item Drop every prescription with no usable duration and every one with
#'     no dispensing date.
#'   \item Read the strength, and apply the minimum-dose rules.
#' }
#'
#' Step 4 runs last because a prescription with no usable duration has no
#' interval, whatever its strength. In the 2026 delivery a missing strength and
#' a missing `fddd` arrive together. A strength read first turns 63,276 silently
#' dropped rows of the Utrogestan family into a hard stop.
#'
#' A missing `fddd` is never imputed. Inventing a duration would put a woman in
#' a treatment arm on no evidence.
#'
#' @param lmed A `data.table` of dispensed prescriptions.
#' @param verbose Logical. If `TRUE`, report progress with `message()`.
#' @return A new `data.table`, one row per contributing prescription, carrying
#'   `lopnr`, `product_category`, `duration_days`, `start_isoyearweek` and
#'   `stop_isoyearweek`.
#' @noRd
lmed_durations_v20260828 <- function(lmed, verbose = TRUE) {
  # Declare variables for data.table non-standard evaluation
  duration_days <- edatum <- fddd <- product_category <- NULL
  start_date <- stop_date <- start_isoyearweek <- stop_isoyearweek <- NULL
  lnmn <- NULL

  needed <- c("lopnr", "produkt", "edatum", "fddd")
  missing <- setdiff(needed, names(lmed))
  if (length(missing) > 0L) {
    stop(
      "lmed has no column ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  # Build the read set explicitly, so the caller's table is never touched and
  # no unread column is copied.
  x <- data.table::data.table(
    lopnr = lmed$lopnr,
    produkt = lmed$produkt,
    edatum = lmed$edatum,
    fddd = as.numeric(lmed$fddd)
  )
  if ("lnmn" %in% names(lmed)) {
    x[, lnmn := lmed$lnmn]
  }

  if (verbose) {
    message(Sys.time(), " LMED categorizing product names ")
  }
  lmed_categorize_product_names_v20260828(x)

  rules <- lmed_read_product_rules_v20260828()

  if (verbose) {
    message(Sys.time(), " LMED fixing durations ")
  }
  # The intrauterine devices carry no dispensed quantity, so the category
  # answers first and the product's own cell answers second. Both SUPPLY a
  # duration, so both run before the screen below.
  x[product_category == "D3", fddd := 1680]
  x[product_category == "E1", fddd := 1680]
  lmed_apply_fixed_durations_v20260828(x, rules)

  # THE SCREEN RUNS BEFORE THE STRENGTH. A prescription with no usable duration
  # has no interval, whatever its strength, so it must never reach the strength
  # requirement. In the 2026 delivery a missing strength and a missing `fddd`
  # arrive together. 63,276 rows of the Utrogestan family carry neither, and
  # 48,248 of the 48,846 `Utrogest` rows are among them. Read the strength first
  # and every one of those stops the batch.
  x <- x[!is.na(product_category)]
  n_before <- nrow(x)
  x[, duration_days := round(fddd)]
  no_date <- is.na(x$edatum)
  no_duration <- is.na(x$duration_days)
  no_days <- !no_duration &
    !(is.finite(x$duration_days) & x$duration_days > 0)
  x <- x[!no_date & !no_duration & !no_days]

  # The strength decides which codebook rule a SURVIVING row takes. A row with
  # a duration and a strength-keyed rule and no readable strength is still an
  # error: it would otherwise take a knowingly wrong duration.
  strength_mg <- lmed_strength_for_rules_v20260828(x, rules)
  lmed_apply_minimum_dose_v20260828(x, rules, strength_mg)
  x[, duration_days := round(fddd)]

  # The minimum-dose rule reduces a supply below one whole month to zero.
  below_minimum <- x$duration_days <= 0
  x <- x[!below_minimum]

  n_dropped <- n_before - nrow(x)
  if (n_dropped > 0L) {
    warning(
      n_dropped,
      " of ",
      n_before,
      " classified prescriptions dropped: ",
      sum(no_duration),
      " with no duration, ",
      sum(no_days) + sum(below_minimum),
      " with a duration that is no positive number of days, ",
      sum(no_date),
      " with no dispensing date",
      call. = FALSE
    )
  }

  if (verbose) {
    message(Sys.time(), " LMED start/stop ")
  }
  x[, start_date := edatum]
  x[, stop_date := edatum + duration_days - 1L]
  x[, start_isoyearweek := cstime::date_to_isoyearweek_c(start_date)]
  x[, stop_isoyearweek := cstime::date_to_isoyearweek_c(stop_date)]
  return(x[])
}

# ====================================================== person-week counting ====

#' Bridge a gap of up to four weeks, bounded on both sides
#'
#' Sets every `FALSE` run of length four or less to `TRUE`, where a `TRUE` run
#' sits on each side of it.
#'
#' @details
#' A leading or a trailing `FALSE` run is not a gap. It is the weeks before the
#' woman started, or the weeks after she stopped. The frozen helper converted
#' both, so a woman started treatment up to four weeks early and stopped up to
#' four weeks late.
#'
#' The function counts ROWS. `assert_person_weeks_v20260828()` is what makes a
#' row equal to one observed ISO week.
#'
#' @param x A logical vector, in person-week order.
#' @return A logical vector of the same length.
#' @noRd
replace_false_runs_v20260828 <- function(x) {
  runs <- rle(x)
  n <- length(runs$lengths)
  gap <- which(!runs$values & runs$lengths <= 4L)
  runs$values[gap[gap > 1L & gap < n]] <- TRUE
  return(inverse.rle(runs))
}

#' Count the weeks elapsed inside the current `TRUE` run
#'
#' Returns a cumulative sum that restarts at every `FALSE`. A `FALSE` week
#' counts zero.
#'
#' @details
#' The run length is what separates two treatments that a woman holds at once.
#' The treatment she started most recently carries the shorter run, and the
#' approach resolver reports that one. Two equal runs are a clash.
#'
#' The function counts ROWS. `assert_person_weeks_v20260828()` is what makes a
#' row equal to one observed ISO week.
#'
#' The 2026-08-28 layer carries its own copy of this helper. A frozen helper and
#' a live one can then never reach each other's pipeline.
#'
#' @param x A logical vector, in person-week order.
#' @return An integer vector of the same length.
#' @noRd
cumulative_reset_v20260828 <- function(x) {
  return(stats::ave(x, data.table::rleid(!x), FUN = cumsum))
}

#' Assert that every person's weeks are unique and consecutive
#'
#' Errors unless each `id` in `skeleton` holds each ISO week once, with no gap
#' between the first week and the last.
#'
#' @details
#' Gap bridging and the three-year minimum-duration rule both count ROWS. A row
#' equals one elapsed ISO week only where a person's weeks are unique and
#' consecutive. The exported entry point takes an arbitrary `data.table`, so
#' nothing else guarantees it.
#'
#' The check errors rather than computing elapsed weeks from the dates. Counting
#' elapsed weeks would read a break in follow-up as continuous observation, and
#' the layer counts observed weeks only.
#'
#' Row ORDER is not checked here. `apply_lmed_categories_to_skeleton_v20260828()`
#' sorts the skeleton by `id` and `isoyearweek` before any consumer counts rows.
#'
#' @param skeleton A person-week `data.table` with `id` and `isoyearweek`.
#' @return `invisible(TRUE)`.
#' @noRd
assert_person_weeks_v20260828 <- function(skeleton) {
  # Declare variables for data.table non-standard evaluation
  . <- NULL
  id <- week_index <- n_weeks <- n_distinct <- span <- NULL

  missing <- setdiff(c("id", "isoyearweek"), names(skeleton))
  if (length(missing) > 0L) {
    stop(
      "skeleton has no column ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  if (nrow(skeleton) == 0L) {
    return(invisible(TRUE))
  }

  # Convert the DISTINCT weeks only. The grid holds a few hundred of them and
  # many million rows.
  weeks <- sort(unique(skeleton$isoyearweek))
  dates <- cstime::isoyearweek_to_last_date(weeks)
  if (anyNA(dates)) {
    stop(
      "skeleton$isoyearweek holds a value that is no ISO week: ",
      paste(first_few_v20260828(weeks[is.na(dates)]), collapse = ", "),
      call. = FALSE
    )
  }
  offset <- as.integer(dates - min(dates))
  if (any(offset %% 7L != 0L)) {
    stop(
      "skeleton$isoyearweek does not resolve to whole weeks apart",
      call. = FALSE
    )
  }
  index <- stats::setNames(offset %/% 7L, weeks)

  w <- data.table::data.table(
    id = skeleton$id,
    week_index = unname(index[skeleton$isoyearweek])
  )
  per_person <- w[,
    .(
      n_weeks = .N,
      n_distinct = data.table::uniqueN(week_index),
      span = max(week_index) - min(week_index) + 1L
    ),
    keyby = .(id)
  ]
  bad <- per_person[n_distinct != n_weeks | span != n_weeks]
  if (nrow(bad) > 0L) {
    stop(
      "the check fails for ",
      nrow(bad),
      " of ",
      nrow(per_person),
      " persons. Each person must hold each ISO week once, consecutively. id ",
      bad$id[1],
      " has ",
      bad$n_weeks[1],
      " rows, ",
      bad$n_distinct[1],
      " distinct weeks, spanning ",
      bad$span[1],
      " weeks",
      call. = FALSE
    )
  }
  return(invisible(TRUE))
}
