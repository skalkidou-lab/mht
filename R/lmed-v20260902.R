# ================================================= the 2026-09-02 lookup layer ====
#
# This layer differs from the 2026-08-28 one in exactly one respect: a product
# gets its category from a shipped TABLE, looked up on an exact key, instead of
# from a 116-rung prefix ladder.
#
# It calls the frozen 2026-08-28 duration, approach and exposure helpers rather
# than copying them. The freeze rule forbids EDITING a dated artefact; it does
# not forbid a newer layer from calling one. Calling cannot change what the
# older entry point does, and every helper reached here carries its own date in
# its name, so nothing undated is shared between the two.
#
# The normaliser is reused for a stronger reason than tidiness. The table's keys
# were BUILT with `lmed_normalize_product_name_v20260828()`. A second copy that
# ever diverged from it would not error: every lookup would simply miss, and
# every product would report as unknown.

#' The categories the person-week grid materialises
#'
#' @details
#' Read from `apply_lmed_categories_to_skeleton_v20260828()`'s own vector, so a
#' second hand-written list cannot drift from the one that builds the columns.
#'
#' @return A character vector.
#' @noRd
lmed_legal_classifications_v20260902 <- function(produkt_raw) {
  # The grid's own vector, plus every category the FROZEN LADDER actually
  # returns for these products. The ladder is RUN, not read: every wrong
  # conclusion in this package's history came from reimplementing a classifier
  # by parsing its source instead of executing it.
  #
  # The union is needed rather than the grid alone because of `G1`. Duavive is
  # classified and then deliberately discarded, so it materialises no column on
  # purpose and would otherwise be rejected here.
  probe <- data.table::data.table(produkt = unique(produkt_raw))
  lmed_categorize_product_names_v20260828(probe)
  return(unique(c(
    lmed_materialised_categories_v20260902(),
    stats::na.omit(probe$product_category)
  )))
}

#' The categories the person-week grid materialises
#'
#' @return A character vector.
#' @noRd
lmed_materialised_categories_v20260902 <- function() {
  src <- deparse(body(apply_lmed_categories_to_skeleton_v20260828))
  block <- paste(src, collapse = " ")
  block <- sub('.*product_categories <- c\\((.*?)\\).*', "\\1", block)
  out <- regmatches(block, gregexpr('"[A-Z][0-9]+"', block))[[1]]
  out <- gsub('"', "", out)
  if (length(out) < 20L) {
    stop(
      "could not read product_categories from the 2026-08-28 layer; ",
      "found ", length(out),
      call. = FALSE
    )
  }
  return(out)
}

#' Validate the product table, cell by cell
#'
#' @details
#' The table is an INPUT, so every cell is checked. A cell the reader tolerates
#' is a cell nobody checks: a classification typed `Al` for `A1` materialises no
#' category column and leaves the person in the reference arm, and an exclusion
#' cell reading anything but TRUE keeps her in the cohort. Neither would raise
#' anything at all.
#'
#' Separate from the reader so the failure branches can be exercised with a
#' synthetic table, rather than only by shipping a broken file.
#'
#' @param tab The table as read, every column character.
#' @return `tab`, with `exclude_entire_person` as a logical.
#' @noRd
lmed_validate_product_table_v20260902 <- function(tab) {
  classification <- exclude_entire_person <- NULL

  # THE TABLE IS AN INPUT, SO EVERY CELL IS VALIDATED. A cell the reader
  # tolerates is a cell nobody checks: a classification typed `Al` for `A1`
  # materialises no category column and leaves the person in the reference
  # arm, and an exclusion cell reading anything but TRUE keeps her in the
  # cohort. Neither would raise anything at all.
  raw <- if ("produkt_raw" %in% names(tab)) tab$produkt_raw else character(0)
  legal <- c(lmed_legal_classifications_v20260902(raw), "notmht")
  wrong <- tab[!classification %chin% legal]
  if (nrow(wrong) > 0L) {
    stop(
      nrow(wrong),
      " table rows carry a classification that is neither a materialised ",
      "category nor notmht: ",
      paste(first_few_v20260828(unique(wrong$classification)), collapse = ", "),
      ". A category the person-week grid never materialises reaches no ",
      "approach rule, so the person would read as untreated.",
      call. = FALSE
    )
  }

  excl_raw <- toupper(trimws(as.character(tab$exclude_entire_person)))
  odd <- unique(excl_raw[!excl_raw %chin% c("TRUE", "FALSE")])
  if (length(odd) > 0L) {
    stop(
      "the exclude_entire_person column carries ",
      paste(first_few_v20260828(odd), collapse = ", "),
      ". Only TRUE and FALSE are read. Any other value would silently keep ",
      "the person in the cohort.",
      call. = FALSE
    )
  }
  tab[, exclude_entire_person := excl_raw == "TRUE"]

  # The key is RECOMPUTED, never trusted. It is supplied in the file, and a
  # key that disagreed with its own raw name would misroute every lookup for
  # that row without erroring.
  if ("produkt_raw" %in% names(tab)) {
    recomputed <- lmed_normalize_product_name_v20260828(tab$produkt_raw)
    drift <- which(recomputed != tab$produkt_clean)
    if (length(drift) > 0L) {
      stop(
        length(drift),
        " table rows carry a produkt_clean that is not produkt_raw normalised: ",
        paste(first_few_v20260828(tab$produkt_raw[drift]), collapse = ", "),
        call. = FALSE
      )
    }
  }

  return(tab[])
}

#' Read the 2026-09-02 product table
#'
#' Returns one row per lookup key, with the classification, the person-level
#' exclusion flag and its reason.
#'
#' @details
#' The table ships as `inst/2023-mht/product_table_20260902.xlsx`. It carries a
#' sheet per ATC scope, and `README`, which is documentation and is not read.
#'
#' Several raw register names reduce to one key: `Estramon 25`, `Estramon 75`
#' and `Estramon 100` are one product at three strengths. Rows sharing a key
#' MUST agree, because the lookup matches the key and cannot see which raw name
#' produced it. This function checks that and errors where they disagree.
#'
#' @return A `data.table` keyed on `produkt_clean`.
#' @noRd
lmed_read_product_table_v20260902 <- function() {
  . <- NULL
  produkt_clean <- classification <- exclude_entire_person <- NULL
  exclusion_reason <- n_class <- n_excl <- n_reason <- NULL

  path <- system.file(
    "2023-mht",
    "product_table_20260902.xlsx",
    package = "mht"
  )
  if (!nzchar(path)) {
    stop("product_table_20260902.xlsx is not installed", call. = FALSE)
  }
  sheets <- setdiff(readxl::excel_sheets(path), "README")
  tab <- data.table::rbindlist(
    lapply(sheets, function(s) {
      data.table::setDT(suppressMessages(suppressWarnings(
        readxl::read_excel(path, sheet = s, col_types = "text")
      )))
    }),
    use.names = TRUE,
    fill = TRUE
  )

  need <- c(
    "produkt_clean",
    "classification",
    "exclude_entire_person",
    "exclusion_reason"
  )
  absent <- setdiff(need, names(tab))
  if (length(absent) > 0L) {
    stop(
      "the product table carries no ",
      paste(absent, collapse = ", "),
      " column",
      call. = FALSE
    )
  }

  tab <- lmed_validate_product_table_v20260902(tab)

  out <- tab[,
    .(
      n_class = data.table::uniqueN(classification),
      n_excl = data.table::uniqueN(exclude_entire_person),
      n_reason = data.table::uniqueN(exclusion_reason),
      classification = classification[1],
      exclude_entire_person = exclude_entire_person[1],
      exclusion_reason = exclusion_reason[1]
    ),
    keyby = produkt_clean
  ]
  bad <- out[n_class > 1L | n_excl > 1L | n_reason > 1L]
  if (nrow(bad) > 0L) {
    stop(
      nrow(bad),
      " lookup keys carry rows that disagree, on classification, on the ",
      "exclusion flag or on its reason: ",
      paste(first_few_v20260828(bad$produkt_clean), collapse = ", "),
      ". The lookup matches the key, so it cannot choose between them.",
      call. = FALSE
    )
  }
  out[, c("n_class", "n_excl", "n_reason") := NULL]
  return(out[])
}

#' Categorise 2026-09-02 LMED product names by exact lookup
#'
#' Adds `produkt_clean` and `product_category` to `x` by reference.
#'
#' @details
#' The match is EXACT on the normalised name, not a prefix. A prefix match
#' guesses: it is how `Depo-Provera` reached the `Provera` duration rule for
#' 72,767 people, and how `Evorel Micronor` reached oestrogen-only.
#'
#' A delivered product with no row is an ERROR, not a silent `notmht`. An
#' unclassified product reads as no MHT, so the woman enters as an unexposed
#' control; that is how tibolone put roughly 91,000 women in the comparator.
#' A delivery is a frozen file, so within one delivery this can fire only once,
#' when the table is first built against it.
#'
#' `notmht` in the table means the product is not MHT. That is a recorded
#' decision, and it is a different thing from a product nobody has ruled on.
#'
#' @param x A `data.table` of dispensed prescriptions with a `produkt` column.
#' @return `x`, modified by reference.
#' @noRd
lmed_categorize_product_names_v20260902 <- function(x) {
  produkt_clean <- product_category <- produkt <- classification <- NULL
  i.classification <- NULL

  tab <- lmed_read_product_table_v20260902()
  x[, produkt_clean := lmed_normalize_product_name_v20260828(produkt)]

  unknown <- setdiff(unique(x$produkt_clean), tab$produkt_clean)
  if (length(unknown) > 0L) {
    shown <- unique(x$produkt[x$produkt_clean %chin% unknown])
    stop(
      length(unknown),
      " delivered products have no row in the product table: ",
      paste(first_few_v20260828(shown), collapse = " | "),
      ". A product with no row would read as no MHT, so the woman would enter ",
      "as an unexposed control. Add each one to the table, as a category or ",
      "as notmht, before the run.",
      call. = FALSE
    )
  }

  x[tab, on = "produkt_clean", product_category := i.classification]
  # `notmht` is a recorded decision that the product is not MHT. The duration
  # layer drops a row with no category, so it must reach that layer as NA.
  x[product_category == "notmht", product_category := NA_character_]
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
#' supply covers one day.
#'
#' A prescription contributes nothing when it reaches no category, when it
#' carries no dispensing date, or when its rounded duration is missing, is not
#' positive, or is not finite. A negative `fddd` is possible, so the negative
#' case is a real one. The function drops every such prescription BEFORE the
#' ISO conversion and reports one aggregate warning.
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
#' interval, whatever its strength. A strength and an `fddd` can both be absent
#' from one prescription. A strength read first would turn every such row into
#' a hard stop.
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
lmed_durations_v20260902 <- function(lmed, verbose = TRUE) {
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
  lmed_categorize_product_names_v20260902(x)

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
  # requirement. A strength and an `fddd` can both be absent from one
  # prescription. Read the strength first and every such row stops the batch.
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
# ==================================================== person-level exclusion ====

#' The people the study removes, and why
#'
#' Returns one row per person the product table flags.
#'
#' @details
#' The flag is person level and time invariant, which is what the `ri_` prefix
#' means in the pipelines that consume this. A woman who ever held a flagged
#' product carries it in every one of her weeks.
#'
#' The function REPORTS. It removes nobody. Removing a person is a change to
#' the cohort, and this layer owns columns of `skeleton`, never its rows. The
#' caller decides, so paper 1 and the 2026 pipeline can act differently.
#'
#' Where a person holds products flagged for different reasons, the reasons
#' join in sorted order, so the value does not depend on row order.
#'
#' @param reads The projected `lmed`, carrying `lopnr` and `produkt`.
#' @return A `data.table` of `id`, `ri_mht_excluded_product` and
#'   `ri_mht_excluded_reason`.
#' @noRd
lmed_person_exclusions_v20260902 <- function(reads) {
  . <- NULL
  produkt_clean <- exclude_entire_person <- exclusion_reason <- NULL
  lopnr <- reason <- id <- i.exclusion_reason <- NULL

  tab <- lmed_read_product_table_v20260902()
  flagged <- tab[exclude_entire_person == TRUE]
  if (nrow(flagged) == 0L) {
    return(data.table::data.table(
      id = character(0),
      ri_mht_excluded_product = logical(0),
      ri_mht_excluded_reason = character(0)
    ))
  }

  x <- data.table::data.table(
    id = reads[["lopnr"]],
    produkt_clean = lmed_normalize_product_name_v20260828(reads[["produkt"]])
  )
  x <- x[produkt_clean %chin% flagged$produkt_clean]
  if (nrow(x) == 0L) {
    return(data.table::data.table(
      id = character(0),
      ri_mht_excluded_product = logical(0),
      ri_mht_excluded_reason = character(0)
    ))
  }
  x[flagged, on = "produkt_clean", reason := i.exclusion_reason]
  out <- x[,
    .(
      ri_mht_excluded_product = TRUE,
      ri_mht_excluded_reason = paste(
        sort(unique(stats::na.omit(reason))),
        collapse = "; "
      )
    ),
    keyby = id
  ]
  return(out[])
}

#' Assert that every delivered product has a row, before anything is dropped
#'
#' @details
#' The guarantee is about the DELIVERY, so it is checked against the delivery.
#' `lmed_read_set_v20260828()` restricts `lmed` to the people in the skeleton,
#' and a check that ran after it would never see a product held only by people
#' outside the cohort. Those rows are exactly the ones nobody has looked at.
#'
#' @param lmed The caller's `lmed`, before any restriction.
#' @return `invisible(TRUE)`.
#' @noRd
lmed_assert_products_known_v20260902 <- function(lmed) {
  tab <- lmed_read_product_table_v20260902()
  raw <- unique(as.character(lmed[["produkt"]]))
  clean <- lmed_normalize_product_name_v20260828(raw)
  unknown <- raw[!clean %chin% tab$produkt_clean]
  if (length(unknown) > 0L) {
    stop(
      length(unknown),
      " delivered products have no row in the product table: ",
      paste(first_few_v20260828(unknown), collapse = " | "),
      ". A product with no row would read as no MHT, so the woman would enter ",
      "as an unexposed control. Add each one to the table, as a category or ",
      "as notmht, before the run.",
      call. = FALSE
    )
  }
  return(invisible(TRUE))
}

#' Add 2026-09-02 MHT exposure to a person-week skeleton
#'
#' Writes the product category columns, `approach1` to `approach3`, the
#' `rd_approach*` columns, and the two person-level exclusion columns.
#'
#' @details
#' The layer differs from `add_lmed_v20260828()` in one respect: a product gets
#' its category from `inst/2023-mht/product_table_20260902.xlsx`, looked up on
#' an exact normalised key, rather than from a prefix ladder. A delivered
#' product with no row is an error.
#'
#' It adds two columns that no earlier entry point wrote:
#' \itemize{
#'   \item `ri_mht_excluded_product`, `TRUE` for every week of a person who ever
#'     held a product the table flags for removal
#'   \item `ri_mht_excluded_reason`, the reason, or `NA`
#' }
#'
#' **The function removes nobody.** It reports. Removing a person changes the
#' cohort, and this layer owns columns of `skeleton`, never its rows.
#'
#' Everything else is the 2026-08-28 behaviour, reached by calling that layer's
#' own dated helpers rather than by copying them.
#'
#' @param skeleton A person-week `data.table` with `id` and `isoyearweek`.
#' @param lmed A `data.table` of dispensed prescriptions.
#' @param id_name The person identifier column of `lmed`.
#' @param create_rd Logical. If `FALSE`, writes no `rd_approach*` column.
#' @param verbose Logical. If `TRUE`, report progress with `message()`.
#' @return `invisible(skeleton)`. Mutation is in place.
#' @examples
#' library(data.table)
#'
#' skeleton <- copy(fake_skeleton_mht)
#' lmed <- copy(fake_lmed_2026)
#'
#' # Utrogestan takes a codebook rule keyed on strength, so it needs `lnmn`.
#' lmed[, lnmn := NA_character_]
#' lmed[produkt == "Utrogestan", lnmn := "Utrogestan, kapsel, mjuk 100 mg"]
#'
#' # The product table is built from the real deliveries, so it holds no row
#' # for a name that exists only in a fixture. `Paracetamol` is one: the
#' # register writes branded spellings such as `Paracetamol Kodein Evolan`.
#' # Dropping it changes no exposure, because it is not MHT either way.
#' lmed <- lmed[produkt != "Paracetamol"]
#'
#' # the fixture holds one negative-duration row, which warns
#' skeleton <- suppressWarnings(
#'   add_lmed_v20260902(skeleton, lmed, verbose = FALSE)
#' )
#' skeleton[, .N, keyby = .(rd_approach1_single)]
#' skeleton[, .N, keyby = .(ri_mht_excluded_product, ri_mht_excluded_reason)]
#' @export
add_lmed_v20260902 <- function(
  skeleton,
  lmed,
  id_name = "lopnr",
  create_rd = TRUE,
  verbose = TRUE
) {
  # Declare variables for data.table non-standard evaluation
  ri_mht_excluded_product <- ri_mht_excluded_reason <- NULL
  i.ri_mht_excluded_product <- i.ri_mht_excluded_reason <- NULL

  row_col <- ".caller_row"

  lmed_assert_entry_arguments_v20260828(skeleton, lmed, id_name)
  assert_person_weeks_v20260828(skeleton)

  if (verbose) {
    message(Sys.time(), " LMED restricting")
  }
  # BEFORE the cohort restriction, so a product held only by people outside
  # the skeleton still has to be a product somebody has ruled on.
  lmed_assert_products_known_v20260902(lmed)

  reads <- lmed_read_set_v20260828(lmed, id_name, unique(skeleton[["id"]]))
  intervals <- lmed_durations_v20260902(reads, verbose = verbose)
  excluded <- lmed_person_exclusions_v20260902(reads)

  work <- data.table::data.table(
    id = skeleton[["id"]],
    isoyearweek = skeleton[["isoyearweek"]]
  )
  data.table::set(work, j = row_col, value = seq_len(nrow(work)))

  if (verbose) {
    message(Sys.time(), " LMED apply categories to skeleton ")
  }
  apply_lmed_categories_to_skeleton_v20260828(work, intervals, verbose = verbose)
  if (verbose) {
    message(Sys.time(), " LMED apply approaches ")
  }
  apply_lmed_approaches_to_skeleton_v20260828(work)
  if (verbose) {
    message(Sys.time(), " LMED create exposure variables ")
  }
  create_exposure_variables_v20260828(work, create_rd = create_rd)

  work[, ri_mht_excluded_product := FALSE]
  work[, ri_mht_excluded_reason := NA_character_]
  work[
    excluded,
    on = "id",
    `:=`(
      ri_mht_excluded_product = i.ri_mht_excluded_product,
      ri_mht_excluded_reason = i.ri_mht_excluded_reason
    )
  ]

  if (!identical(sort(work[[row_col]]), seq_len(nrow(work)))) {
    stop(
      "a working column overwrote ",
      row_col,
      ", so the caller's row order cannot be restored",
      call. = FALSE
    )
  }
  data.table::setorderv(work, row_col)

  out_cols <- setdiff(names(work), c("id", "isoyearweek", row_col))
  skeleton[, (out_cols) := work[, out_cols, with = FALSE]]

  stale <- setdiff(rd_approach_columns_v20260828(skeleton), out_cols)
  if (length(stale) > 0L) {
    skeleton[, (stale) := NULL]
  }

  if (verbose) {
    message(Sys.time(), " LMED finished ")
  }
  return(invisible(skeleton))
}
