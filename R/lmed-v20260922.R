# ================================================= the 2026-09-22 lookup layer ====
#
# This layer differs from the 2026-09-02 one in its two workbooks. It reads
# `product_table_20260922.xlsx` and `dataDictionary20260922.xlsx`, and no other
# workbook.
#
# Every helper that reads a workbook, directly or through another helper, has a
# `_v20260922` copy in this file. The older helpers stay frozen on their own
# workbooks. A dated helper that reads no workbook is called unchanged, as the
# 2026-09-02 layer does.
#
# The entry point reads the product table once and passes it down. The
# 2026-09-02 layer reads its table three times, from one file.

#' Read the 2026-09-22 product table
#'
#' A copy of `lmed_read_product_table_v20260902()` that reads
#' `inst/2023-mht/product_table_20260922.xlsx`. That function documents the
#' checks.
#'
#' @return A `data.table` keyed on `produkt_clean`.
#' @noRd
lmed_read_product_table_v20260922 <- function() {
  . <- NULL
  produkt_clean <- classification <- exclude_entire_person <- NULL
  exclusion_reason <- n_class <- n_excl <- n_reason <- NULL

  path <- system.file(
    "2023-mht",
    "product_table_20260922.xlsx",
    package = "mht"
  )
  if (!nzchar(path)) {
    stop("product_table_20260922.xlsx is not installed", call. = FALSE)
  }
  sheets <- setdiff(readxl::excel_sheets(path), "README")
  tab <- data.table::rbindlist(
    lapply(sheets, function(s) {
      return(data.table::setDT(suppressMessages(suppressWarnings(
        readxl::read_excel(path, sheet = s, col_types = "text")
      ))))
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

#' Categorise LMED product names by exact lookup in the 2026-09-22 table
#'
#' A variant of `lmed_categorize_product_names_v20260902()` that takes the
#' product table as an argument. Adds `produkt_clean` and `product_category` to
#' `x` by reference.
#'
#' @param x A `data.table` of dispensed prescriptions with a `produkt` column.
#' @param tab The table from `lmed_read_product_table_v20260922()`.
#' @return `x`, modified by reference.
#' @noRd
lmed_categorize_product_names_v20260922 <- function(x, tab) {
  produkt_clean <- product_category <- produkt <- classification <- NULL
  i.classification <- NULL

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

#' Read the product rules of the 2026-09-22 codebook
#'
#' A copy of `lmed_read_product_rules_v20260828()` that reads the `MHT_groups`
#' sheet of `inst/2023-mht/dataDictionary20260922.xlsx`.
#'
#' @return A `data.table` with one row per codebook product.
#' @noRd
lmed_read_product_rules_v20260922 <- function() {
  path <- system.file(
    "2023-mht",
    "dataDictionary20260922.xlsx",
    package = "mht"
  )
  if (!nzchar(path)) {
    stop("dataDictionary20260922.xlsx is not installed", call. = FALSE)
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

#' Turn dispensed prescriptions into ISO-week exposure intervals, 2026-09-22
#'
#' A variant of `lmed_durations_v20260902()` that takes the product table as an
#' argument, and reads its duration rules from the 2026-09-22 codebook. That
#' function documents the order of the steps.
#'
#' @param lmed A `data.table` of dispensed prescriptions.
#' @param tab The table from `lmed_read_product_table_v20260922()`.
#' @param verbose Logical. If `TRUE`, report progress with `message()`.
#' @return A new `data.table`, one row per contributing prescription.
#' @noRd
lmed_durations_v20260922 <- function(lmed, tab, verbose = TRUE) {
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
  lmed_categorize_product_names_v20260922(x, tab)

  rules <- lmed_read_product_rules_v20260922()

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
  # requirement.
  x <- x[!is.na(product_category)]
  n_before <- nrow(x)
  x[, duration_days := round(fddd)]
  no_date <- is.na(x$edatum)
  no_duration <- is.na(x$duration_days)
  no_days <- !no_duration &
    !(is.finite(x$duration_days) & x$duration_days > 0)
  x <- x[!no_date & !no_duration & !no_days]

  # The strength decides which codebook rule a SURVIVING row takes.
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

#' Read the approach rules of the 2026-09-22 codebook
#'
#' A copy of `lmed_read_approach_rules_v20260828()` that reads the
#' `post_grouping` sheet of `inst/2023-mht/dataDictionary20260922.xlsx`.
#'
#' @return A list of rules, one per rule of the sheet, in sheet order.
#' @noRd
lmed_read_approach_rules_v20260922 <- function() {
  path <- system.file(
    "2023-mht",
    "dataDictionary20260922.xlsx",
    package = "mht"
  )
  if (!nzchar(path)) {
    stop("dataDictionary20260922.xlsx is not installed", call. = FALSE)
  }
  pg <- suppressMessages(suppressWarnings(readxl::read_excel(
    path,
    sheet = "post_grouping",
    col_types = "text"
  )))
  pg <- as.data.frame(pg, stringsAsFactors = FALSE)
  pg <- pg[!is.na(pg$approach), , drop = FALSE]

  nameless <- is.na(pg$variable) | !nzchar(pg$variable)
  if (any(nameless)) {
    stop(
      sum(nameless),
      " rows of post_grouping name an approach and no variable",
      call. = FALSE
    )
  }

  include_cols <- c("includes1", "includes2")
  exclude_cols <- grep("^doesnotinclude", names(pg), value = TRUE)
  out <- vector("list", nrow(pg))
  for (i in seq_len(nrow(pg))) {
    out[[i]] <- list(
      approach = pg$approach[i],
      variable = pg$variable[i],
      includes = lmed_rule_cells_v20260828(pg[i, ], include_cols),
      excludes = lmed_rule_cells_v20260828(pg[i, ], exclude_cols)
    )
  }
  return(out)
}

#' Derive the 2026-09-22 approach variables from the product-category columns
#'
#' A copy of `apply_lmed_approaches_to_skeleton_v20260828()` that reads its
#' rules with `lmed_read_approach_rules_v20260922()`. The 2026-08-28 function
#' documents the resolver.
#'
#' @param skeleton A person-week `data.table` carrying one logical column per
#'   product category, sorted by `id` and `isoyearweek`.
#' @return `skeleton`, modified by reference.
#' @noRd
apply_lmed_approaches_to_skeleton_v20260922 <- function(skeleton) {
  # Declare variables for data.table non-standard evaluation
  . <- NULL
  id <- run_min <- tied_n <- NULL

  # A VARIABLE of post_grouping, not a category. The sheet names it in every
  # approach, and it is the value an untreated week takes.
  reference_level <- "local_or_none_mht"

  rules <- lmed_read_approach_rules_v20260922()
  lmed_assert_categories_present_v20260828(skeleton, rules)

  approach_of <- vapply(rules, function(r) r$approach, character(1))
  for (a in unique(approach_of)) {
    app <- rules[approach_of == a]
    vars <- lmed_light_approach_variables_v20260828(skeleton, app)
    treatment_vars <- setdiff(vars, reference_level)
    run_vars <- paste0("run_", treatment_vars)

    for (v in treatment_vars) {
      skeleton[, (v) := replace_false_runs_v20260828(get(v)), by = .(id)]
    }
    for (k in seq_along(treatment_vars)) {
      skeleton[,
        (run_vars[k]) := cumulative_reset_v20260828(get(treatment_vars[k])),
        by = .(id)
      ]
      # A week outside the run holds no run length, and NA never wins a pmin.
      skeleton[get(run_vars[k]) == 0L, (run_vars[k]) := NA_integer_]
    }
    skeleton[,
      run_min := do.call(pmin, c(.SD, na.rm = TRUE)),
      .SDcols = run_vars
    ]

    approach_name <- paste0("approach", a)
    skeleton[, (approach_name) := reference_level]
    skeleton[, tied_n := 0L]
    for (k in seq_along(treatment_vars)) {
      skeleton[
        get(run_vars[k]) == run_min,
        (approach_name) := treatment_vars[k]
      ]
      skeleton[get(run_vars[k]) == run_min, tied_n := tied_n + 1L]
    }
    lmed_carry_clash_v20260828(skeleton, approach_name)

    skeleton[, run_min := NULL]
    skeleton[, tied_n := NULL]
    for (v in c(run_vars, vars)) {
      skeleton[, (v) := NULL]
    }
  }
  return(invisible(skeleton))
}

#' The people the 2026-09-22 product table flags, and why
#'
#' A variant of `lmed_person_exclusions_v20260902()` that takes the product
#' table as an argument. That function documents the flag.
#'
#' @param reads The projected `lmed`, carrying `lopnr` and `produkt`.
#' @param tab The table from `lmed_read_product_table_v20260922()`.
#' @return A `data.table` of `id`, `ri_mht_excluded_product` and
#'   `ri_mht_excluded_reason`.
#' @noRd
lmed_person_exclusions_v20260922 <- function(reads, tab) {
  . <- NULL
  produkt_clean <- exclude_entire_person <- exclusion_reason <- NULL
  lopnr <- reason <- id <- i.exclusion_reason <- NULL

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

#' Assert that every delivered product has a row in the 2026-09-22 table
#'
#' A variant of `lmed_assert_products_known_v20260902()` that takes the product
#' table as an argument. It runs before the cohort restriction.
#'
#' @param lmed The caller's `lmed`, before any restriction.
#' @param tab The table from `lmed_read_product_table_v20260922()`.
#' @return `invisible(TRUE)`.
#' @noRd
lmed_assert_products_known_v20260922 <- function(lmed, tab) {
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

#' Add 2026-09-22 MHT exposure to a person-week skeleton
#'
#' Writes the product category columns, `approach1` to `approach3`, the
#' `rd_approach*` columns, and the two person-level exclusion columns. It reads
#' `inst/2023-mht/product_table_20260922.xlsx` and
#' `inst/2023-mht/dataDictionary20260922.xlsx`, and no other workbook.
#'
#' @details
#' The output equals that of `add_lmed_v20260902()`, except for the effects of
#' two workbook changes:
#' \itemize{
#'   \item `Estradiol Valerate` is category `A7`. A woman with any dispensing
#'     of it gets `ri_mht_excluded_product = TRUE` and the reason
#'     `possible gender-affirming therapy`.
#'   \item No rule of the `post_grouping` sheet reads `A7`. The `A7` column
#'     still shows each week an `A7` product covers, and the approach columns
#'     ignore it.
#' }
#'
#' @param skeleton A person-week `data.table` with `id` and `isoyearweek`.
#'   Weekly rows only, as for `add_lmed_v20260902()`.
#' @param lmed A `data.table` of dispensed prescriptions.
#' @param id_name The person identifier column of `lmed`.
#' @param create_rd Logical. If `FALSE`, writes no `rd_approach*` column.
#' @param verbose Logical. If `TRUE`, report progress with `message()`.
#' @return `invisible(skeleton)`. Mutation is in place.
#' @seealso \code{\link{add_lmed_v20260902}}, which documents the behaviour the
#'   two entry points share.
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
#' # The product table holds no row for the fixture-only name `Paracetamol`.
#' lmed <- lmed[produkt != "Paracetamol"]
#'
#' # the fixture holds one negative-duration row, which warns
#' skeleton <- suppressWarnings(
#'   add_lmed_v20260922(skeleton, lmed, verbose = FALSE)
#' )
#' skeleton[, .N, keyby = .(ri_mht_excluded_product, ri_mht_excluded_reason)]
#' @export
add_lmed_v20260922 <- function(
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
  tab <- lmed_read_product_table_v20260922()
  # BEFORE the cohort restriction, so a product held only by people outside
  # the skeleton still has to be a product somebody has ruled on.
  lmed_assert_products_known_v20260922(lmed, tab)

  reads <- lmed_read_set_v20260828(lmed, id_name, unique(skeleton[["id"]]))
  intervals <- lmed_durations_v20260922(reads, tab, verbose = verbose)
  excluded <- lmed_person_exclusions_v20260922(reads, tab)

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
  apply_lmed_approaches_to_skeleton_v20260922(work)
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
