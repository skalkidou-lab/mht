# ================================================= the 2026-09-24 lookup layer ====
#
# This layer adds approach 4 to the 2026-09-22 layer. It reads
# `product_table_20260924.xlsx` and `dataDictionary20260924.xlsx`, and no other
# workbook. The product table is the 2026-09-22 table with one note cell
# reworded. The dictionary adds the 47 approach 4 rules to the `post_grouping`
# sheet.
#
# Every helper that reads a workbook, directly or through another helper, has a
# `_v20260924` copy in this file. The exposure layer has a copy too, because it
# must write the two `rd_approach4` columns. A dated helper that reads no
# workbook is called unchanged, as the 2026-09-22 layer does.

#' Read the 2026-09-24 product table
#'
#' A copy of `lmed_read_product_table_v20260922()` that reads
#' `inst/2023-mht/product_table_20260924.xlsx`.
#' `lmed_read_product_table_v20260902()` documents the checks.
#'
#' @return A `data.table` keyed on `produkt_clean`.
#' @noRd
lmed_read_product_table_v20260924 <- function() {
  . <- NULL
  produkt_clean <- classification <- exclude_entire_person <- NULL
  exclusion_reason <- n_class <- n_excl <- n_reason <- NULL

  path <- system.file(
    "2023-mht",
    "product_table_20260924.xlsx",
    package = "mht"
  )
  if (!nzchar(path)) {
    stop("product_table_20260924.xlsx is not installed", call. = FALSE)
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

#' Read the product rules of the 2026-09-24 codebook
#'
#' A copy of `lmed_read_product_rules_v20260922()` that reads the `MHT_groups`
#' sheet of `inst/2023-mht/dataDictionary20260924.xlsx`.
#'
#' @return A `data.table` with one row per codebook product.
#' @noRd
lmed_read_product_rules_v20260924 <- function() {
  path <- system.file(
    "2023-mht",
    "dataDictionary20260924.xlsx",
    package = "mht"
  )
  if (!nzchar(path)) {
    stop("dataDictionary20260924.xlsx is not installed", call. = FALSE)
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

#' Turn dispensed prescriptions into ISO-week exposure intervals, 2026-09-24
#'
#' A copy of `lmed_durations_v20260922()` that reads its duration rules from
#' the 2026-09-24 codebook. `lmed_durations_v20260902()` documents the order of
#' the steps.
#'
#' @param lmed A `data.table` of dispensed prescriptions.
#' @param tab The table from `lmed_read_product_table_v20260924()`.
#' @param verbose Logical. If `TRUE`, report progress with `message()`.
#' @return A new `data.table`, one row per contributing prescription.
#' @noRd
lmed_durations_v20260924 <- function(lmed, tab, verbose = TRUE) {
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

  rules <- lmed_read_product_rules_v20260924()

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

#' Read the approach rules of the 2026-09-24 codebook
#'
#' A copy of `lmed_read_approach_rules_v20260922()` that reads the
#' `post_grouping` sheet of `inst/2023-mht/dataDictionary20260924.xlsx`.
#'
#' @return A list of rules, one per rule of the sheet, in sheet order.
#' @noRd
lmed_read_approach_rules_v20260924 <- function() {
  path <- system.file(
    "2023-mht",
    "dataDictionary20260924.xlsx",
    package = "mht"
  )
  if (!nzchar(path)) {
    stop("dataDictionary20260924.xlsx is not installed", call. = FALSE)
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

#' Derive the 2026-09-24 approach variables from the product-category columns
#'
#' A copy of `apply_lmed_approaches_to_skeleton_v20260922()` that reads its
#' rules with `lmed_read_approach_rules_v20260924()`. It writes `approach1` to
#' `approach4`. `apply_lmed_approaches_to_skeleton_v20260828()` documents the
#' resolver.
#'
#' @param skeleton A person-week `data.table` carrying one logical column per
#'   product category, sorted by `id` and `isoyearweek`.
#' @return `skeleton`, modified by reference.
#' @noRd
apply_lmed_approaches_to_skeleton_v20260924 <- function(skeleton) {
  # Declare variables for data.table non-standard evaluation
  . <- NULL
  id <- run_min <- tied_n <- NULL

  # A VARIABLE of post_grouping, not a category. The sheet names it in every
  # approach, and it is the value an untreated week takes.
  reference_level <- "local_or_none_mht"

  rules <- lmed_read_approach_rules_v20260924()
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

#' Build the 2026-09-24 `rd_approach*` exposure variables
#'
#' A copy of `create_exposure_variables_v20260828()` that also turns
#' `approach4` into `rd_approach4_single` and `rd_approach4_multiple`. That
#' function documents the five steps.
#'
#' @details
#' The function writes ten columns: the `single` and `multiple` variants of
#' `approach1` to `approach4`, and of `approach3b`. `approach3b` collapses the
#' two progesterone levels of `approach3`, as in the 2026-08-28 layer.
#'
#' @param skeleton A person-week `data.table` carrying `approach1` to
#'   `approach4`, sorted by `id` and `isoyearweek`.
#' @param create_rd Logical. If `FALSE`, the function writes no `rd_approach*`
#'   column, and deletes every one that `skeleton` already carries.
#' @return `skeleton`, modified by reference.
#' @noRd
create_exposure_variables_v20260924 <- function(skeleton, create_rd = TRUE) {
  # Declare variables for data.table non-standard evaluation
  . <- NULL
  id <- isoyearweek <- var_to_clean <- var_to_clean_lag1 <- NULL
  isoyearweek_first_previous <- temp <- NULL
  reinitiation_isoyearweek <- NULL
  on_mht <- n <- length_on_mht <- last_session_on_mht <- NULL

  if (!create_rd) {
    # A stale column reads as this run's answer and is not. Delete it.
    stale <- rd_approach_columns_v20260828(skeleton)
    if (length(stale) > 0L) {
      skeleton[, (stale) := NULL]
    }
    return(invisible(skeleton))
  }

  for (i in c("approach1", "approach2", "approach3", "approach4")) {
    for (p in c("single", "multiple")) {
      final_var <- paste0("rd_", i, "_", p)
      skeleton[, var_to_clean := get(i)]

      # Step 1. The move to no treatment, and every untreated week after it.
      skeleton[,
        var_to_clean_lag1 := shift(var_to_clean, type = "lag"),
        by = .(id)
      ]
      skeleton[is.na(var_to_clean_lag1), var_to_clean_lag1 := var_to_clean]
      skeleton[
        var_to_clean_lag1 != "local_or_none_mht" &
          var_to_clean == "local_or_none_mht",
        var_to_clean := "previous"
      ]
      skeleton[var_to_clean == "previous", temp := isoyearweek]
      skeleton[,
        isoyearweek_first_previous := first_non_na_v20260828(temp),
        by = .(id)
      ]
      skeleton[, temp := NULL]
      skeleton[
        is.na(isoyearweek_first_previous),
        isoyearweek_first_previous := "9999-99"
      ]
      skeleton[
        isoyearweek >= isoyearweek_first_previous &
          var_to_clean == "local_or_none_mht",
        var_to_clean := "previous"
      ]
      skeleton[, isoyearweek_first_previous := NULL]
      skeleton[, var_to_clean_lag1 := NULL]

      # Step 2. The first week that leaves `previous` for an active level.
      skeleton[,
        var_to_clean_lag1 := shift(var_to_clean, type = "lag"),
        by = .(id)
      ]
      skeleton[is.na(var_to_clean_lag1), var_to_clean_lag1 := var_to_clean]
      skeleton[
        var_to_clean_lag1 == "previous" & var_to_clean != "previous",
        temp := isoyearweek
      ]
      skeleton[,
        reinitiation_isoyearweek := first_non_na_v20260828(temp),
        by = .(id)
      ]
      skeleton[, temp := NULL]
      skeleton[
        is.na(reinitiation_isoyearweek),
        reinitiation_isoyearweek := "9999-99"
      ]
      skeleton[, var_to_clean_lag1 := NULL]

      # Step 3. One lifetime episode, in the `single` variant alone.
      if (p == "single") {
        skeleton[
          isoyearweek >= reinitiation_isoyearweek,
          var_to_clean := "exclude"
        ]
      }
      skeleton[, reinitiation_isoyearweek := NULL]

      # Step 4. Three years of treatment, or the woman is no former user.
      skeleton[,
        on_mht := !var_to_clean %in%
          c("local_or_none_mht", "previous", "exclude")
      ]
      skeleton[, n := seq_len(.N), by = .(id, data.table::rleid(on_mht))]
      skeleton[, length_on_mht := .N, by = .(id, data.table::rleid(on_mht))]
      skeleton[on_mht == FALSE, length_on_mht := 0]
      skeleton[, last_session_on_mht := shift(length_on_mht), by = .(id)]
      skeleton[n != 1, last_session_on_mht := NA]
      skeleton[,
        last_session_on_mht := first_non_na_v20260828(last_session_on_mht),
        by = .(id, data.table::rleid(on_mht))
      ]
      skeleton[is.na(last_session_on_mht), last_session_on_mht := 0]
      skeleton[
        last_session_on_mht < 3 * 52 & var_to_clean == "previous",
        var_to_clean := "exclude"
      ]
      skeleton[, on_mht := NULL]
      skeleton[, n := NULL]
      skeleton[, length_on_mht := NULL]
      skeleton[, last_session_on_mht := NULL]

      # Step 5.
      skeleton[, (final_var) := var_to_clean]
      skeleton[, var_to_clean := NULL]
    }
  }

  for (p in c("single", "multiple")) {
    src <- paste0("rd_approach3_", p)
    dst <- paste0("rd_approach3b_", p)
    skeleton[, (dst) := get(src)]
    skeleton[
      get(dst) %in%
        c(
          "estrogen_progesterone_bioidentical",
          "estrogen_progesterone_synthetic"
        ),
      (dst) := "estrogen_progesterone"
    ]
  }
  return(invisible(skeleton))
}

#' Add 2026-09-24 MHT exposure to a person-week skeleton
#'
#' Writes every column that `add_lmed_v20260922()` writes, and adds `approach4`,
#' `rd_approach4_single` and `rd_approach4_multiple`. It reads
#' `inst/2023-mht/product_table_20260924.xlsx` and
#' `inst/2023-mht/dataDictionary20260924.xlsx`, and no other workbook.
#'
#' @details
#' Every column other than the three approach 4 columns equals the output of
#' `add_lmed_v20260922()`. Approach 4 splits systemic oestrogen by route and by
#' progestogen type. The `post_grouping` sheet gives it these levels:
#' \itemize{
#'   \item `oral_estrogen_progesterone`: B4 or B11, or oral oestrogen (A2 or
#'     A6) with C1 or C5.
#'   \item `oral_estrogen_other_progestogen`: B2, B3, B5, B6, B8, B9, B10 or
#'     B12, or A2 or A6 with C3, C4, D1, D2, D3, E1, I1 or I2.
#'   \item `transdermal_estrogen_progesterone`: transdermal oestrogen (A1) with
#'     C1 or C5.
#'   \item `transdermal_estrogen_other_progestogen`: B1 or B7, or A1 with C3,
#'     C4, D1, D2, D3, E1, I1 or I2.
#'   \item `estrogen_only`: A1, A2 or A6, in a week with no B1 to B12, C1 to C5,
#'     D1 to D3, E1, I1 or I2.
#'   \item `tibolone`: F1.
#'   \item `local_or_none_mht`: a week that lights no level above.
#' }
#'
#' The resolver is that of `add_lmed_v20260922()`. When two levels are lit,
#' `approach4` reports the level that started most recently. Two levels that
#' start in the same week are `clashingprescriptions` until the treated episode
#' ends.
#'
#' @param skeleton A person-week `data.table` with `id` and `isoyearweek`.
#'   Weekly rows only, as for `add_lmed_v20260922()`.
#' @param lmed A `data.table` of dispensed prescriptions.
#' @param id_name The person identifier column of `lmed`.
#' @param create_rd Logical. If `FALSE`, writes no `rd_approach*` column.
#' @param verbose Logical. If `TRUE`, report progress with `message()`.
#' @return `invisible(skeleton)`. Mutation is in place.
#' @seealso \code{\link{add_lmed_v20260922}}, which documents the behaviour the
#'   two entry points share. `vignette("lmed-v20260924")` explains approach 4,
#'   and checks each of its worked cases when it builds.
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
#'   add_lmed_v20260924(skeleton, lmed, verbose = FALSE)
#' )
#' skeleton[, .N, keyby = .(approach4)]
#' @export
add_lmed_v20260924 <- function(
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
  tab <- lmed_read_product_table_v20260924()
  # BEFORE the cohort restriction, so a product held only by people outside
  # the skeleton still has to be a product somebody has ruled on.
  lmed_assert_products_known_v20260922(lmed, tab)

  reads <- lmed_read_set_v20260828(lmed, id_name, unique(skeleton[["id"]]))
  intervals <- lmed_durations_v20260924(reads, tab, verbose = verbose)
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
  apply_lmed_approaches_to_skeleton_v20260924(work)
  if (verbose) {
    message(Sys.time(), " LMED create exposure variables ")
  }
  create_exposure_variables_v20260924(work, create_rd = create_rd)

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
