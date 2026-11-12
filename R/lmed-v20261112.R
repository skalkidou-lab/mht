# ================================================= the 2026-11-12 exposure layer ====
#
# This layer splits each `rd_approach*` column of the 2026-11-05 layer into two
# columns, one per rule for former use. It reads the same two workbooks as the
# 2026-11-05 layer, through the same helpers, so it adds no workbook and no
# lookup helper. Only the exposure layer and the entry point have copies here.

# The rules for former use: the shortest treated episode, in weeks, whose stop
# gives `previous`. The name of each rule is the suffix of its columns.
# `prev3y` is the 2026-11-05 rule of three years. `prevany` has no minimum, so
# any stop gives `previous`.
previous_rules_v20261112 <- c(
  prev3y = 156L,
  prevany = 0L
)

#' Build one `rd_approach*` exposure column, 2026-11-12
#'
#' Runs the five steps of `create_exposure_variables_v20261105()` for one
#' approach, one variant and one rule for former use, and writes the result to
#' `final_var`. Step 4 runs only when `min_weeks` is above zero.
#'
#' @param skeleton A person-week `data.table` carrying the column `approach`,
#'   sorted by `id` and `isoyearweek`.
#' @param approach The name of the approach column, for example `"approach1"`.
#' @param reference_level The reference level of that approach.
#' @param variant `"single"` or `"multiple"`.
#' @param min_weeks The shortest treated episode, in weeks, whose stop gives
#'   `previous`. Zero skips step 4.
#' @param final_var The name of the column to write.
#' @return `skeleton`, modified by reference.
#' @noRd
lmed_rd_column_v20261112 <- function(
  skeleton,
  approach,
  reference_level,
  variant,
  min_weeks,
  final_var
) {
  # Declare variables for data.table non-standard evaluation
  . <- NULL
  id <- isoyearweek <- var_to_clean <- var_to_clean_lag1 <- NULL
  isoyearweek_first_previous <- temp <- NULL
  reinitiation_isoyearweek <- NULL
  on_mht <- n <- length_on_mht <- last_session_on_mht <- NULL

  skeleton[, var_to_clean := get(approach)]

  # Step 1. The move to no treatment, and every untreated week after it.
  skeleton[,
    var_to_clean_lag1 := shift(var_to_clean, type = "lag"),
    by = .(id)
  ]
  skeleton[is.na(var_to_clean_lag1), var_to_clean_lag1 := var_to_clean]
  skeleton[
    var_to_clean_lag1 != reference_level &
      var_to_clean == reference_level,
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
      var_to_clean == reference_level,
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
  if (variant == "single") {
    skeleton[
      isoyearweek >= reinitiation_isoyearweek,
      var_to_clean := "exclude"
    ]
  }
  skeleton[, reinitiation_isoyearweek := NULL]

  # Step 4. An episode of `min_weeks` weeks of treatment, or the woman is no
  # former user. The `prevany` rule has no minimum and skips this step.
  if (min_weeks > 0L) {
    skeleton[,
      on_mht := !var_to_clean %in%
        c(reference_level, "previous", "exclude")
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
      last_session_on_mht < min_weeks & var_to_clean == "previous",
      var_to_clean := "exclude"
    ]
    skeleton[, on_mht := NULL]
    skeleton[, n := NULL]
    skeleton[, length_on_mht := NULL]
    skeleton[, last_session_on_mht := NULL]
  }

  # Step 5.
  skeleton[, (final_var) := var_to_clean]
  skeleton[, var_to_clean := NULL]
  return(invisible(skeleton))
}

#' Build the 2026-11-12 `rd_approach*` exposure variables
#'
#' A copy of `create_exposure_variables_v20261105()` that writes each column
#' twice, once per rule for former use in `previous_rules_v20261112`.
#' `create_exposure_variables_v20260828()` documents the five steps.
#'
#' @details
#' The function writes 28 columns, named
#' `rd_approach<N>_<variant>_<rule>`: `N` is 1 to 6 or `3b`, `variant` is
#' `single` or `multiple`, and `rule` is `prev3y` or `prevany`.
#'
#' The `prev3y` columns equal the 2026-11-05 columns of the same approach and
#' variant. A stop after an episode of 156 weeks or more gives `previous`, and
#' a shorter one gives `exclude`. The `prevany` columns skip step 4, so any
#' stop gives `previous`. Steps 1, 2, 3 and 5 are the same in both rules.
#'
#' `approach3b` collapses the two progesterone levels of `approach3`, per rule,
#' as in the 2026-08-28 layer.
#'
#' @param skeleton A person-week `data.table` carrying `approach1` to
#'   `approach6`, sorted by `id` and `isoyearweek`.
#' @param create_rd Logical. If `FALSE`, the function writes no `rd_approach*`
#'   column, and deletes every one that `skeleton` already carries.
#' @return `skeleton`, modified by reference.
#' @noRd
create_exposure_variables_v20261112 <- function(skeleton, create_rd = TRUE) {
  if (!create_rd) {
    # A stale column reads as this run's answer and is not. Delete it.
    stale <- rd_approach_columns_v20260828(skeleton)
    if (length(stale) > 0L) {
      skeleton[, (stale) := NULL]
    }
    return(invisible(skeleton))
  }

  for (i in names(reference_levels_v20261105)) {
    for (p in c("single", "multiple")) {
      for (r in names(previous_rules_v20261112)) {
        lmed_rd_column_v20261112(
          skeleton,
          approach = i,
          reference_level = reference_levels_v20261105[[i]],
          variant = p,
          min_weeks = previous_rules_v20261112[[r]],
          final_var = paste0("rd_", i, "_", p, "_", r)
        )
      }
    }
  }

  for (p in c("single", "multiple")) {
    for (r in names(previous_rules_v20261112)) {
      src <- paste0("rd_approach3_", p, "_", r)
      dst <- paste0("rd_approach3b_", p, "_", r)
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
  }
  return(invisible(skeleton))
}

#' Add 2026-11-12 MHT exposure to a person-week skeleton
#'
#' Writes every column that `add_lmed_v20261105()` writes, except that each
#' `rd_approach*` column is written twice, once per rule for former use. It
#' reads the same two workbooks as `add_lmed_v20261105()`:
#' `inst/2023-mht/product_table_20261105.xlsx` and
#' `inst/2023-mht/dataDictionary20261105.xlsx`.
#'
#' @details
#' The function writes 28 `rd_approach*` columns, named
#' `rd_approach<N>_<variant>_<rule>`. `N` is 1 to 6 or `3b`, `variant` is
#' `single` or `multiple`, and `rule` is one of the two rules below. It writes
#' no column named `rd_approach<N>_<variant>` without a rule.
#' \itemize{
#'   \item `prev3y`: an untreated week after a stop is `previous` only if the
#'     treated episode lasted 156 weeks or more. After a shorter episode it is
#'     `exclude`. This is the rule of `add_lmed_v20261105()`, and each `prev3y`
#'     column equals the `add_lmed_v20261105()` column of the same approach and
#'     variant.
#'   \item `prevany`: any stop gives `previous`, whatever the length of the
#'     episode.
#' }
#'
#' The two rules differ only in the weeks after a stop from an episode shorter
#' than 156 weeks: `prev3y` gives `exclude` and `prevany` gives `previous`. In
#' the `single` variant both rules set every week from a re-initiation onward to
#' `exclude`. The four-week gap bridge runs before the rules and is the same for
#' both.
#'
#' Every other column equals the output of `add_lmed_v20261105()`.
#'
#' @param skeleton A person-week `data.table` with `id` and `isoyearweek`.
#'   Weekly rows only, as for `add_lmed_v20261105()`.
#' @param lmed A `data.table` of dispensed prescriptions.
#' @param id_name The person identifier column of `lmed`.
#' @param create_rd Logical. If `FALSE`, writes no `rd_approach*` column.
#' @param verbose Logical. If `TRUE`, report progress with `message()`.
#' @return `invisible(skeleton)`. Mutation is in place.
#' @seealso \code{\link{add_lmed_v20261105}}, which documents the approaches.
#'   `vignette("lmed-v20261112")` explains approaches 1 to 6 and the two rules.
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
#'   add_lmed_v20261112(skeleton, lmed, verbose = FALSE)
#' )
#' skeleton[, .N, keyby = .(rd_approach1_single_prev3y, rd_approach1_single_prevany)]
#' @export
add_lmed_v20261112 <- function(
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
  tab <- lmed_read_product_table_v20261105()
  # BEFORE the cohort restriction, so a product held only by people outside
  # the skeleton still has to be a product somebody has ruled on.
  lmed_assert_products_known_v20260922(lmed, tab)

  reads <- lmed_read_set_v20260828(lmed, id_name, unique(skeleton[["id"]]))
  intervals <- lmed_durations_v20261105(reads, tab, verbose = verbose)
  excluded <- lmed_person_exclusions_v20260922(reads, tab)

  work <- data.table::data.table(
    id = skeleton[["id"]],
    isoyearweek = skeleton[["isoyearweek"]]
  )
  data.table::set(work, j = row_col, value = seq_len(nrow(work)))

  if (verbose) {
    message(Sys.time(), " LMED apply categories to skeleton ")
  }
  apply_lmed_categories_to_skeleton_v20260828(
    work,
    intervals,
    verbose = verbose
  )
  if (verbose) {
    message(Sys.time(), " LMED apply approaches ")
  }
  apply_lmed_approaches_to_skeleton_v20261105(work)
  if (verbose) {
    message(Sys.time(), " LMED create exposure variables ")
  }
  create_exposure_variables_v20261112(work, create_rd = create_rd)

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

  # This drops the unsuffixed columns of an earlier entry point too.
  stale <- setdiff(rd_approach_columns_v20260828(skeleton), out_cols)
  if (length(stale) > 0L) {
    skeleton[, (stale) := NULL]
  }

  if (verbose) {
    message(Sys.time(), " LMED finished ")
  }
  return(invisible(skeleton))
}
