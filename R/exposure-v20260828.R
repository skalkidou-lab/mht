# ======================================================== the approach layer ====
#
# Two steps sit between the product-category columns and an analysis. The first
# reads the `post_grouping` sheet and writes `approach1`, `approach2` and
# `approach3`. The second turns those into the `rd_approach*` exposure columns.
#
# The sheet is DATA, never code. The frozen resolver built R source from the
# same cells with `glue::glue()` and ran `eval(parse())`. This one reads the
# cells and evaluates them directly.

#' Read the approach rules of the 2026-08-28 codebook
#'
#' Returns one entry per rule of the `post_grouping` sheet, in sheet order.
#'
#' @details
#' Each entry holds four fields. `approach` names the approach the rule belongs
#' to. `variable` names the treatment group the rule lights. `includes` holds
#' the categories a week must carry, from `includes1` and `includes2`.
#' `excludes` holds the categories it must not carry, from the 30
#' `doesnotinclude` columns.
#'
#' No category is named here. The sheet is the one place a category enters the
#' approach definitions, so a codebook edit changes the answer with no code
#' change.
#'
#' @return A list of rules.
#' @noRd
lmed_read_approach_rules_v20260828 <- function() {
  path <- system.file(
    "2023-mht",
    "dataDictionary20260828.xlsx",
    package = "mht"
  )
  if (!nzchar(path)) {
    stop("dataDictionary20260828.xlsx is not installed", call. = FALSE)
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

#' The categories one rule names in one set of columns
#'
#' Drops every empty cell, so a rule carries only the categories it reads.
#'
#' @param row One row of the `post_grouping` sheet, as a `data.frame`.
#' @param cols A character vector of column names.
#' @return A character vector, possibly empty.
#' @noRd
lmed_rule_cells_v20260828 <- function(row, cols) {
  cols <- cols[cols %in% names(row)]
  v <- as.character(unlist(row[, cols, drop = FALSE], use.names = FALSE))
  v <- v[!is.na(v)]
  return(v[nzchar(v)])
}

#' Assert that the grid carries every category the approach rules read
#'
#' Errors unless `skeleton` holds one column per category named in the rules.
#'
#' @details
#' This is the invariant, checked where it is used. A rule that reads a category
#' the person-week grid never materialises is a silent no-op. The column is
#' absent, so the rule lights nothing.
#'
#' The other direction is not an error. A category the grid materialises and no
#' rule reads cannot change exposure, and `dev/check-crosswalk.R` reports every
#' one of them.
#'
#' @param skeleton A person-week `data.table`.
#' @param rules The rule list from `lmed_read_approach_rules_v20260828()`.
#' @return `invisible(TRUE)`.
#' @noRd
lmed_assert_categories_present_v20260828 <- function(skeleton, rules) {
  read <- unlist(lapply(rules, function(r) c(r$includes, r$excludes)))
  absent <- sort(unique(setdiff(read, names(skeleton))))
  if (length(absent) > 0L) {
    stop(
      "the approach rules read ",
      paste(absent, collapse = ", "),
      ", which the person-week grid does not carry. Every category a rule ",
      "reads must be a column of the skeleton.",
      call. = FALSE
    )
  }
  return(invisible(TRUE))
}

#' Light one logical column per approach variable
#'
#' Adds one column per variable of `app` to `skeleton` by reference, and returns
#' the variable names in sheet order.
#'
#' @details
#' A rule lights its variable in every week that carries all of `includes` and
#' none of `excludes`. Several rules can name one variable, and each of them
#' lights it on its own.
#'
#' A rule with an empty `includes1` declares its variable and lights nothing.
#' The 2026-08-28 sheet carries one, for `local_or_none_mht` in approach 3.
#'
#' @param skeleton A person-week `data.table` carrying the category columns.
#' @param app The rules of one approach.
#' @return A character vector of variable names.
#' @noRd
lmed_light_approach_variables_v20260828 <- function(skeleton, app) {
  vars <- unique(vapply(app, function(r) r$variable, character(1)))
  for (v in vars) {
    skeleton[, (v) := FALSE]
  }
  for (r in app) {
    if (length(r$includes) == 0L) {
      next
    }
    hit <- rep(TRUE, nrow(skeleton))
    for (nm in r$includes) {
      hit <- hit & skeleton[[nm]]
    }
    for (nm in r$excludes) {
      hit <- hit & !skeleton[[nm]]
    }
    skeleton[hit, (r$variable) := TRUE]
  }
  return(vars)
}

#' Carry a clash to the end of the treated episode, and no further
#'
#' Rewrites `approach_name` to `clashingprescriptions` for every week from the
#' first clash of an episode to the last treated week of that same episode.
#'
#' @details
#' A woman who starts two conflicting treatments in one week leaves that
#' comparison. She leaves it from the clash until she stops MHT altogether. Her
#' first untreated week is then an ordinary stop, and
#' `create_exposure_variables_v20260828()` records her as a former user.
#'
#' Neither end of that rule is arbitrary. The frozen resolver carried the clash
#' to the end of follow-up, so a woman who stopped and restarted never returned
#' to the cohort. The opposite defect ends the clash with the overlap. She then
#' reads as a new user in the week the overlap ends. She was treated throughout.
#' These are new-user analyses.
#'
#' A treated episode is a run of weeks that carry a run length. The four-week
#' bridge runs before this, so a bridged gap does not end an episode.
#'
#' @param skeleton A person-week `data.table` carrying `run_min` and `tied_n`.
#' @param approach_name The name of the approach column to rewrite.
#' @return `skeleton`, modified by reference.
#' @noRd
lmed_carry_clash_v20260828 <- function(skeleton, approach_name) {
  # Declare variables for data.table non-standard evaluation
  . <- NULL
  id <- run_min <- tied_n <- on_mht <- episode <- carry <- NULL

  skeleton[, on_mht := !is.na(run_min)]
  skeleton[, episode := data.table::rleid(on_mht), by = .(id)]
  skeleton[, carry := cumsum(tied_n > 1L) > 0L, by = .(id, episode)]
  skeleton[carry == TRUE, (approach_name) := "clashingprescriptions"]
  skeleton[, on_mht := NULL]
  skeleton[, episode := NULL]
  skeleton[, carry := NULL]
  return(invisible(skeleton))
}

#' Derive the 2026-08-28 approach variables from the product-category columns
#'
#' Reads the `post_grouping` sheet of `dataDictionary20260828.xlsx` and adds
#' `approach1`, `approach2` and `approach3` to `skeleton` by reference.
#'
#' @details
#' The resolver reports the treatment a woman started most recently. It gives
#' each treatment variable a run length, which counts the weeks since that
#' variable last turned on. It then reports the variable with the shortest run.
#'
#' `local_or_none_mht` is the reference level. It carries no run length, so it
#' can never tie with a treatment variable. The clash is two treatment variables
#' that light in the same week, and `lmed_carry_clash_v20260828()` states how
#' far it reaches.
#'
#' Every week outside every run carries no run length. The approach column then
#' keeps the reference level.
#'
#' The function deletes every column it made, apart from the three it returns.
#'
#' @param skeleton A person-week `data.table` carrying one logical column per
#'   product category, sorted by `id` and `isoyearweek`.
#' @return `skeleton`, modified by reference.
#' @noRd
apply_lmed_approaches_to_skeleton_v20260828 <- function(skeleton) {
  # Declare variables for data.table non-standard evaluation
  . <- NULL
  id <- run_min <- tied_n <- NULL

  # A VARIABLE of post_grouping, not a category. The sheet names it in every
  # approach, and it is the value an untreated week takes.
  reference_level <- "local_or_none_mht"

  rules <- lmed_read_approach_rules_v20260828()
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

# ======================================================== the exposure layer ====

#' The first value of a vector that is not `NA`
#'
#' @details
#' The 2026-08-28 layer carries its own copy, so a change to a shared helper
#' cannot reach a frozen layer and a live one at once.
#'
#' @param x A vector of any type.
#' @return The first value of `x` that is not `NA`.
#' @noRd
first_non_na_v20260828 <- function(x) {
  return(dplyr::first(na.omit(x)))
}

#' The eight exposure columns the 2026-08-28 layer owns
#'
#' @details
#' The function owns every column whose name starts with `rd_approach`. That is
#' wider than the eight it writes, and it is deliberate: `create_rd = FALSE`
#' must leave no stale column of any earlier run behind.
#'
#' @param skeleton A person-week `data.table`.
#' @return A character vector of column names, possibly empty.
#' @noRd
rd_approach_columns_v20260828 <- function(skeleton) {
  return(grep("^rd_approach", names(skeleton), value = TRUE))
}

#' Build the 2026-08-28 `rd_approach*` exposure variables
#'
#' Turns `approach1`, `approach2` and `approach3` into the eight
#' `rd_approach*_single` and `rd_approach*_multiple` columns.
#'
#' @details
#' Each approach runs twice. The `single` variant models one lifetime episode.
#' It sets every week from a re-initiation onward to `exclude`. The `multiple`
#' variant lets a woman re-enter her new level, and excludes no person-time for
#' a re-initiation alone.
#'
#' Five steps run per approach and per variant:
#' \enumerate{
#'   \item Mark the move from an active level to `local_or_none_mht` as
#'     `previous`. Carry `previous` over every later untreated week.
#'   \item Find the first week that moves from `previous` back to an active
#'     level.
#'   \item Set every week from that re-initiation onward to `exclude`, in the
#'     `single` variant alone.
#'   \item Rewrite `previous` to `exclude` where the episode before it ran under
#'     three years, which is 156 weeks.
#'   \item Write the result to `rd_approach<N>_<variant>`.
#' }
#'
#' `clashingprescriptions` counts as an active level in every step. The layer
#' therefore records a woman who leaves a clash for no treatment as a former
#' user.
#'
#' `approach3b` collapses the two progesterone levels of the finished
#' `approach3` columns. A switch between active levels never creates `previous`,
#' so the relabel gives what a full second pass gives.
#'
#' @param skeleton A person-week `data.table` carrying `approach1`, `approach2`
#'   and `approach3`, sorted by `id` and `isoyearweek`.
#' @param create_rd Logical. If `FALSE`, the function writes no `rd_approach*`
#'   column, and deletes every one that `skeleton` already carries. The eight
#'   columns cost about 570 MB per batch of 10,000 people.
#' @return `skeleton`, modified by reference.
#' @noRd
create_exposure_variables_v20260828 <- function(skeleton, create_rd = TRUE) {
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

  for (i in c("approach1", "approach2", "approach3")) {
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
