#' Build the 2026 `rd_approach*` exposure variables
#'
#' Turns the `approach1`, `approach2` and `approach3` columns into the eight
#' `rd_approach*_single` / `rd_approach*_multiple` exposure variables, applying
#' the `previous` / `exclude` rules described in the comment block below.
#'
#' @param skeleton A person-week `data.table` already carrying `approach1`,
#'   `approach2` and `approach3`.
#' @return `skeleton`, modified by reference.
#' @noRd
create_exposure_variables_v20250909 <- function(skeleton) {
  # ---------------------------------------------------------------------------
  # Exposure variable creation
  # ---------------------------------------------------------------------------
  #
  # INPUT
  # -----
  # Requires columns from apply_lmed_approaches_to_skeleton_v20250909():
  #   approach1, approach2, approach3  (character, one row per person-week)
  #
  # OUTPUT
  # ------
  # Creates 8 character exposure columns (3 approaches x 2 variants + 1 derived):
  #   rd_approach1_single, rd_approach1_multiple
  #   rd_approach2_single, rd_approach2_multiple
  #   rd_approach3_single, rd_approach3_multiple
  #   rd_approach3b_single, rd_approach3b_multiple
  #
  # CATEGORICAL LEVELS
  # ------------------
  # Each exposure column can take the following values. The treatment-specific
  # levels come from the upstream approach; the remaining levels are added
  # by the processing steps below.
  #
  # --- Approach 1 (systemic vs local/none) ---
  #   "systemic_mht"          - Currently on systemic MHT
  #   "local_or_none_mht"     - Never started MHT, or only local/no treatment
  #   "clashingprescriptions" - Overlapping incompatible prescriptions (from upstream)
  #   "previous"              - Was on MHT but stopped (added by step A)
  #   "exclude"               - Censored (added by steps C/D)
  #
  # --- Approach 2 (route of estrogen administration) ---
  #   "peroral_estrogen"      - Currently on peroral estrogen
  #   "transdermal_estrogen"  - Currently on transdermal estrogen
  #   "local_or_none_mht"     - Never started MHT, or only local/no treatment
  #   "clashingprescriptions" - Overlapping incompatible prescriptions (from upstream)
  #   "previous"              - Was on MHT but stopped (added by step A)
  #   "exclude"               - Censored (added by steps C/D)
  #
  # --- Approach 3 (estrogen +/- progesterone type) ---
  #   "estrogen_only"                      - Estrogen without progesterone
  #   "estrogen_progesterone_bioidentical"  - Estrogen + bioidentical progesterone
  #   "estrogen_progesterone_synthetic"     - Estrogen + synthetic progesterone
  #   "local_or_none_mht"                  - Never started MHT, or only local/no treatment
  #   "clashingprescriptions"               - Overlapping incompatible prescriptions (from upstream)
  #   "previous"                            - Was on MHT but stopped (added by step A)
  #   "exclude"                             - Censored (added by steps C/D)
  #
  # --- Approach 3b (estrogen +/- progesterone, collapsed) ---
  #   "estrogen_only"          - Estrogen without progesterone
  #   "estrogen_progesterone"  - Estrogen + any progesterone (bio or synthetic)
  #   "local_or_none_mht"      - Never started MHT, or only local/no treatment
  #   "clashingprescriptions"  - Overlapping incompatible prescriptions (from upstream)
  #   "previous"               - Was on MHT but stopped (added by step A)
  #   "exclude"                - Censored (added by steps C/D)
  #
  #   Note: approach3b is derived by relabeling the finished approach3 columns.
  #   This is valid because switching between active MHT types (e.g.,
  #   bioidentical → synthetic) never triggers "previous" — only transitions
  #   to "local_or_none_mht" do — so the relabeled columns are identical to
  #   what the full pipeline would produce.
  #
  # SINGLE vs MULTIPLE VARIANT
  # --------------------------
  # Each approach is computed twice, once as "single" and once as "multiple":
  #
  #   "single":  Models a single lifetime MHT episode. If a person stops MHT
  #              and later re-initiates, all rows from re-initiation onward
  #              are set to "exclude". This is the stricter, intention-to-treat
  #              style definition — a person contributes person-time only for
  #              their first MHT episode and subsequent "previous" period.
  #
  #   "multiple": Allows multiple MHT episodes. If a person stops and
  #              re-initiates, they re-enter their new treatment level.
  #              Person-time is never excluded due to re-initiation alone.
  #              This captures the full treatment history including switches
  #              between different MHT types.
  #
  # Both variants still apply the 3-year minimum duration rule (step D):
  # if an MHT episode was < 3 years, subsequent "previous" rows become
  # "exclude" regardless of variant.
  #
  # UPSTREAM NOTE
  # -------------
  # The input approach1/approach2/approach3 columns have already had gaps of
  # <= 4 weeks filled by replace_false_runs_v20250909() inside
  # apply_lmed_approaches_to_skeleton_v20250909(). This means short treatment
  # interruptions (e.g., delayed prescription refills) are bridged before the
  # exposure logic below runs.
  #
  # PROCESSING STEPS (per approach x variant)
  # ------------------------------------------
  #
  # Step A — Derive "previous" status:
  #   Compare each row to its lag-1 value within each person. When a person
  #   transitions from any active MHT level to "local_or_none_mht", that
  #   transition row is marked "previous". Then the first "previous" week is
  #   found per person, and all subsequent "local_or_none_mht" rows are also
  #   set to "previous". This means once a person stops MHT, they stay
  #   "previous" for all future non-MHT weeks.
  #
  # Step B — Detect re-initiation:
  #   Find the first week where a person transitions from "previous" back to
  #   any active MHT level. This isoyearweek is stored as the re-initiation
  #   point.
  #
  # Step C — Single vs multiple variant:
  #   "single":   All rows from the re-initiation week onward are set to
  #               "exclude". A person can only have one episode of MHT use.
  #   "multiple": No exclusion. The person re-enters their new MHT level,
  #               allowing multiple episodes of MHT use.
  #
  # Step D — 3-year minimum duration check:
  #   For each "previous" row, check the length of the immediately preceding
  #   MHT episode. If that episode was < 3 years (156 weeks), the "previous"
  #   rows are reclassified to "exclude". This ensures that only sustained
  #   MHT use (>= 3 years) qualifies for "previous" status.
  #
  # Step E — Assign final exposure variable:
  #   The processed values are written to rd_approach{N}_{single,multiple}.
  #
  # Note: "clashingprescriptions" rows are treated as active treatment
  # throughout all steps — they are never reclassified to "previous" or
  # "exclude".
  # ---------------------------------------------------------------------------

  # Declare variables for data.table non-standard evaluation
  . <- NULL
  id <- isoyearweek <- var_to_clean <- var_to_clean_lag1 <- NULL
  isoyearweek_first_previous <- temp <- NULL
  reinitiation_isoyearweek <- NULL
  on_mht <- n <- length_on_mht <- last_session_on_mht <- NULL

  for (i in c("approach1", "approach2", "approach3")) {
    for (p in c("single", "multiple")) {
      final_var <- paste0("rd_", i, "_", p)
      skeleton[, var_to_clean := get(i)]

      # Derive "previous" status
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
      skeleton[, isoyearweek_first_previous := first_non_na(temp), by = .(id)]
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

      # Detect re-initiation
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
        reinitiation_isoyearweek := first_non_na(temp),
        by = .(id)
      ]
      skeleton[, temp := NULL]
      skeleton[
        is.na(reinitiation_isoyearweek),
        reinitiation_isoyearweek := "9999-99"
      ]
      skeleton[, var_to_clean_lag1 := NULL]

      # Apply single-variant exclusion
      if (p == "single") {
        skeleton[
          isoyearweek >= reinitiation_isoyearweek,
          var_to_clean := "exclude"
        ]
      }
      skeleton[, reinitiation_isoyearweek := NULL]

      # Apply 3-year minimum duration check
      skeleton[,
        on_mht := !var_to_clean %in%
          c("local_or_none_mht", "previous", "exclude")
      ]
      skeleton[, n := 1:.N, by = .(id, data.table::rleid(on_mht))]
      skeleton[, length_on_mht := .N, by = .(id, data.table::rleid(on_mht))]
      skeleton[on_mht == FALSE, length_on_mht := 0]
      skeleton[, last_session_on_mht := shift(length_on_mht), by = .(id)]
      skeleton[n != 1, last_session_on_mht := NA]
      skeleton[,
        last_session_on_mht := first_non_na(last_session_on_mht),
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

      # Assign final exposure variable
      skeleton[, (final_var) := var_to_clean]

      skeleton[, var_to_clean := NULL]
    }
  }

  # Approach 3b: collapse progesterone subtypes into single level
  for (p in c("single", "multiple")) {
    src <- paste0("rd_approach3_", p)
    dst <- paste0("rd_approach3b_", p)
    skeleton[, (dst) := get(src)]
    skeleton[
      get(dst) %in% c("estrogen_progesterone_bioidentical",
                       "estrogen_progesterone_synthetic"),
      (dst) := "estrogen_progesterone"
    ]
  }
}
