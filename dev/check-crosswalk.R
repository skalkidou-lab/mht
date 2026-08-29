#!/usr/bin/env Rscript
#
# Check the 2026-08-28 ladder against the 2026-08-28 codebook and against the
# decision table. Export MHT_DECISIONS_DIR to name the directory that holds it.
#
# Run it from the package root:
#
#     Rscript --vanilla dev/check-crosswalk.R
#
# The decisions CSV is the expected side. The script exits non-zero unless all
# of these hold:
#
#   1  Every decision appears exactly once.
#   2  Every non-NOT_MHT decision has exactly one codebook row of the same
#      atomic category.
#   3  The classifier returns the decided category for the exact register
#      spelling, and a NOT_MHT decision reaches no category that any approach
#      rule reads.
#   4  Every codebook product is reachable, or sits on EXEMPT_PRODUCTS.
#   5  Every category the ladder returns is materialised in
#      product_categories, or sits on DISCARDED_CATEGORIES.
#   6  Every prefix collision is declared, and the longer rung of each pair
#      sits first.
#   7  The whole test suite passes under NOT_CRAN=true.
#
# The script reads the ladder by parsing it, never by pattern matching over
# deparsed text. It reads every rung in source order, with its category, so it
# can decide the ordering question that check 6 asks.
#
# CHECK 3 SAYS "reads", NOT "reaches". A category changes exposure only when a
# post_grouping rule names it, in includes1, includes2 or a doesnotinclude
# column. H1 appears in no rule, so an H1 classification cannot move any
# approach variable. For a testosterone product, `H1` and `NOT MHT` are then
# the same answer. C3 appears in several rules, so a C3 classification does
# change exposure. read_categories() derives the set from post_grouping at
# run time. No category is named in this file.
#
# EVERY REGISTER BELOW IS SELF-INVALIDATING. An entry that no longer exempts
# anything is a hard error, so an entry cannot outlive its reason.

LADDER_FN <- "lmed_categorize_product_names_v20260828"
SKELETON_FN <- "apply_lmed_categories_to_skeleton_v20260828"
CODEBOOK <- "inst/2023-mht/dataDictionary20260828.xlsx"
DECISIONS_DIR_VAR <- "MHT_DECISIONS_DIR"
DECISIONS_FILE <- "decisions-2026-08-26.csv"

# The decision table lives outside this repository, so no directory of it is
# written here. Export MHT_DECISIONS_DIR before you run the script.
decisions_csv <- function() {
  root <- Sys.getenv(DECISIONS_DIR_VAR, unset = NA_character_)
  if (is.na(root) || !nzchar(root)) {
    stop(
      sprintf(
        "Set %s to the directory that holds %s.",
        DECISIONS_DIR_VAR,
        DECISIONS_FILE
      ),
      call. = FALSE
    )
  }
  return(file.path(root, DECISIONS_FILE))
}

# ================================================================ registers ====

# Check 4. A codebook row whose own Preparatnamn does not reach its own
# Subgrupp. Key is "Preparatnamn|Subgrupp".
EXEMPT_PRODUCTS <- c(
  "Presomen|B10" = paste(
    "The codebook carries two rows named Presomen, one A6 and one B10.",
    "The register tells them apart, and the codebook does not: the B10 row is",
    "the sequential product, dispensed as `Presomen 28 compositum`. The ladder",
    "keys B10 on that register spelling, so the bare codebook name reaches A6."
  ),
  "Endovelle|C3" = paste(
    "The codebook classifies Endovelle C3. The decision table gives NOT MHT.",
    "The decision table governs a clinical classification, so the ladder gives",
    "Endovelle no rung and the codebook keeps the row, unreachable. The Rules",
    "sheet of the same codebook already excludes Endovelle upstream, for too",
    "few users."
  )
)

# Check 3. A NOT_MHT decision whose exact register spelling reaches a category
# that an approach rule READS. Key is "produkt|category". Such an entry records
# a real disagreement between the codebook and the decision table, and it needs
# a resolution before the entry is removed.
#
# The register is empty today, and the staleness check below keeps it honest.
# An empty self-invalidating register is the place a future disagreement goes.
CONFLICTING_DECISIONS <- stats::setNames(character(0), character(0))

# Check 5. A category the ladder returns that product_categories leaves out.
DISCARDED_CATEGORIES <- c(
  G1 = paste(
    "G1 is Duavive alone. Duavive counts as no MHT.",
    "Classify it, then discard it: a category the person-week grid never",
    "materialises reaches no approach rule."
  )
)

# Check 5, the other direction. A product_categories entry the ladder never
# returns.
UNUSED_CATEGORIES <- c(
  D4 = paste(
    "The codebook defines no product with subgroup D4, and post_grouping names",
    "D4 in no rule. The column exists and is FALSE in every person-week."
  )
)

# Check 6. Every pair where one rung is a prefix of another. `long` MUST sit
# before `short`, or `short` answers for both products.
PREFIX_COLLISIONS <- data.frame(
  short = c(
    "estalis",
    "evorel",
    "femoston",
    "levosert",
    "premelle",
    "presomen",
    "progesteron",
    "undestor",
    "utrogest"
  ),
  long = c(
    "estalissekvens",
    "evorelmicronor",
    "femostonconti",
    "levosertone",
    "premellesekvens",
    "presomencompositum",
    "progesteronmicapl",
    "undestortestocaps",
    "utrogestan"
  ),
  stringsAsFactors = FALSE
)

# The findings this script records and does not act on. Each one carries a
# probe, so a finding that stops being true fails the script.
FINDINGS <- list(
  list(
    id = "Folistrel",
    text = paste(
      "The codebook spells this product Folistrel, with one l, and gives it",
      "D2. The register spells it Follistrel, with two. No register spelling",
      "reaches the rung. The rung stays, and the decision table gives NOT MHT",
      "for Follistrel, which is what the ladder already gives."
    ),
    probe = function(cl) {
      return(identical(cl("Folistrel"), "D2") && is.na(cl("Follistrel")))
    }
  ),
  list(
    id = "Cyclogest",
    text = paste(
      "Cyclogest still reaches C1, although its exclusion is settled. The",
      "exclusion happens upstream of the ladder, not inside it, so the rung",
      "stays."
    ),
    probe = function(cl) {
      return(identical(cl("Cyclogest"), "C1"))
    }
  ),
  list(
    id = "Gepretix",
    text = paste(
      "Gepretix reaches no category and carries no decision. The decision",
      "table does not name it."
    ),
    probe = function(cl) {
      return(is.na(cl("Gepretix")))
    }
  )
)

# ================================================================== readers ====

read_decisions <- function() {
  path <- decisions_csv()
  if (!file.exists(path)) {
    stop(sprintf("%s not found", path), call. = FALSE)
  }
  x <- utils::read.csv(path, stringsAsFactors = FALSE)
  need <- c("produkt", "decision_code")
  missing <- setdiff(need, names(x))
  if (length(missing) > 0) {
    stop(
      sprintf(
        "%s has no column %s",
        path,
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  return(x)
}

read_codebook <- function() {
  x <- suppressMessages(readxl::read_excel(
    CODEBOOK,
    sheet = "MHT_groups",
    col_types = "text"
  ))
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  for (j in seq_along(x)) {
    v <- as.character(x[[j]])
    v[is.na(v)] <- ""
    x[[j]] <- v
  }
  return(x[nzchar(x$Preparatnamn), c("Preparatnamn", "Subgrupp")])
}

read_post_grouping <- function() {
  x <- suppressMessages(readxl::read_excel(
    CODEBOOK,
    sheet = "post_grouping",
    col_types = "text"
  ))
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  for (j in seq_along(x)) {
    v <- as.character(x[[j]])
    v[is.na(v)] <- ""
    x[[j]] <- v
  }
  return(x)
}

# ============================================================== the ladder ====

call_name <- function(e) {
  h <- e[[1]]
  if (is.name(h)) {
    return(as.character(h))
  }
  if (is.call(h) && identical(as.character(h[[1]]), "::")) {
    return(as.character(h[[3]]))
  }
  return("")
}

# The one fcase() call of the ladder. Walk the parsed body, so a line break
# between the call and its arguments cannot hide it.
find_call <- function(e, name) {
  if (!is.call(e)) {
    return(NULL)
  }
  if (identical(call_name(e), name)) {
    return(e)
  }
  for (i in seq_along(e)) {
    if (identical(e[i], list(quote(expr = )))) {
      next
    }
    hit <- find_call(e[[i]], name)
    if (!is.null(hit)) {
      return(hit)
    }
  }
  return(NULL)
}

# Every rung, in source order, with its pattern and its category. The function
# also asserts the SHAPE of each rung, which is what pins the prefix match and
# the normalised pattern.
ladder_rungs <- function(fn) {
  e <- find_call(body(utils::removeSource(fn)), "fcase")
  if (is.null(e)) {
    stop(sprintf("%s holds no fcase() call", LADDER_FN), call. = FALSE)
  }
  args <- as.list(e)[-1]
  if (length(args) %% 2 != 0) {
    stop("the fcase() ladder has an odd number of arguments", call. = FALSE)
  }
  idx <- seq(1L, length(args), by = 2L)
  pattern <- rep(NA_character_, length(idx))
  category <- rep(NA_character_, length(idx))
  shape <- rep("", length(idx))
  for (k in seq_along(idx)) {
    cond <- args[[idx[k]]]
    value <- args[[idx[k] + 1L]]
    ok <- is.call(cond) &&
      identical(call_name(cond), "startsWith") &&
      length(cond) == 3L &&
      identical(cond[[2]], quote(produkt_clean)) &&
      is.character(cond[[3]])
    # Record the shape rather than stop(). An abort here would lose every
    # other check, and the classifier still has an answer to measure.
    if (!ok) {
      shape[k] <- sprintf(
        "rung %d is not startsWith(produkt_clean, \"<pattern>\"): %s",
        k,
        paste(deparse(cond), collapse = " ")
      )
    } else if (!grepl("^[a-z]+$", cond[[3]])) {
      shape[k] <- sprintf(
        "rung %d has a pattern that is not normalised: %s",
        k,
        cond[[3]]
      )
    } else {
      pattern[k] <- cond[[3]]
    }
    if (is.character(value)) {
      category[k] <- value
    } else {
      shape[k] <- sprintf("rung %d returns no literal category", k)
    }
  }
  return(data.frame(
    order = seq_along(pattern),
    pattern = pattern,
    category = category,
    shape = shape,
    stringsAsFactors = FALSE
  ))
}

# product_categories, read out of the function that builds the person-week
# grid. Parse the assignment and evaluate only that expression, so reading the
# vector needs no data and no join.
grid_categories <- function(fn) {
  e <- body(utils::removeSource(fn))
  out <- NULL
  walk <- function(node) {
    if (!is.call(node)) {
      return(invisible(NULL))
    }
    if (
      identical(call_name(node), "<-") &&
        identical(node[[2]], quote(product_categories))
    ) {
      out <<- eval(node[[3]])
    }
    for (i in seq_along(node)) {
      if (identical(node[i], list(quote(expr = )))) {
        next
      }
      walk(node[[i]])
    }
    return(invisible(NULL))
  }
  walk(e)
  if (is.null(out)) {
    stop(
      sprintf("%s assigns no product_categories", SKELETON_FN),
      call. = FALSE
    )
  }
  return(out)
}

# ================================================================= reporting ====

REPORT <- new.env(parent = emptyenv())
REPORT$ok <- TRUE

item <- function(id, ok, headline, detail = character(0)) {
  if (!ok) {
    REPORT$ok <- FALSE
  }
  cat(sprintf("  %-3s %s  %s\n", id, if (ok) "OK  " else "FAIL", headline))
  for (line in detail) {
    cat(sprintf("        %s\n", line))
  }
  return(invisible(ok))
}

wrapped <- function(prefix, text) {
  return(paste0(prefix, strwrap(text, width = 88, exdent = nchar(prefix))))
}

# =================================================================== checks ====

# One classifier call. A classifier error fails the check that asked for it. It
# does not abort the script. The script must still reach the test suite and name
# the test that failed.
classifier <- function(fn) {
  return(function(produkt) {
    got <- tryCatch(
      {
        x <- data.table::data.table(produkt = produkt)
        fn(x)
        x$product_category
      },
      error = function(e) {
        # Same length as the input, so every downstream comparison stays
        # element by element and the failure names the right product.
        return(rep(
          sprintf("<ERROR: %s>", conditionMessage(e)),
          length(produkt)
        ))
      }
    )
    return(got)
  })
}

check_1_decisions_unique <- function(csv) {
  dup <- unique(csv$produkt[duplicated(csv$produkt)])
  n_not <- sum(csv$decision_code == "NOT_MHT")
  item(
    "1",
    length(dup) == 0L,
    sprintf(
      "%d decisions, %d distinct products, %d NOT_MHT, %d with a category",
      nrow(csv),
      length(unique(csv$produkt)),
      n_not,
      nrow(csv) - n_not
    ),
    if (length(dup) == 0L) {
      character(0)
    } else {
      sprintf("appears more than once: %s", paste(dup, collapse = ", "))
    }
  )
  return(invisible(NULL))
}

check_2_decision_has_codebook_row <- function(csv, wb) {
  keep <- csv[csv$decision_code != "NOT_MHT", ]
  ok <- TRUE
  detail <- character(0)
  for (i in seq_len(nrow(keep))) {
    hit <- which(wb$Preparatnamn == keep$produkt[i])
    sub <- unique(wb$Subgrupp[hit])
    good <- length(hit) == 1L && identical(sub, keep$decision_code[i])
    if (!good) {
      ok <- FALSE
    }
    detail <- c(
      detail,
      sprintf(
        "%-14s decision=%-4s codebook rows=%d subgroup=%s%s",
        keep$produkt[i],
        keep$decision_code[i],
        length(hit),
        paste(sub, collapse = "/"),
        if (good) "" else "   MISMATCH"
      )
    )
  }
  # An atomic category is one letter and one number. The combination rows of
  # the codebook carry a "+" and name no product.
  atomic <- grepl("^[A-Z][0-9]+$", keep$decision_code)
  if (!all(atomic)) {
    ok <- FALSE
    detail <- c(
      detail,
      sprintf(
        "not an atomic category: %s",
        paste(keep$decision_code[!atomic], collapse = ", ")
      )
    )
  }
  item(
    "2",
    ok,
    sprintf("%d decisions with a category, one codebook row each", nrow(keep)),
    detail
  )
  return(invisible(NULL))
}

# Every category an approach rule READS. post_grouping names a category in
# includes1, includes2 or a doesnotinclude column, and a rule that names it can
# move an approach variable. A category no rule names cannot.
#
# includes1 and includes2 hold a space-separated pair on the combination rows,
# so split on anything that is not a letter or a digit.
read_categories <- function(pg) {
  cols <- grep("^(includes|doesnotinclude)", names(pg), value = TRUE)
  v <- unique(unlist(pg[, cols], use.names = FALSE))
  v <- unlist(strsplit(v[nzchar(v)], "[^A-Za-z0-9]+"))
  return(sort(unique(v[nzchar(v)])))
}

# Check 3, in two halves.
#
#   A decision WITH a category: the classifier MUST return that category for
#   the exact register spelling. No exemption.
#
#   A NOT_MHT decision: the classifier MUST NOT return a category that
#   read_categories() names. A category no rule reads is permitted, because it
#   cannot change exposure. That case is reported, never waved through in
#   silence.
check_3_classifier_matches_decisions <- function(csv, cl, pg) {
  read <- read_categories(pg)
  got <- cl(csv$produkt)
  not_mht <- csv$decision_code == "NOT_MHT"
  want <- ifelse(not_mht, NA_character_, csv$decision_code)

  named <- !is.na(got)
  # The two ways a decision and the classifier can disagree.
  wrong_category <- !not_mht & (!named | got != want)
  reaches_read <- not_mht & named & got %in% read
  # Permitted, and reported: a NOT_MHT decision reaching an unread category.
  reaches_unread <- not_mht & named & !(got %in% read)

  key <- paste0(csv$produkt, "|", got, recycle0 = TRUE)
  declared <- reaches_read & key %in% names(CONFLICTING_DECISIONS)
  bad <- which(wrong_category | (reaches_read & !declared))
  stale <- setdiff(names(CONFLICTING_DECISIONS), key[reaches_read])
  ok <- length(bad) == 0L && length(stale) == 0L

  detail <- character(0)
  for (i in bad) {
    detail <- c(
      detail,
      sprintf(
        "%-30s decision=%-8s classifier=%-4s %s",
        csv$produkt[i],
        csv$decision_code[i],
        if (is.na(got[i])) "NA" else got[i],
        if (not_mht[i]) {
          "an approach rule READS that category, UNDECLARED"
        } else {
          "the classifier does not return the decided category"
        }
      )
    )
  }
  for (k in stale) {
    detail <- c(
      detail,
      sprintf("CONFLICTING_DECISIONS entry '%s' names no live disagreement", k)
    )
  }
  for (i in which(declared)) {
    detail <- c(
      detail,
      sprintf(
        "DISAGREEMENT, declared: %s reaches %s, which an approach rule reads",
        csv$produkt[i],
        got[i]
      ),
      wrapped("    ", CONFLICTING_DECISIONS[[key[i]]])
    )
  }
  for (i in which(reaches_unread)) {
    detail <- c(
      detail,
      sprintf(
        "PERMITTED: %s answered NOT MHT and reaches %s, which no approach rule reads",
        csv$produkt[i],
        got[i]
      )
    )
  }
  detail <- c(
    detail,
    sprintf(
      "post_grouping reads %d categories: %s",
      length(read),
      paste(read, collapse = ", ")
    )
  )
  item(
    "3",
    ok,
    sprintf(
      "%d decisions: %d with a category, %d NOT_MHT reaching nothing, %d NOT_MHT reaching an unread category, %d declared disagreements",
      nrow(csv),
      sum(!not_mht),
      sum(not_mht & !named),
      sum(reaches_unread),
      sum(declared)
    ),
    detail
  )
  return(invisible(NULL))
}

check_4_codebook_reachable <- function(wb, cl) {
  got <- cl(wb$Preparatnamn)
  agree <- !is.na(got) & got == wb$Subgrupp
  key <- paste0(wb$Preparatnamn, "|", wb$Subgrupp, recycle0 = TRUE)
  exempt <- key %in% names(EXEMPT_PRODUCTS)
  bad <- which(!agree & !exempt)
  stale <- setdiff(names(EXEMPT_PRODUCTS), key[!agree])
  ok <- length(bad) == 0L && length(stale) == 0L
  detail <- character(0)
  for (i in bad) {
    detail <- c(
      detail,
      sprintf(
        "%-24s codebook=%-4s classifier=%s   NOT EXEMPT",
        wb$Preparatnamn[i],
        wb$Subgrupp[i],
        if (is.na(got[i])) "NA" else got[i]
      )
    )
  }
  for (k in stale) {
    detail <- c(
      detail,
      sprintf(
        "EXEMPT_PRODUCTS entry '%s' exempts nothing: the row is reachable",
        k
      )
    )
  }
  for (i in which(exempt & !agree)) {
    detail <- c(
      detail,
      sprintf(
        "EXEMPT: %s wants %s and reaches %s",
        wb$Preparatnamn[i],
        wb$Subgrupp[i],
        if (is.na(got[i])) "NA" else got[i]
      ),
      wrapped("    ", EXEMPT_PRODUCTS[[key[i]]])
    )
  }
  item(
    "4",
    ok,
    sprintf(
      "%d codebook products, %d reach their own subgroup, %d exempt",
      nrow(wb),
      sum(agree),
      sum(exempt & !agree)
    ),
    detail
  )
  return(invisible(NULL))
}

check_5_categories_materialised <- function(rungs, grid, pg) {
  produced <- sort(unique(rungs$category))
  missing <- setdiff(produced, grid)
  extra <- setdiff(grid, produced)
  bad_missing <- setdiff(missing, names(DISCARDED_CATEGORIES))
  bad_extra <- setdiff(extra, names(UNUSED_CATEGORIES))
  stale_discard <- setdiff(names(DISCARDED_CATEGORIES), missing)
  stale_unused <- setdiff(names(UNUSED_CATEGORIES), extra)
  ok <- length(bad_missing) == 0L &&
    length(bad_extra) == 0L &&
    length(stale_discard) == 0L &&
    length(stale_unused) == 0L
  detail <- character(0)
  for (k in bad_missing) {
    detail <- c(
      detail,
      sprintf("%s: the ladder returns it, the grid drops it, UNDECLARED", k)
    )
  }
  for (k in bad_extra) {
    detail <- c(
      detail,
      sprintf(
        "%s: the grid materialises it, the ladder never returns it, UNDECLARED",
        k
      )
    )
  }
  for (k in stale_discard) {
    detail <- c(
      detail,
      sprintf("DISCARDED_CATEGORIES entry '%s' discards nothing", k)
    )
  }
  for (k in stale_unused) {
    detail <- c(
      detail,
      sprintf(
        "UNUSED_CATEGORIES entry '%s' names nothing: the grid must materialise it and the ladder must never return it",
        k
      )
    )
  }
  for (k in intersect(missing, names(DISCARDED_CATEGORIES))) {
    detail <- c(
      detail,
      sprintf("DISCARDED: %s", k),
      wrapped("    ", DISCARDED_CATEGORIES[[k]])
    )
  }
  for (k in intersect(extra, names(UNUSED_CATEGORIES))) {
    detail <- c(
      detail,
      sprintf("UNUSED: %s", k),
      wrapped("    ", UNUSED_CATEGORIES[[k]])
    )
  }
  # The other reader of a category is post_grouping. read_categories() is the
  # one implementation of that set, and check 3 uses the same one.
  used <- read_categories(pg)
  detail <- c(
    detail,
    sprintf(
      "post_grouping reads %d categories; the grid materialises %d that no rule reads: %s",
      length(used),
      length(setdiff(grid, used)),
      paste(setdiff(grid, used), collapse = ", ")
    )
  )
  item(
    "5",
    ok,
    sprintf(
      "the ladder returns %d categories, the grid materialises %d",
      length(produced),
      length(grid)
    ),
    detail
  )
  return(invisible(NULL))
}

check_6_prefix_collisions <- function(rungs) {
  found <- NULL
  readable <- which(!is.na(rungs$pattern))
  for (i in readable) {
    for (j in readable) {
      a <- rungs$pattern[i]
      b <- rungs$pattern[j]
      if (i == j || nchar(a) >= nchar(b) || !startsWith(b, a)) {
        next
      }
      found <- rbind(
        found,
        data.frame(
          short = a,
          long = b,
          short_at = rungs$order[i],
          long_at = rungs$order[j],
          short_cat = rungs$category[i],
          long_cat = rungs$category[j],
          stringsAsFactors = FALSE
        )
      )
    }
  }
  if (is.null(found)) {
    found <- data.frame(
      short = character(),
      long = character(),
      short_at = integer(),
      long_at = integer(),
      short_cat = character(),
      long_cat = character(),
      stringsAsFactors = FALSE
    )
  }
  key_found <- paste0(found$short, "|", found$long, recycle0 = TRUE)
  key_want <- paste0(
    PREFIX_COLLISIONS$short,
    "|",
    PREFIX_COLLISIONS$long,
    recycle0 = TRUE
  )
  undeclared <- setdiff(key_found, key_want)
  stale <- setdiff(key_want, key_found)
  shadowed <- found[found$long_at > found$short_at, , drop = FALSE]
  wrong_shape <- rungs$shape[nzchar(rungs$shape)]
  duplicated_pattern <- unique(rungs$pattern[duplicated(rungs$pattern)])
  duplicated_pattern <- duplicated_pattern[!is.na(duplicated_pattern)]
  ok <- length(undeclared) == 0L &&
    length(stale) == 0L &&
    nrow(shadowed) == 0L &&
    length(wrong_shape) == 0L &&
    length(duplicated_pattern) == 0L
  detail <- wrong_shape
  for (k in duplicated_pattern) {
    detail <- c(
      detail,
      sprintf("pattern '%s' appears on more than one rung", k)
    )
  }
  for (i in seq_len(nrow(found))) {
    detail <- c(
      detail,
      sprintf(
        "%-20s (%-3s) at %3d  extends  %-14s (%-3s) at %3d   %s%s",
        found$long[i],
        found$long_cat[i],
        found$long_at[i],
        found$short[i],
        found$short_cat[i],
        found$short_at[i],
        if (found$long_at[i] < found$short_at[i]) {
          "specific first"
        } else {
          "SHADOWED"
        },
        if (found$long_cat[i] == found$short_cat[i]) {
          ""
        } else {
          ", different category"
        }
      )
    )
  }
  for (k in undeclared) {
    detail <- c(detail, sprintf("PREFIX_COLLISIONS does not declare '%s'", k))
  }
  for (k in stale) {
    detail <- c(
      detail,
      sprintf("PREFIX_COLLISIONS entry '%s' names no collision", k)
    )
  }
  item(
    "6",
    ok,
    sprintf(
      "%d rungs, %d prefix collisions, %d shadowed",
      nrow(rungs),
      nrow(found),
      nrow(shadowed)
    ),
    detail
  )
  return(invisible(NULL))
}

report_findings <- function(cl) {
  cat("\nFINDINGS, recorded and not acted on\n")
  for (f in FINDINGS) {
    good <- isTRUE(f$probe(cl))
    if (!good) {
      REPORT$ok <- FALSE
    }
    cat(sprintf("  %-10s %s\n", f$id, if (good) "still true" else "STALE"))
    cat(paste0(wrapped("        ", f$text), collapse = "\n"), "\n", sep = "")
  }
  return(invisible(NULL))
}

show_rungs <- function(rungs) {
  cat(sprintf("\nTHE LADDER, %d rungs in source order\n", nrow(rungs)))
  for (i in seq_len(nrow(rungs))) {
    cat(sprintf(
      "  %3d  %-32s %s\n",
      rungs$order[i],
      rungs$pattern[i],
      rungs$category[i]
    ))
  }
  return(invisible(NULL))
}

# Run the package test suite with NOT_CRAN set, and fail the script when any
# test fails. The gate is necessary: a mutation of the four-week bridge leaves
# every codebook check reporting OK while four tests fail.
run_suite <- function() {
  cat("\nTEST SUITE, NOT_CRAN=true\n")
  before <- Sys.getenv("NOT_CRAN", unset = NA_character_)
  Sys.setenv(NOT_CRAN = "true")
  on.exit(
    if (is.na(before)) {
      Sys.unsetenv("NOT_CRAN")
    } else {
      Sys.setenv(NOT_CRAN = before)
    },
    add = TRUE
  )
  res <- as.data.frame(testthat::test_local(
    reporter = "silent",
    stop_on_failure = FALSE
  ))
  n_fail <- sum(res$failed) + sum(res$error)
  cat(sprintf(
    "  %s  %d files, %d passed, %d failed, %d errored, %d warnings, %d skipped\n",
    if (n_fail == 0L) "OK  " else "FAIL",
    length(unique(res$file)),
    sum(res$passed),
    sum(res$failed),
    sum(res$error),
    sum(res$warning),
    sum(res$skipped)
  ))
  if (n_fail > 0L) {
    for (i in which(res$failed > 0L | res$error > 0L)) {
      cat(sprintf("    %s :: %s\n", res$file[i], res$test[i]))
    }
  }
  return(n_fail == 0L)
}

# ==================================================================== main ====

main <- function() {
  for (path in c(CODEBOOK, decisions_csv())) {
    if (!file.exists(path)) {
      stop(
        sprintf("%s not found. Run this from the package root.", path),
        call. = FALSE
      )
    }
  }
  ns <- asNamespace("mht")
  fn <- get(LADDER_FN, envir = ns)
  cl <- classifier(fn)
  csv <- read_decisions()
  wb <- read_codebook()
  pg <- read_post_grouping()
  rungs <- ladder_rungs(fn)
  grid <- grid_categories(get(SKELETON_FN, envir = ns))

  cat("SOURCES\n")
  cat(sprintf("  ladder    %s\n", LADDER_FN))
  cat(sprintf("  grid      %s\n", SKELETON_FN))
  cat(sprintf("  codebook  %s\n", CODEBOOK))
  cat(sprintf("  decisions $%s/%s\n", DECISIONS_DIR_VAR, DECISIONS_FILE))

  cat("\nCHECKS\n")
  check_1_decisions_unique(csv)
  check_2_decision_has_codebook_row(csv, wb)
  check_3_classifier_matches_decisions(csv, cl, pg)
  check_4_codebook_reachable(wb, cl)
  check_5_categories_materialised(rungs, grid, pg)
  check_6_prefix_collisions(rungs)

  report_findings(cl)
  show_rungs(rungs)
  if (!run_suite()) {
    REPORT$ok <- FALSE
  }
  cat("\n", if (REPORT$ok) "PASS" else "FAIL", "\n", sep = "")
  return(invisible(REPORT$ok))
}

suppressMessages(pkgload::load_all(".", quiet = TRUE, helpers = FALSE))
if (!isTRUE(main())) {
  quit(status = 1L)
}
quit(status = 0L)
