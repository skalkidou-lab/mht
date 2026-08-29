#!/usr/bin/env Rscript
#
# Check dataDictionary20260828.xlsx against dataDictionary20241105.xlsx.
#
# Run it from the package root:
#
#     Rscript --vanilla dev/diff-codebooks.R
#
# The new codebook MUST equal the old one on every sheet and every cell, except
# for the delta this file enumerates. The delta is items a to j of the phase 2
# brief, plus item h2. MANIFEST below carries one entry per changed cell, one
# entry per cell of an added row, and one entry per cell of a removed row. No
# entry stands for a whole row.
#
# The check works by reconstruction, not by pattern matching. The script builds
# the sheet it expects from the old sheet and the manifest, then compares that
# to the new sheet with identical(). Reconstruction catches a column
# reordering, a row reordering and an unapproved cell, because all three break
# identical().
#
# The script also verifies that both frozen codebooks are untouched. A dated
# codebook is immutable, so an edit to either one is a hard failure.
#
# NOTE ON readxl WARNINGS. An earlier version escalated any readxl warning to
# an error. That assertion could not fire. Under col_types = "text" readxl has
# no reachable warning branch: ten deliberate malformations of the archive
# produced six clean reads and four hard errors, and no warning at all. The
# script now asserts that every sheet reads WITHOUT AN ERROR, which is the
# branch that a corrupt part does reach.

OLD_PATH <- "inst/2023-mht/dataDictionary20241105.xlsx"
NEW_PATH <- "inst/2023-mht/dataDictionary20260828.xlsx"
FROZEN_PATHS <- c(
  "inst/2023-mht/dataDictionary20241105.xlsx",
  "inst/2023-mht/dataDictionary20260803.xlsx"
)
DECISIONS_CSV <- file.path(
  "/home/raw996/skalkidou/structural-mht-registry-data",
  "coauthor-questions",
  "decisions-2026-08-26.csv"
)

# The columns that name a row for a human reader, per sheet. For MHT_groups the
# two strength columns are part of the key, because they are what tells the two
# Utrogestan rows apart.
KEY_COLS <- list(
  MHT_groups = c(
    "Preparatnamn",
    "Subgrupp",
    "minimum_monthly_dose",
    "minimum_months",
    "strength_mg_min",
    "strength_mg_max"
  ),
  post_grouping = c("variable", "approach", "includes1", "includes2")
)

# ============================================================ THE MANIFEST ====

# Item f. The only approved column change, on the only sheet that gets one.
# Both columns MUST be appended, in this order, after every existing column.
APPENDED_COLUMNS <- list(
  MHT_groups = c("strength_mg_min", "strength_mg_max")
)

# One row of MANIFEST is one cell.
#
#   item   the brief's item letter
#   sheet  the sheet it lives on
#   kind   "cell", "add", "remove" or "noop"
#   row    for "cell", "remove" and "noop", the row index in the OLD sheet
#          for "add", the OLD row index the new row follows
#   unit   orders several rows added after the same old row
#   key    the row key, asserted against the workbook
#   column the column name
#   old    the value before, "" for "add"
#   new    the value after, "" for "remove"
#
# A "noop" entry asserts that a cell holds the same value in BOTH codebooks and
# that nothing changed it. `old` and `new` MUST be equal. It records a decision
# NOT to act, so that the absence of a change cannot be read as an omission.
mf <- function(item, sheet, kind, row, unit, key, column, old, new) {
  return(data.frame(
    item = item,
    sheet = sheet,
    kind = kind,
    row = as.integer(row),
    unit = as.integer(unit),
    key = key,
    column = column,
    old = old,
    new = new,
    stringsAsFactors = FALSE
  ))
}

# A whole added or removed row, spelled out one cell at a time. `cells` is a
# named character vector; every column it does not name MUST be empty.
mf_row <- function(item, sheet, kind, row, unit, key, cells) {
  out <- NULL
  for (col in names(cells)) {
    value <- unname(cells[[col]])
    out <- rbind(
      out,
      mf(
        item,
        sheet,
        kind,
        row,
        unit,
        key,
        col,
        if (kind == "remove") value else "",
        if (kind == "remove") "" else value
      )
    )
  }
  return(out)
}

# One no-op record: several cells of one row, all unchanged.
mf_noop <- function(item, sheet, row, key, cells) {
  out <- NULL
  for (col in names(cells)) {
    value <- unname(cells[[col]])
    out <- rbind(out, mf(item, sheet, "noop", row, 1, key, col, value, value))
  }
  return(out)
}

# The Swedish controlled vocabulary the sheet already uses, and the two block
# labels every new row repeats.
EST <- "Estrogener"
GES <- "Gestagener"
GEP <- "Gestagener i kombination med estrogener"

# Item a and item c. Every new MHT_groups row takes its Administrationssatt
# from the neighbouring rows of its own Subgrupp, and carries the CSV's
# register_form verbatim in Comment.
row_estramon <- function(name) {
  return(c(
    "Grupp" = "A",
    "ATC-kod" = "G03C",
    "Specifikation" = EST,
    "Detaljerad kod" = "G03CA03",
    "Administrationssätt" = "Transdermalt",
    "Preparatnamn" = name,
    "Subgrupp" = "A1",
    "Comment" = "patch"
  ))
}

row_a4 <- function(name, comment) {
  return(c(
    "Grupp" = "A",
    "Specifikation" = EST,
    "Administrationssätt" = "Vaginalt (lokal effekt)",
    "Preparatnamn" = name,
    "Subgrupp" = "A4",
    "Comment" = comment
  ))
}

row_b1 <- function(name, comment) {
  out <- c(
    "Grupp" = "B",
    "ATC-kod" = "G03F",
    "Specifikation" = GEP,
    "Detaljerad kod" = "G03FA01",
    "Administrationssätt" = "Transdermalt",
    "Preparatnamn" = name,
    "Subgrupp" = "B1"
  )
  if (nzchar(comment)) {
    out <- c(out, "Comment" = comment)
  }
  return(out)
}

row_pg <- function(approach) {
  return(c("variable" = "tibolone", "approach" = approach, "includes1" = "F1"))
}

MANIFEST <- rbind(
  # -- item a: eight products from the 2026-08-26 decisions -------------------
  # Lafamme is the ninth non-NOT_MHT decision. It already has a row, so it is
  # a "noop" entry further down, not an "add".
  mf_row(
    "a",
    "MHT_groups",
    "add",
    10,
    1,
    "Estramon 100|A1||||",
    row_estramon("Estramon 100")
  ),
  mf_row(
    "a",
    "MHT_groups",
    "add",
    10,
    2,
    "Estramon 75|A1||||",
    row_estramon("Estramon 75")
  ),
  mf_row(
    "a",
    "MHT_groups",
    "add",
    10,
    3,
    "Estramon 25|A1||||",
    row_estramon("Estramon 25")
  ),
  mf_row(
    "a",
    "MHT_groups",
    "add",
    22,
    1,
    "Intrarosa|A4||||",
    row_a4("Intrarosa", "Prasterone, vaginal pessary")
  ),
  mf_row(
    "a",
    "MHT_groups",
    "add",
    22,
    2,
    "Gynoflor|A4||||",
    row_a4("Gynoflor", "Oestriol with lactobacilli, vaginal tablet")
  ),
  mf_row(
    "a",
    "MHT_groups",
    "add",
    25,
    1,
    "Climopax Mono|A6||||",
    c(
      "Grupp" = "A",
      "ATC-kod" = "G03C",
      "Specifikation" = EST,
      "Detaljerad kod" = "G03CA57",
      "Administrationssätt" = "Peroralt",
      "Preparatnamn" = "Climopax Mono",
      "Subgrupp" = "A6",
      "Comment" = "tablet"
    )
  ),
  mf_row(
    "a",
    "MHT_groups",
    "add",
    29,
    1,
    "CombiPatch|B1||||",
    row_b1("CombiPatch", "patch")
  ),
  mf_row(
    "a",
    "MHT_groups",
    "add",
    62,
    1,
    "Dienosis|C3||||",
    c(
      "Grupp" = "C",
      "ATC-kod" = "G03D",
      "Specifikation" = GES,
      "Detaljerad kod" = "G03DB08",
      "Administrationssätt" = "Peroral",
      "Preparatnamn" = "Dienosis",
      "Subgrupp" = "C3",
      "Comment" = "tablet"
    )
  ),

  # -- item c: Evorel Micronor reaches B1, not A1 -----------------------------
  mf_row(
    "c",
    "MHT_groups",
    "add",
    29,
    2,
    "Evorel Micronor|B1||||",
    row_b1("Evorel Micronor", "")
  ),

  # -- item e: the one unreachable product whose WORKBOOK spelling is wrong ---
  mf(
    "e",
    "MHT_groups",
    "cell",
    59,
    1,
    "Extempore progesteron|C1||||",
    "Preparatnamn",
    "Extrempore progesteron",
    "Extempore progesteron"
  ),

  # -- item g: the Utrogestan family becomes strength-keyed -------------------
  mf_row(
    "g",
    "MHT_groups",
    "remove",
    57,
    1,
    "Utrogest|C1|8|3||",
    c(
      "Grupp" = "C",
      "ATC-kod" = "G03D",
      "Specifikation" = GES,
      "Detaljerad kod" = "G03DA08",
      "Administrationssätt" = "Vaginal progesteron",
      "Preparatnamn" = "Utrogest",
      "Subgrupp" = "C1",
      "minimum_monthly_dose" = "8",
      "minimum_months" = "3"
    )
  ),
  mf_row(
    "g",
    "MHT_groups",
    "add",
    56,
    1,
    "Utrogestan|C1|28|1||150",
    c(
      "Grupp" = "C",
      "ATC-kod" = "G03D",
      "Specifikation" = GES,
      "Detaljerad kod" = "G03DA09",
      "Administrationssätt" = "Vaginal progesteron",
      "Preparatnamn" = "Utrogestan",
      "Subgrupp" = "C1",
      "minimum_monthly_dose" = "28",
      "minimum_months" = "1",
      "strength_mg_max" = "150"
    )
  ),
  mf(
    "g",
    "MHT_groups",
    "cell",
    58,
    1,
    "Utrogestan|C1|12|1|150|",
    "strength_mg_min",
    "",
    "150"
  ),

  # -- item h: C5 joins the doesnotinclude list of estrogen_only, approach 3 --
  mf(
    "h",
    "post_grouping",
    "cell",
    42,
    1,
    "estrogen_only|3|A1|",
    "doesnotinclude23",
    "",
    "C5"
  ),
  mf(
    "h",
    "post_grouping",
    "cell",
    43,
    1,
    "estrogen_only|3|A2|",
    "doesnotinclude23",
    "",
    "C5"
  ),
  mf(
    "h",
    "post_grouping",
    "cell",
    44,
    1,
    "estrogen_only|3|A6|",
    "doesnotinclude23",
    "",
    "C5"
  ),
  mf(
    "h",
    "post_grouping",
    "cell",
    45,
    1,
    "estrogen_only|3|A7|",
    "doesnotinclude23",
    "",
    "C5"
  ),

  # -- item h2: the same missing C5 in local_or_none_mht, approach 2 ----------
  mf(
    "h2",
    "post_grouping",
    "cell",
    38,
    1,
    "local_or_none_mht|2|A3|",
    "doesnotinclude23",
    "",
    "C5"
  ),
  mf(
    "h2",
    "post_grouping",
    "cell",
    39,
    1,
    "local_or_none_mht|2|A4|",
    "doesnotinclude23",
    "",
    "C5"
  ),
  mf(
    "h2",
    "post_grouping",
    "cell",
    40,
    1,
    "local_or_none_mht|2|A5|",
    "doesnotinclude23",
    "",
    "C5"
  ),

  # -- item i: a tibolone variable in approaches 1, 2 and 3 -------------------
  mf_row("i", "post_grouping", "add", 19, 1, "tibolone|1|F1|", row_pg("1")),
  mf_row("i", "post_grouping", "add", 40, 1, "tibolone|2|F1|", row_pg("2")),
  mf_row("i", "post_grouping", "add", 101, 1, "tibolone|3|F1|", row_pg("3")),

  # -- item j: the A2 peroral rule appeared twice in approach 2 ---------------
  # Old rows 21 and 22 are identical. The manifest removes row 22.
  mf_row(
    "j",
    "post_grouping",
    "remove",
    22,
    1,
    "peroral_estrogen|2|A2|",
    c(
      "variable" = "peroral_estrogen",
      "approach" = "2",
      "includes1" = "A2"
    )
  ),

  # -- item a, the ninth decision: Lafamme is present already -----------------
  # Present already, unchanged, no action. Every field below is asserted, and
  # the whole row is asserted byte-identical between the two codebooks.
  mf_noop(
    "a",
    "MHT_groups",
    41,
    "Lafamme|B5||||",
    c(
      "Subgrupp" = "B5",
      "Detaljerad kod" = "G03FA15",
      "Administrationssätt" = "Peroralt"
    )
  ),

  # -- item b: the three subgroups the workbook already gets right ------------
  # No action. The divergence is in the ladder, which phase 3 owns.
  mf_noop("b", "MHT_groups", 76, "Duphaston|C5|12|1||", c("Subgrupp" = "C5")),
  mf_noop("b", "MHT_groups", 52, "Cyclabil|B12||||", c("Subgrupp" = "B12")),
  mf_noop("b", "MHT_groups", 61, "Prolutex|C2||||", c("Subgrupp" = "C2"))
)

LAFAMME_PRODUKT <- "Lafamme"

# Item a, the decision table, transcribed from decisions-2026-08-26.csv. The
# script asserts the workbook against THIS literal, and cross-checks the
# literal against the CSV when the CSV is reachable.
DECISIONS <- data.frame(
  produkt = c(
    "Intrarosa",
    "Gynoflor",
    "Estramon 100",
    "Lafamme",
    "Dienosis",
    "Climopax Mono",
    "Estramon 75",
    "Estramon 25",
    "CombiPatch"
  ),
  decision_code = c(
    "A4",
    "A4",
    "A1",
    "B5",
    "C3",
    "A6",
    "A1",
    "A1",
    "B1"
  ),
  register_form = c(
    "Prasterone, vaginal pessary",
    "Oestriol with lactobacilli, vaginal tablet",
    "patch",
    "tablet",
    "tablet",
    "tablet",
    "patch",
    "patch",
    "patch"
  ),
  stringsAsFactors = FALSE
)
DECISIONS_NOT_MHT_N <- 37L

# Item b. What the ladder returns for those three products TODAY. The workbook
# side of item b lives in MANIFEST as "noop" entries; this is the other side of
# the divergence, and it is a claim about the classifier, not about a cell.
LADDER_TODAY <- data.frame(
  produkt = c("Duphaston", "Cyclabil", "Prolutex"),
  ladder = c("C4", "B11", "C1"),
  stringsAsFactors = FALSE
)

# Item e. The six products the codebook names that reach no category, and the
# category each reaches once the item e typo is corrected.
UNREACHED_E <- data.frame(
  produkt = c(
    "Lafamme",
    "Extrempore progesteron",
    "Endovelle",
    "Primolut-Nor",
    "Testosteron depot",
    "Mini-Pe"
  ),
  after = c(NA, "C1", NA, NA, NA, NA),
  owner = c(
    "ladder",
    "workbook typo, fixed here",
    "ladder",
    "ladder",
    "ladder",
    "ladder"
  ),
  stringsAsFactors = FALSE
)

# ================================================================= readers ====

read_sheet <- function(path, sheet) {
  x <- tryCatch(
    suppressMessages(readxl::read_excel(
      path,
      sheet = sheet,
      col_types = "text"
    )),
    # The one function in this file with no explicit return(). stop() never
    # returns, so a return() after it is unreachable, and lintr's explicit
    # return style accepts a terminal stop().
    error = function(e) {
      stop(
        sprintf(
          "readxl could not read %s sheet %s: %s",
          path,
          sheet,
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  for (j in seq_along(x)) {
    v <- as.character(x[[j]])
    v[is.na(v)] <- ""
    x[[j]] <- v
  }
  rownames(x) <- NULL
  return(x)
}

row_key <- function(sheet, x, i) {
  cols <- KEY_COLS[[sheet]]
  if (is.null(cols)) {
    return(as.character(i))
  }
  parts <- vapply(
    cols,
    function(col) return(if (is.null(x[[col]])) "" else x[[col]][i]),
    character(1)
  )
  return(paste(parts, collapse = "|"))
}

# ======================================================= the reconstruction ====

# Add the approved columns to the old sheet, empty, in the approved order.
widen <- function(sheet, old) {
  for (col in APPENDED_COLUMNS[[sheet]]) {
    old[[col]] <- rep("", nrow(old))
  }
  return(old)
}

apply_cells <- function(sheet, x, fails) {
  m <- MANIFEST[MANIFEST$sheet == sheet & MANIFEST$kind == "cell", ]
  for (i in seq_len(nrow(m))) {
    r <- m$row[i]
    got <- x[[m$column[i]]][r]
    if (!identical(got, m$old[i])) {
      fails <- c(
        fails,
        sprintf(
          "item %s: %s row %d column %s held '%s', the manifest says '%s'",
          m$item[i],
          sheet,
          r,
          m$column[i],
          got,
          m$old[i]
        )
      )
    }
    x[[m$column[i]]][r] <- m$new[i]
  }
  return(list(x = x, fails = fails))
}

# Assert that the old row about to be deleted is exactly what the manifest says
# it is, in every column.
check_removals <- function(sheet, x, fails) {
  m <- MANIFEST[MANIFEST$sheet == sheet & MANIFEST$kind == "remove", ]
  for (r in unique(m$row)) {
    part <- m[m$row == r, ]
    want <- rep("", ncol(x))
    names(want) <- names(x)
    want[part$column] <- part$old
    got <- vapply(names(x), function(col) return(x[[col]][r]), character(1))
    if (!identical(unname(got), unname(want))) {
      fails <- c(
        fails,
        sprintf(
          "item %s: %s row %d is not the row the manifest removes",
          part$item[1],
          sheet,
          r
        )
      )
    }
    key <- row_key(sheet, x, r)
    if (!identical(key, part$key[1])) {
      fails <- c(
        fails,
        sprintf(
          "item %s: %s row %d has key '%s', the manifest says '%s'",
          part$item[1],
          sheet,
          r,
          key,
          part$key[1]
        )
      )
    }
  }
  return(fails)
}

# Build one added row as a one-row data frame shaped like x.
build_added <- function(x, part) {
  out <- x[1, , drop = FALSE]
  for (col in names(out)) {
    out[[col]] <- ""
  }
  for (i in seq_len(nrow(part))) {
    out[[part$column[i]]] <- part$new[i]
  }
  return(out)
}

# Walk the old rows in order. Drop the removed ones, and insert the added ones
# after their anchor. This is what makes a row reordering fail.
assemble <- function(sheet, x) {
  adds <- MANIFEST[MANIFEST$sheet == sheet & MANIFEST$kind == "add", ]
  drops <- unique(MANIFEST$row[
    MANIFEST$sheet == sheet & MANIFEST$kind == "remove"
  ])
  pieces <- list()
  for (i in c(0L, seq_len(nrow(x)))) {
    if (i > 0L && !(i %in% drops)) {
      pieces[[length(pieces) + 1L]] <- x[i, , drop = FALSE]
    }
    here <- adds[adds$row == i, ]
    for (u in sort(unique(here$unit))) {
      pieces[[length(pieces) + 1L]] <- build_added(x, here[here$unit == u, ])
    }
  }
  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  return(out)
}

# Every manifest key MUST name the row it claims to name, in the sheet the
# script reconstructed.
check_keys <- function(sheet, expected, fails) {
  m <- MANIFEST[MANIFEST$sheet == sheet & MANIFEST$kind != "remove", ]
  keys <- vapply(
    seq_len(nrow(expected)),
    function(i) return(row_key(sheet, expected, i)),
    character(1)
  )
  for (key in unique(m$key)) {
    n <- sum(keys == key)
    if (n != 1L) {
      fails <- c(
        fails,
        sprintf(
          "%s: manifest key '%s' matches %d rows of the new sheet, not 1",
          sheet,
          key,
          n
        )
      )
    }
  }
  return(fails)
}

# The positional column check. The new column-name vector MUST be the old one
# with the approved columns appended, in order. Comparing name SETS would let
# a reordering pass.
check_columns <- function(sheet, old, new, fails) {
  want <- c(names(old), APPENDED_COLUMNS[[sheet]])
  if (!identical(names(new), want)) {
    fails <- c(
      fails,
      sprintf(
        "%s: column names are\n    %s\n  the manifest says\n    %s",
        sheet,
        paste(names(new), collapse = " | "),
        paste(want, collapse = " | ")
      )
    )
  }
  return(fails)
}

# Locate every cell on which the reconstruction and the workbook disagree.
locate_drift <- function(sheet, expected, actual) {
  out <- NULL
  if (!identical(dim(expected), dim(actual))) {
    return(sprintf(
      "%s: reconstruction is %d x %d, the workbook is %d x %d",
      sheet,
      nrow(expected),
      ncol(expected),
      nrow(actual),
      ncol(actual)
    ))
  }
  for (col in names(expected)) {
    bad <- which(expected[[col]] != actual[[col]])
    for (i in bad) {
      out <- c(
        out,
        sprintf(
          "%s: row %d [%s] column %s is '%s', the manifest builds '%s'",
          sheet,
          i,
          row_key(sheet, actual, i),
          col,
          actual[[col]][i],
          expected[[col]][i]
        )
      )
    }
  }
  return(out)
}

# A "noop" entry claims that a row is present in both codebooks and unchanged.
# The reconstruction cannot verify that on its own, because a no-op leaves the
# reconstruction unchanged. So assert it here, directly, on both codebooks.
#
# Three assertions per record, and each one names its own failure:
#   1. the OLD row at the declared index carries the declared value
#   2. the key names exactly one row in each codebook
#   3. the two rows agree on every column the OLD codebook has
check_noops <- function(sheet, old, new, fails) {
  m <- MANIFEST[MANIFEST$sheet == sheet & MANIFEST$kind == "noop", ]
  wide <- widen(sheet, old)
  for (r in unique(m$row)) {
    part <- m[m$row == r, ]
    key <- part$key[1]
    fails <- c(fails, noop_cells(sheet, part, wide, new, r, key))
    fails <- c(fails, noop_row(sheet, old, new, part$item[1], r, key))
  }
  return(fails)
}

noop_cells <- function(sheet, part, wide, new, r, key) {
  out <- character(0)
  if (!identical(row_key(sheet, wide, r), key)) {
    out <- c(
      out,
      sprintf(
        "item %s: %s row %d has key '%s', the manifest says '%s'",
        part$item[1],
        sheet,
        r,
        row_key(sheet, wide, r),
        key
      )
    )
  }
  hit <- which(vapply(
    seq_len(nrow(new)),
    function(i) return(identical(row_key(sheet, new, i), key)),
    logical(1)
  ))
  if (length(hit) != 1L) {
    return(c(
      out,
      sprintf(
        "item %s: key '%s' names %d rows of the new %s, not 1",
        part$item[1],
        key,
        length(hit),
        sheet
      )
    ))
  }
  for (i in seq_len(nrow(part))) {
    col <- part$column[i]
    a <- wide[[col]][r]
    b <- new[[col]][hit]
    if (!identical(a, part$old[i]) || !identical(b, part$new[i])) {
      out <- c(
        out,
        sprintf(
          "item %s: %s [%s] column %s is '%s' in old, '%s' in new, manifest '%s'",
          part$item[1],
          sheet,
          key,
          col,
          a,
          b,
          part$old[i]
        )
      )
    }
  }
  return(out)
}

# The whole row, every column the OLD codebook has, must agree.
noop_row <- function(sheet, old, new, item, r, key) {
  wide <- widen(sheet, old)
  hit <- which(vapply(
    seq_len(nrow(new)),
    function(i) return(identical(row_key(sheet, new, i), key)),
    logical(1)
  ))
  if (length(hit) != 1L) {
    return(character(0))
  }
  a <- vapply(names(old), function(col) return(old[[col]][r]), character(1))
  b <- vapply(names(old), function(col) return(new[[col]][hit]), character(1))
  if (identical(unname(a), unname(b))) {
    return(character(0))
  }
  bad <- names(old)[a != b]
  return(sprintf(
    "item %s: %s [%s] differs between the codebooks in column(s) %s",
    item,
    sheet,
    key,
    paste(bad, collapse = ", ")
  ))
}

check_sheet <- function(sheet, old, new) {
  fails <- character(0)
  fails <- check_columns(sheet, old, new, fails)
  if (length(fails) > 0) {
    return(fails)
  }
  x <- widen(sheet, old)
  fails <- check_removals(sheet, x, fails)
  fails <- check_noops(sheet, old, new, fails)
  step <- apply_cells(sheet, x, fails)
  expected <- assemble(sheet, step$x)
  fails <- check_keys(sheet, expected, step$fails)
  return(c(fails, locate_drift(sheet, expected, new)))
}

# ====================================================== the frozen codebooks ====

# The git tree entry HEAD records for one path: the file mode and the blob.
head_entry <- function(path) {
  line <- system2(
    "git",
    c("ls-tree", "HEAD", "--", path),
    stdout = TRUE,
    stderr = FALSE
  )
  if (length(line) != 1L) {
    return(c(mode = "?", blob = "?"))
  }
  parts <- strsplit(line, "[ \t]+")[[1]]
  return(c(mode = parts[1], blob = substr(parts[3], 1, 12)))
}

# The same two fields, read off the working file. Git records only the owner
# execute bit, which is octal 100, so that is the only bit to test.
working_entry <- function(path) {
  blob <- system2("git", c("hash-object", path), stdout = TRUE, stderr = FALSE)
  exec <- bitwAnd(as.integer(file.info(path)$mode), 64L) != 0L
  return(c(
    mode = if (exec) "100755" else "100644",
    blob = if (length(blob) == 1L) substr(blob, 1, 12) else "?"
  ))
}

# A dated codebook is immutable. Compare BOTH fields: a chmod rewrites the tree
# entry and leaves the content untouched, so a blob comparison alone misses it.
check_frozen <- function() {
  ok <- TRUE
  for (path in FROZEN_PATHS) {
    want <- head_entry(path)
    got <- working_entry(path)
    same <- identical(want, got)
    cat(sprintf(
      "  %s  %s  working %s %s / HEAD %s %s\n",
      if (same) "OK  " else "FAIL",
      path,
      got[["mode"]],
      got[["blob"]],
      want[["mode"]],
      want[["blob"]]
    ))
    if (!same) {
      ok <- FALSE
    }
  }
  return(ok)
}

# ================================================== the ladder, for b, d, e ====

# Walk the parsed function body and collect the literal pattern of every
# str_detect() rung. deparse() wraps a long line between the call and its
# argument, so a regular expression over the deparsed text misses rungs.
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

ladder_patterns <- function(fn) {
  out <- character(0)
  walk <- function(e) {
    if (!is.call(e)) {
      return(invisible(NULL))
    }
    if (identical(call_name(e), "str_detect") && length(e) >= 3L) {
      if (is.character(e[[3]])) {
        out <<- c(out, e[[3]])
      }
    }
    for (i in seq_along(e)) {
      if (identical(e[i], list(quote(expr = )))) {
        next
      }
      walk(e[[i]])
    }
    return(invisible(NULL))
  }
  walk(body(utils::removeSource(fn)))
  return(out)
}

classify_with <- function(fn, produkt) {
  x <- data.table::data.table(produkt = produkt)
  fn(x)
  return(x$product_category)
}

LADDER_NAMES <- c(
  "lmed_categorize_product_names_v20230509",
  "lmed_categorize_product_names_v20250909"
)

ladders <- function() {
  ns <- asNamespace("mht")
  out <- lapply(LADDER_NAMES, function(nm) return(get(nm, envir = ns)))
  names(out) <- LADDER_NAMES
  return(out)
}

# Run one classifier expectation against BOTH ladders and report one line.
classify_both <- function(produkt) {
  got <- lapply(ladders(), function(fn) return(classify_with(fn, produkt)))
  return(got)
}

# ================================================= the item by item report ====

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

manifest_lines <- function(id) {
  m <- MANIFEST[MANIFEST$item == id, ]
  return(sprintf(
    "[%s] %s | %s | '%s' -> '%s'",
    m$sheet,
    m$key,
    m$column,
    m$old,
    m$new
  ))
}

# The no-op record for one item, read back out of MANIFEST.
noop_of <- function(item) {
  return(MANIFEST[MANIFEST$item == item & MANIFEST$kind == "noop", ])
}

report_item_a <- function(new) {
  added <- MANIFEST[MANIFEST$item == "a" & MANIFEST$column == "Preparatnamn", ]
  laf <- noop_of("a")
  ok <- setequal(added$new, setdiff(DECISIONS$produkt, LAFAMME_PRODUKT)) &&
    nrow(laf) == 3L &&
    identical(unique(laf$key), "Lafamme|B5||||")
  detail <- character(0)
  for (i in seq_len(nrow(DECISIONS))) {
    p <- DECISIONS$produkt[i]
    hit <- which(new$Preparatnamn == p)
    sub <- if (length(hit) == 1L) new$Subgrupp[hit] else "(not unique)"
    com <- if (length(hit) == 1L) new$Comment[hit] else ""
    want_com <- if (identical(p, LAFAMME_PRODUKT)) {
      ""
    } else {
      DECISIONS$register_form[i]
    }
    good <- length(hit) == 1L &&
      identical(sub, DECISIONS$decision_code[i]) &&
      identical(com, want_com)
    if (!good) {
      ok <- FALSE
    }
    detail <- c(
      detail,
      sprintf(
        "%-14s Subgrupp=%-4s decision=%-4s Comment='%s'%s",
        p,
        sub,
        DECISIONS$decision_code[i],
        com,
        if (identical(p, LAFAMME_PRODUKT)) {
          "  [MANIFEST noop: present already, unchanged, no action]"
        } else {
          ""
        }
      )
    )
  }
  detail <- c(
    detail,
    sprintf(
      "Lafamme noop, asserted by check_noops(): old row %d, %s",
      laf$row[1],
      paste(sprintf("%s='%s'", laf$column, laf$new), collapse = ", ")
    )
  )
  detail <- c(detail, csv_cross_check())
  item(
    "a",
    ok,
    sprintf(
      "8 rows added, Lafamme present already at old row %d, 37 NOT_MHT get none",
      laf$row[1]
    ),
    detail
  )
  return(invisible(NULL))
}

csv_cross_check <- function() {
  if (!file.exists(DECISIONS_CSV)) {
    REPORT$ok <- FALSE
    return(sprintf("CSV CROSS-CHECK DID NOT RUN: %s is absent", DECISIONS_CSV))
  }
  csv <- utils::read.csv(DECISIONS_CSV, stringsAsFactors = FALSE)
  keep <- csv[
    csv$decision_code != "NOT_MHT",
    c(
      "produkt",
      "decision_code",
      "register_form"
    )
  ]
  keep <- keep[order(keep$produkt), ]
  want <- DECISIONS[order(DECISIONS$produkt), ]
  rownames(keep) <- NULL
  rownames(want) <- NULL
  n_not <- sum(csv$decision_code == "NOT_MHT")
  same <- identical(keep, want) && identical(n_not, DECISIONS_NOT_MHT_N)
  if (!same) {
    REPORT$ok <- FALSE
  }
  return(sprintf(
    "CSV cross-check: %s (%d non-NOT_MHT, %d NOT_MHT of %d rows)",
    if (same) "the literal table matches the CSV" else "MISMATCH",
    nrow(keep),
    n_not,
    nrow(csv)
  ))
}

# One item b product: its MANIFEST noop entry, both codebooks and the ladder.
item_b_one <- function(produkt, entry, o, n, lad, want_ladder) {
  good <- nrow(entry) == 1L &&
    identical(entry$old[1], entry$new[1]) &&
    identical(o, entry$old[1]) &&
    identical(n, entry$new[1]) &&
    all(lad == want_ladder)
  said <- if (nrow(entry) == 1L) entry$old[1] else "?"
  row <- if (nrow(entry) == 1L) as.character(entry$row[1]) else "?"
  line <- sprintf(
    "%-10s noop row %s Subgrupp='%s'; 20241105=%-4s 20260828=%-4s ladder=%s",
    produkt,
    row,
    said,
    o,
    n,
    paste(unique(lad), collapse = "/")
  )
  return(list(good = good, line = line))
}

report_item_b <- function(old, new) {
  got <- classify_both(LADDER_TODAY$produkt)
  noop <- noop_of("b")
  # Every item b product MUST have a noop entry, and no other kind of entry.
  ok <- nrow(noop) == nrow(LADDER_TODAY) &&
    setequal(sub("\\|.*", "", noop$key), LADDER_TODAY$produkt) &&
    all(MANIFEST$kind[MANIFEST$item == "b"] == "noop")
  detail <- character(0)
  for (i in seq_len(nrow(LADDER_TODAY))) {
    produkt <- LADDER_TODAY$produkt[i]
    one <- item_b_one(
      produkt,
      noop[sub("\\|.*", "", noop$key) == produkt, ],
      old$Subgrupp[match(produkt, old$Preparatnamn)],
      new$Subgrupp[match(produkt, new$Preparatnamn)],
      vapply(got, function(v) return(v[i]), character(1)),
      LADDER_TODAY$ladder[i]
    )
    if (!one$good) {
      ok <- FALSE
    }
    detail <- c(detail, one$line)
  }
  detail <- c(detail, "the divergence is in the ladder, which phase 3 owns")
  item(
    "b",
    ok,
    "NO-OP: the three subgroups are already right in both codebooks",
    detail
  )
  return(invisible(NULL))
}

report_item_c <- function(old) {
  hit <- which(
    old$`Detaljerad kod` == "G03FA01" &
      old$Administrationssätt == "Transdermalt"
  )
  ok <- identical(
    sort(old$Preparatnamn[hit]),
    c("Estalis", "Estalis Sekvens")
  ) &&
    all(old$Subgrupp[hit] == "B1")
  item(
    "c",
    ok,
    "Evorel Micronor added as B1, G03FA01, transdermal",
    c(
      sprintf(
        "G03FA01 + Transdermalt in 20241105: %s, all Subgrupp %s",
        paste(old$Preparatnamn[hit], collapse = ", "),
        paste(unique(old$Subgrupp[hit]), collapse = "/")
      ),
      manifest_lines("c")
    )
  )
  return(invisible(NULL))
}

report_item_d <- function(new) {
  squash <- function(v) return(gsub(" ", "", v, fixed = TRUE))
  n_wb <- sum(squash(new$Preparatnamn) == "Testovirondepot")
  pats <- lapply(ladders(), function(fn) {
    return(sum(ladder_patterns(fn) == "Testovirondepot"))
  })
  ok <- n_wb == 1L && all(unlist(pats) == 2L)
  item(
    "d",
    ok,
    "NO-OP: MHT_groups has no Testovirondepot duplicate",
    c(
      sprintf("MHT_groups rows matching Testovirondepot: %d", n_wb),
      sprintf(
        "ladder rungs carrying the pattern: %s",
        paste(sprintf("%s=%d", names(pats), unlist(pats)), collapse = ", ")
      ),
      "the duplicate is ladder-only, and phase 3 owns it"
    )
  )
  return(invisible(NULL))
}

report_item_e <- function() {
  probe <- c(UNREACHED_E$produkt, "Extempore progesteron")
  got <- classify_both(probe)
  ok <- TRUE
  detail <- character(0)
  for (i in seq_len(nrow(UNREACHED_E))) {
    lad <- vapply(got, function(v) return(v[i]), character(1))
    if (!all(is.na(lad))) {
      ok <- FALSE
    }
    detail <- c(
      detail,
      sprintf(
        "%-24s ladder=%-4s owner=%s",
        UNREACHED_E$produkt[i],
        paste(unique(lad), collapse = "/"),
        UNREACHED_E$owner[i]
      )
    )
  }
  # `after` is the category the corrected spelling reaches. Read it out of the
  # table rather than repeating the value, so the column cannot go stale.
  typo <- which(UNREACHED_E$owner == "workbook typo, fixed here")
  want <- UNREACHED_E$after[typo]
  fixed <- vapply(got, function(v) return(v[length(probe)]), character(1))
  if (length(typo) != 1L || !all(fixed == want)) {
    ok <- FALSE
  }
  # `owner` is a claim about who fixes each one. The rows it calls a workbook
  # typo MUST be exactly the rows MANIFEST changes under item e.
  changed <- MANIFEST$old[
    MANIFEST$item == "e" & MANIFEST$column == "Preparatnamn"
  ]
  if (!setequal(UNREACHED_E$produkt[typo], changed)) {
    ok <- FALSE
  }
  if (!all(is.na(UNREACHED_E$after[-typo]))) {
    ok <- FALSE
  }
  detail <- c(
    detail,
    sprintf(
      "Extempore progesteron    ladder=%s  the item e typo fix moves it off NA, to the declared %s",
      paste(unique(fixed), collapse = "/"),
      want
    ),
    sprintf(
      "owner='workbook typo, fixed here' names %s; MANIFEST item e changes %s",
      paste(UNREACHED_E$produkt[typo], collapse = ", "),
      paste(changed, collapse = ", ")
    )
  )
  item(
    "e",
    ok,
    "6 products reach no category: 1 workbook typo, 5 ladder defects",
    detail
  )
  return(invisible(NULL))
}

report_item_f <- function(new) {
  cols <- APPENDED_COLUMNS$MHT_groups
  filled <- lapply(cols, function(col) return(which(nzchar(new[[col]]))))
  names(filled) <- cols
  ok <- identical(new$strength_mg_min[filled$strength_mg_min], "150") &&
    identical(new$strength_mg_max[filled$strength_mg_max], "150") &&
    length(filled$strength_mg_min) == 1L &&
    length(filled$strength_mg_max) == 1L
  item(
    "f",
    ok,
    "two columns appended to MHT_groups, NA except where strength-keyed",
    c(
      sprintf(
        "strength_mg_min filled on %d row(s), strength_mg_max on %d row(s), of %d",
        length(filled$strength_mg_min),
        length(filled$strength_mg_max),
        nrow(new)
      ),
      sprintf("column order: %s", paste(tail(names(new), 3), collapse = " | "))
    )
  )
  return(invisible(NULL))
}

report_item_g <- function(new) {
  hit <- which(new$Preparatnamn == "Utrogestan")
  ok <- length(hit) == 2L &&
    !("Utrogest" %in% new$Preparatnamn) &&
    identical(new$minimum_monthly_dose[hit], c("28", "12")) &&
    identical(new$strength_mg_max[hit], c("150", "")) &&
    identical(new$strength_mg_min[hit], c("", "150"))
  item(
    "g",
    ok,
    "Utrogest deleted, two strength-keyed Utrogestan rows remain",
    c(
      sprintf(
        "rows %s: dose/months %s, strength_mg_min %s, strength_mg_max %s",
        paste(hit, collapse = "+"),
        paste(
          sprintf(
            "%s/%s",
            new$minimum_monthly_dose[hit],
            new$minimum_months[hit]
          ),
          collapse = " and "
        ),
        paste(sprintf("'%s'", new$strength_mg_min[hit]), collapse = " and "),
        paste(sprintf("'%s'", new$strength_mg_max[hit]), collapse = " and ")
      ),
      "phase 4 MUST read 28 and 12 from these cells, never as literals",
      manifest_lines("g")
    )
  )
  return(invisible(NULL))
}

doesnotinclude_cols <- function(x) {
  return(grep("^doesnotinclude", names(x), value = TRUE))
}

excluded_set <- function(x, i) {
  v <- unlist(x[i, doesnotinclude_cols(x)], use.names = FALSE)
  return(sort(v[nzchar(v)]))
}

report_item_h <- function(id, new, rows, headline) {
  ok <- TRUE
  detail <- character(0)
  for (i in rows) {
    ex <- excluded_set(new, i)
    if (!("C5" %in% ex)) {
      ok <- FALSE
    }
    detail <- c(
      detail,
      sprintf(
        "row %3d %-20s approach %s includes %s: %d exclusions, C5 %s",
        i,
        new$variable[i],
        new$approach[i],
        new$includes1[i],
        length(ex),
        if ("C5" %in% ex) "present" else "ABSENT"
      )
    )
  }
  item(id, ok, headline, detail)
  return(invisible(NULL))
}

report_item_i <- function(new) {
  hit <- which(new$variable == "tibolone")
  ok <- length(hit) == 3L &&
    identical(sort(new$approach[hit]), c("1", "2", "3")) &&
    all(new$includes1[hit] == "F1")
  item(
    "i",
    ok,
    "a tibolone variable carrying F1 in approaches 1, 2 and 3",
    c(
      sprintf(
        "rows %s, approaches %s",
        paste(hit, collapse = ", "),
        paste(new$approach[hit], collapse = ", ")
      ),
      manifest_lines("i")
    )
  )
  return(invisible(NULL))
}

# Item i2. Two variables lighting at once IS the clash mechanism, so tibolone
# needs no exclusion. The assertion below is what carries that claim.
#
# apply_lmed_approaches_to_skeleton_v20250909() skips local_or_none_mht when it
# builds the run-length columns, so only the other variables can clash. It then
# labels a person-week "clashingprescriptions" when two of them tie at the
# minimum run length. Every clash-eligible variable other than estrogen_only
# carries no doesnotinclude at all, so co-occurring groups already light two
# variables today.
CLASH_INELIGIBLE <- "local_or_none_mht"

# The claim above is about the resolver, so read the resolver. It skips this
# variable when it builds the run-length columns, which is what keeps the
# variable out of the clash count.
clash_ineligible_in_source <- function() {
  fn <- get(
    "apply_lmed_approaches_to_skeleton_v20250909",
    envir = asNamespace("mht")
  )
  src <- paste(deparse(body(utils::removeSource(fn))), collapse = " ")
  skips <- grepl(
    sprintf('j == "%s"', CLASH_INELIGIBLE),
    src,
    fixed = TRUE
  )
  clash <- grepl("num_of_approaches_at_row_min > 1", src, fixed = TRUE)
  return(skips && clash)
}

report_item_i2 <- function(new) {
  detail <- character(0)
  ok <- TRUE
  for (a in c("1", "2", "3")) {
    rows <- which(new$approach == a)
    vars <- setdiff(unique(new$variable[rows]), CLASH_INELIGIBLE)
    n_excl <- vapply(
      vars,
      function(v) {
        idx <- rows[new$variable[rows] == v]
        return(max(vapply(
          idx,
          function(i) return(length(excluded_set(new, i))),
          integer(1)
        )))
      },
      integer(1)
    )
    detail <- c(
      detail,
      sprintf(
        "approach %s clash-eligible: %s",
        a,
        paste(
          sprintf("%s(max %d exclusions per rule)", vars, n_excl),
          collapse = ", "
        )
      )
    )
  }
  # The two pairs that can already both be TRUE for one person-week today.
  pairs <- list(
    c("peroral_estrogen", "transdermal_estrogen"),
    c("estrogen_progesterone_synthetic", "estrogen_progesterone_bioidentical")
  )
  for (p in pairs) {
    idx <- which(new$variable %in% p)
    n <- sum(vapply(
      idx,
      function(i) return(length(excluded_set(new, i))),
      integer(1)
    ))
    if (n != 0L) {
      ok <- FALSE
    }
    detail <- c(
      detail,
      sprintf(
        "%s and %s carry %d exclusions between them",
        p[1],
        p[2],
        n
      )
    )
  }
  n_f1 <- sum(vapply(
    seq_len(nrow(new)),
    function(i) return(as.integer("F1" %in% excluded_set(new, i))),
    integer(1)
  ))
  if (n_f1 != 0L) {
    ok <- FALSE
  }
  source_ok <- clash_ineligible_in_source()
  if (!source_ok) {
    ok <- FALSE
  }
  detail <- c(
    detail,
    sprintf("F1 appears in %d doesnotinclude lists", n_f1),
    sprintf(
      "the resolver skips %s and clashes on a tie: %s",
      CLASH_INELIGIBLE,
      if (source_ok) "confirmed in the function body" else "NOT FOUND"
    ),
    "FINDING: lighting two variables IS the clash mechanism. No change made."
  )
  item(
    "i2",
    ok,
    "tibolone gets no exclusion, because none of its peers has one",
    detail
  )
  return(invisible(NULL))
}

report_item_j <- function(old, new) {
  n_old <- sum(
    old$variable == "peroral_estrogen" &
      old$approach == "2" &
      old$includes1 == "A2"
  )
  n_new <- sum(
    new$variable == "peroral_estrogen" &
      new$approach == "2" &
      new$includes1 == "A2"
  )
  ok <- n_old == 2L && n_new == 1L
  item(
    "j",
    ok,
    "the duplicated A2 peroral rule in approach 2 is gone",
    c(
      sprintf(
        "peroral_estrogen|2|A2 rows: 20241105 has %d, 20260828 has %d",
        n_old,
        n_new
      ),
      manifest_lines("j")
    )
  )
  return(invisible(NULL))
}

# ==================================================================== main ====

show_manifest <- function() {
  cat("\nMANIFEST, one entry per (sheet, row key, column, old, new)\n")
  cat(sprintf("  %d entries\n", nrow(MANIFEST)))
  for (i in seq_len(nrow(MANIFEST))) {
    cat(sprintf(
      "  %-3s %-14s %-6s %-30s %-22s '%s' -> '%s'\n",
      MANIFEST$item[i],
      MANIFEST$sheet[i],
      MANIFEST$kind[i],
      MANIFEST$key[i],
      MANIFEST$column[i],
      MANIFEST$old[i],
      MANIFEST$new[i]
    ))
  }
  return(invisible(NULL))
}

# Run the package test suite, with NOT_CRAN set, and fail the script when any
# test fails. Items b, d and e assert what the classifier returns today, so the
# script already depends on the package behaving. One command then proves both.
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

main <- function() {
  cat("FROZEN CODEBOOKS\n")
  frozen_ok <- check_frozen()
  if (!frozen_ok) {
    REPORT$ok <- FALSE
  }

  for (path in c(OLD_PATH, NEW_PATH)) {
    if (!file.exists(path)) {
      stop(
        sprintf("%s not found. Run this from the package root.", path),
        call. = FALSE
      )
    }
  }
  old_sheets <- readxl::excel_sheets(OLD_PATH)
  new_sheets <- readxl::excel_sheets(NEW_PATH)
  if (!identical(old_sheets, new_sheets)) {
    stop(
      sprintf(
        "sheet names differ: %s vs %s",
        paste(old_sheets, collapse = ", "),
        paste(new_sheets, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  cat("\nSHEETS\n")
  cat(sprintf("  old: %s\n  new: %s\n", OLD_PATH, NEW_PATH))
  drift <- character(0)
  sheets <- list()
  for (sheet in old_sheets) {
    old <- read_sheet(OLD_PATH, sheet)
    new <- read_sheet(NEW_PATH, sheet)
    sheets[[sheet]] <- list(old = old, new = new)
    bad <- check_sheet(sheet, old, new)
    drift <- c(drift, bad)
    cat(sprintf(
      "  %-4s %-14s %3d x %3d  ->  %3d x %3d\n",
      if (length(bad) == 0L) "OK" else "FAIL",
      sheet,
      nrow(old),
      ncol(old),
      nrow(new),
      ncol(new)
    ))
  }
  if (length(drift) > 0) {
    REPORT$ok <- FALSE
    cat("\nUNAPPROVED DRIFT\n")
    for (line in drift) {
      cat("  ", line, "\n", sep = "")
    }
  }

  mg <- sheets$MHT_groups
  pg <- sheets$post_grouping
  cat("\nITEM BY ITEM\n")
  report_item_a(mg$new)
  report_item_b(mg$old, mg$new)
  report_item_c(mg$old)
  report_item_d(mg$new)
  report_item_e()
  report_item_f(mg$new)
  report_item_g(mg$new)
  report_item_h(
    "h",
    pg$new,
    43:46,
    "C5 joins estrogen_only's doesnotinclude, approach 3"
  )
  report_item_h(
    "h2",
    pg$new,
    38:40,
    "C5 joins local_or_none_mht's doesnotinclude, approach 2"
  )
  report_item_i(pg$new)
  report_item_i2(pg$new)
  report_item_j(pg$old, pg$new)

  show_manifest()
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
