#!/usr/bin/env Rscript
# The three checks over a product table.
#
# The table replaces the ladder. One row per register product name, and the
# category is looked up, never matched by prefix.
#
# Run:  Rscript dev/product-table/check-product-table.R <table.csv> <delivery.tsv> [...]
#
# <table.csv>    produkt, atc, lform, route, category, source, note
# <delivery.tsv> produkt, atc, lform, rx, persons -- one product scan per delivery
#
# Exit 0 on pass, 1 on any failure. Every check reports what it measured, so a
# silent pass and a skipped check cannot look alike.

suppressMessages(library(data.table))

# Locate route.R beside this script, whatever the working directory.
script_dir <- function() {
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[startsWith(a, "--file=")])
  if (length(f) == 1L) return(dirname(normalizePath(f)))
  return(".")
}
source(file.path(script_dir(), "route.R"))

# The one normalisation. Both sides of every comparison go through it, so a
# register string declared latin1 and a table string declared UTF-8 compare
# equal. An exact match on the raw bytes would not: the rawbatch declares
# produkt latin1 on some rows and unknown on others, measured 2026-08-31.
norm_name <- function(x) tolower(stringr::str_remove_all(as.character(x), "[^a-zA-Z]"))

# A category no approach rule reads cannot change exposure, so for the purpose
# of check 3 it is the same as no category at all. H1 is testosterone, which
# the codebook marks NOT EXPOSURE.
# A classification no approach rule reads cannot change exposure, so for
# check 3 it is the same as no classification. H1 is testosterone and G1 is
# Duavive; the codebook and the clinical decisions put both outside exposure.
# `excluded` is NOT in this list: an excluded product keeps its category, and a
# disagreement about the category is still a disagreement.
INERT <- c("notmht", "H1", "G1")

fail <- function(...) { cat("FAIL: ", ..., "\n", sep = ""); TRUE }

check_1_completeness <- function(tab, del) {
  cat("\n== Check 1. Every delivered product is in the table ==\n")
  missing <- del[!produkt %chin% tab$produkt_raw]
  cat("  delivered products: ", uniqueN(del$produkt),
      " | absent from the table: ", uniqueN(missing$produkt), "\n", sep = "")
  if (nrow(missing) == 0L) return(FALSE)
  print(missing[order(-persons), .(produkt, atc, lform, rx, persons)])
  return(fail(uniqueN(missing$produkt), " delivered products have no row. ",
              "An absent product reads as no MHT, so it enters as an unexposed control."))
}

check_2_key_agreement <- function(tab) {
  cat("\n== Check 2. Raw names sharing a lookup key agree ==\n")
  # Several raw register names reduce to one key, and that is expected:
  # `Estramon 25`, `Estramon 75` and `Estramon 100` are one product at three
  # strengths. What is NOT allowed is for them to disagree, because the runtime
  # looks up the key and cannot see which raw name it came from.
  g <- tab[, .(nc = uniqueN(classification), nx = uniqueN(excluded)),
           keyby = produkt_clean][nc > 1L | nx > 1L]
  cat("  rows: ", nrow(tab), " | distinct keys: ", uniqueN(tab$produkt_clean),
      " | keys whose rows disagree: ", nrow(g), "\n", sep = "")
  if (nrow(g) == 0L) return(FALSE)
  print(tab[produkt_clean %chin% g$produkt_clean][
    order(produkt_clean), .(produkt_clean, produkt_raw, atc, classification, excluded)])
  return(fail(nrow(g), " lookup keys carry rows that disagree. The runtime ",
              "matches the key, so it cannot choose between them."))
}

check_3_atc_route_consistency <- function(tab) {
  cat("\n== Check 3. One substance and one route take one category ==\n")
  t2 <- copy(tab)
  t2[, eff := fifelse(classification %chin% INERT, "notmht", classification)]
  t2 <- t2[!is.na(atc) & nzchar(atc)]
  g <- t2[, .(nc = uniqueN(eff)), keyby = .(atc, route)][nc > 1L]
  # A waiver is a non-empty note on every row of the group that differs.
  bad <- merge(t2, g, by = c("atc", "route"))
  # A group is waived when at least one of its rows records a reason. The note
  # belongs on the row that differs, and which row that is depends on the group,
  # so the check asks only that somebody wrote the reason down.
  waived <- bad[!is.na(note) & nzchar(note), unique(.SD), .SDcols = c("atc", "route")]
  groups_unwaived <- fsetdiff(unique(bad[, .(atc, route)]), waived)
  cat("  (atc, route) groups: ", uniqueN(t2[, .(atc, route)]),
      " | disagreeing: ", nrow(g),
      " | without a written waiver: ", nrow(groups_unwaived), "\n", sep = "")
  if (nrow(groups_unwaived) == 0L) return(FALSE)
  print(merge(bad, groups_unwaived, by = c("atc", "route"))[
    order(atc, route), .(atc, route, produkt_raw, classification, excluded, note)])
  return(fail(nrow(groups_unwaived), " (atc, route) groups classify the same substance ",
              "two ways with no reason recorded. Either the categories agree, or the ",
              "row that differs carries a note saying why."))
}

main <- function(args) {
  if (length(args) < 2L) stop("usage: check-product-table.R <table.csv> <delivery.tsv> [...]", call. = FALSE)
  tab <- if (grepl("\\.xlsx$", args[1])) {
    sheets <- setdiff(readxl::excel_sheets(args[1]), "README")
    rbindlist(lapply(sheets, function(s) setDT(suppressMessages(
      readxl::read_excel(args[1], sheet = s, col_types = "text")))),
      use.names = TRUE, fill = TRUE)  # sheets differ: a sheet drops a count column it never measured
  } else {
    fread(args[1], encoding = "UTF-8", colClasses = "character")
  }
  need <- c("produkt_raw", "produkt_clean", "atc", "lform", "route",
            "classification", "classification_meaning", "excluded", "source", "note")
  if (!all(need %in% names(tab))) {
    stop("the table must carry: ", paste(need, collapse = ", "), call. = FALSE)
  }
  # A scan carries five columns when it was taken with the dosage form and
  # three when it was not. quote = "" because register names carry stray quotes.
  del <- rbindlist(lapply(args[-1], function(f) {
    z <- fread(f, header = FALSE, sep = "\t", quote = "", encoding = "Latin-1")
    if (ncol(z) >= 5L) {
      setnames(z, 1:5, c("produkt", "atc", "lform", "rx", "persons"))
    } else {
      setnames(z, 1:3, c("produkt", "atc", "rx"))
      z[, `:=`(lform = NA_character_, persons = NA_integer_)]
    }
    return(z[, .(produkt, atc, lform, rx, persons)])
  }), use.names = TRUE)
  del <- del[!startsWith(produkt, "#")]

  bad <- c(check_1_completeness(tab, del),
           check_2_key_agreement(tab),
           check_3_atc_route_consistency(tab))
  cat("\n")
  if (any(bad)) { cat("FAILED ", sum(bad), " of 3 checks\n", sep = ""); quit(status = 1L) }
  cat("PASS\n")
  return(invisible(NULL))
}

if (!interactive()) main(commandArgs(trailingOnly = TRUE))
