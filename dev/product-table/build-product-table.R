#!/usr/bin/env Rscript
# Build the product table from the delivery scans and the recorded sources.
#
# Run:  Rscript dev/product-table/build-product-table.R <out.xlsx> \
#         g:<label>=<scan-with-lform.tsv> ... all:<label>=<scan.tsv> ...
#
# ONE ROW PER RAW REGISTER NAME. `produkt_clean` is the lookup key, and several
# raw names may share one. That collapse is a fact about the register, so the
# table shows it rather than merging the rows and losing the names.
#
# The output is a DRAFT. Every `note` is empty, so check 3 fires on every
# substance the sources disagree about. That list is the worklist.

suppressMessages({library(data.table); library(stringr)})
script_dir <- function() {
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[startsWith(a, "--file=")])
  if (length(f) == 1L) return(dirname(normalizePath(f)))
  return(".")
}
source(file.path(script_dir(), "route.R"))
PKG <- file.path(script_dir(), "..", "..")
suppressMessages(pkgload::load_all(PKG, quiet = TRUE))
norm <- function(x) tolower(str_remove_all(as.character(x), "[^a-zA-Z]"))

args <- commandArgs(trailingOnly = TRUE)
out <- args[1]
specs <- args[-1]

read_scan <- function(spec) {
  sheet <- sub(":.*$", "", spec)
  rest  <- sub("^[^:]*:", "", spec)
  label <- sub("=.*$", "", rest)
  path  <- sub("^[^=]*=", "", rest)
  # quote = "" is load-bearing: register product names carry stray quote
  # characters, and fread's default handling then misdetects the column count.
  z <- fread(path, header = FALSE, sep = "\t", quote = "", encoding = "Latin-1")
  z <- z[!startsWith(V1, "#")]
  if (ncol(z) >= 5L) {
    setnames(z, 1:5, c("produkt", "atc", "lform", "rx", "people"))
  } else {
    setnames(z, 1:3, c("produkt", "atc", "rx"))
    z[, `:=`(lform = NA_character_, people = NA_integer_)]
  }
  if (TRUE) {
    z[, rx := as.integer(rx)]; z[, people := as.integer(people)]
  }
  z[, `:=`(sheet = sheet, delivery = label)]
  return(z[, .(sheet, delivery, produkt, atc, lform, rx, people)])
}
d <- rbindlist(lapply(specs, read_scan), use.names = TRUE)
d[, route := lmed_route_of_form(lform)]

tab <- d[, .(
  lform  = paste(sort(unique(na.omit(lform))), collapse = " / "),
  route  = paste(sort(unique(na.omit(route))), collapse = " / ")
), keyby = .(sheet, atc, produkt)]
setnames(tab, "produkt", "produkt_raw")
tab[, produkt_clean := norm(produkt_raw)]

counts <- dcast(d[, .(people = max(people), prescriptions = sum(rx)), by = .(atc, produkt, delivery)],
                atc + produkt ~ delivery, value.var = c("people", "prescriptions"))
tab <- merge(tab, counts, by.x = c("atc", "produkt_raw"), by.y = c("atc", "produkt"), all.x = TRUE)

# The classification, from the classifier itself.
x <- data.table(produkt = tab$produkt_raw)
mht:::lmed_categorize_product_names_v20260828(x)
tab[, classification := fifelse(is.na(x$product_category), "notmht", x$product_category)]

# Exclusion is a SEPARATE fact. A product removed from the study keeps the
# category the codebook gives it, because issue 4 may put it back.
EXCLUDED <- c("Cyclogest", "Delestrogen", "Neofollin", "Lafamme", "Visanne",
              "Endovelle", "Slinda", "Lugesteron", "Prolutex",
              "Extempore ATC-kod G03DA04 Progesteron")
tab[, exclude_entire_person := vapply(produkt_clean,
     function(z) any(startsWith(z, norm(EXCLUDED))), logical(1))]

# Provenance, recorded rather than inferred. Each branch names what actually
# decided the classification, and the codebook branch is an EXACT match on the
# cleaned name: a prefix match says only that some other product looks similar.
cb <- setDT(readxl::read_excel(file.path(PKG, "inst", "2023-mht",
            "dataDictionary20260828.xlsx"), sheet = "MHT_groups", col_types = "text"))
cb_clean <- norm(cb$Preparatnamn[!is.na(cb$Preparatnamn)])
dec_dir <- Sys.getenv("MHT_DECISIONS_DIR", "")
dec <- if (nzchar(dec_dir)) fread(file.path(dec_dir, "decisions-2026-08-26.csv")) else data.table(produkt = character(0))
alias <- mht:::lmed_rule_aliases_v20260828()
tab[, source := fcase(
  produkt_clean %chin% cb_clean,          "codebook20260828",
  produkt_clean %chin% norm(dec$produkt), "decision20260826",
  produkt_clean %chin% names(alias),      "alias in mht R source",
  classification != "notmht",             "R source only, no recorded decision",
  default = "not classified; nobody asked")]
tab[, note := ""]
tab[, exclusion_reason := NA_character_]

# Dated decision files override the classifier. Each row names a product, the
# classification it takes, and whether the study removes the whole person.
# MHT_DECISION_FILES is a colon-separated list, applied in order, so a later
# file supersedes an earlier one.
for (df in strsplit(Sys.getenv("MHT_DECISION_FILES", ""), ":", fixed = TRUE)[[1]]) {
  if (!nzchar(df)) next
  dd <- fread(df)
  dd[, key := norm(produkt)]
  for (i in seq_len(nrow(dd))) {
    hit <- startsWith(tab$produkt_clean, dd$key[i])
    if (!any(hit)) {
      stop("decision file ", basename(df), " names ", dd$produkt[i],
           ", which no delivered product matches", call. = FALSE)
    }
    tab[hit, `:=`(classification = dd$classification[i],
                  exclude_entire_person = as.logical(dd$exclude_entire_person[i]),
                  source = paste0("decision ", dd$decided_on[i]),
                  note = dd$note[i])]
    # Two different reasons now remove a person, and a sensitivity analysis may
    # want only one of them. Keep them separable rather than buried in prose.
    if ("exclusion_reason" %in% names(dd)) {
      tab[hit, exclusion_reason := dd$exclusion_reason[i]]
    }
  }
  cat("applied ", nrow(dd), " decisions from ", basename(df), "\n", sep = "")
}

# A previous table, so a reader can see what moved. Only a CHANGED value is
# written: a column repeating the current value on every row says nothing.
tab[, `:=`(classification_previous = NA_character_,
           exclude_entire_person_previous = NA_character_)]
prev_path <- Sys.getenv("MHT_PREVIOUS_TABLE", "")
if (nzchar(prev_path)) {
  prev <- rbindlist(lapply(setdiff(readxl::excel_sheets(prev_path), "README"),
    function(sh) setDT(suppressMessages(readxl::read_excel(prev_path, sheet = sh,
      col_types = "text")))), use.names = TRUE, fill = TRUE)
  ecol <- if ("exclude_entire_person" %in% names(prev)) "exclude_entire_person" else "excluded"
  prev <- prev[, .(produkt_raw, prev_class = classification, prev_excl = get(ecol))]
  tab[prev, on = "produkt_raw", `:=`(
    classification_previous = fifelse(classification != i.prev_class, i.prev_class, NA_character_),
    exclude_entire_person_previous = fifelse(
      toupper(as.character(exclude_entire_person)) != toupper(i.prev_excl),
      i.prev_excl, NA_character_))]
  cat("changed against ", basename(prev_path), ": ",
      tab[!is.na(classification_previous), .N], " classifications, ",
      tab[!is.na(exclude_entire_person_previous), .N], " exclusion flags\n", sep = "")
}

setnames(cb, c("Subgrupp", "Specifikation"), c("sub", "spec"), skip_absent = TRUE)
if (!"adm" %in% names(cb)) setnames(cb, "Administrationssätt", "adm", skip_absent = TRUE)

# The codebook is Swedish throughout. It carries no English anywhere, so the
# meaning below is OUR translation of its own cells, not a definition it
# supplies. Only the vocabulary is ours: the composition still comes from
# Specifikation and Administrationssatt, so a codebook edit still moves it.
TERMS <- c(
  "Estrogener" = "oestrogens",
  "Gestagener" = "progestogens",
  "Androgener" = "androgens",
  "Tibolon" = "tibolone",
  "Hormonella antikonceptionella medel för systemiskt bruk; gestagener" =
    "hormonal contraceptives for systemic use; progestogens",
  "Hormonella antikonceptionella medel för systemiskt bruk" =
    "hormonal contraceptives for systemic use",
  "Gestagener i kombination med estrogener" =
    "progestogens in combination with oestrogens",
  "Intrauterina preventivmedel; vid användning i kombination med G03C" =
    "intrauterine contraceptives, when used in combination with G03C",
  "Transdermalt" = "transdermal", "Peroralt" = "oral", "Peroral" = "oral",
  "Vaginalt (lokal effekt)" = "vaginal, local effect",
  "Vaginal progesteron" = "vaginal progesterone",
  "Injektion" = "injection", "Intramuskulärt" = "intramuscular",
  "Subkutant" = "subcutaneous", "IUD - gestagen" = "IUD, progestogen")
translate <- function(x) {
  out <- unname(TERMS[trimws(as.character(x))])
  bad <- !is.na(x) & is.na(out)
  if (any(bad)) {
    stop("no English term for: ", paste(unique(x[bad]), collapse = " | "),
         ". Add it to TERMS rather than leaving the Swedish in place.",
         call. = FALSE)
  }
  return(out)
}
mean_map <- cb[!is.na(sub) & !grepl("\\+", sub),
  .(meaning = paste(na.omit(c(translate(spec[1]), translate(adm[1]))), collapse = ", ")),
  keyby = sub]

# Two codebook rows describe themselves wrongly, so the derived English would
# be false. Both are recorded rather than silently patched.
OVERRIDE <- c(
  G1 = "oestrogens with bazedoxifene, oral (the codebook Specifikation says Tibolon, which is wrong: G1 is Duavive, G03CC07)",
  D2 = "hormonal contraceptives for systemic use; progestogens, subcutaneous implant (etonogestrel)",
  D3 = "hormonal contraceptives for systemic use; progestogens, subcutaneous implant (levonorgestrel)")
mean_map[names(OVERRIDE), meaning := unname(OVERRIDE), on = "sub"]

tab[mean_map, classification_meaning := i.meaning, on = c(classification = "sub")]
tab[classification == "notmht", classification_meaning := "not menopausal hormone therapy"]
if (anyNA(tab$classification_meaning)) {
  stop("no meaning for: ", paste(unique(tab[is.na(classification_meaning)]$classification), collapse = ", "), call. = FALSE)
}

setcolorder(tab, c("atc", "produkt_raw", "produkt_clean", "lform", "route",
                   "classification", "classification_meaning", "exclude_entire_person",
                   "exclusion_reason", "classification_previous", "exclude_entire_person_previous",
                   "source", "note"))
setorder(tab, atc, produkt_raw)

readme <- data.table(field = c(
  "atc", "produkt_raw", "produkt_clean", "lform", "route", "classification",
  "classification_meaning", "excluded", "source", "note", "exclusion_reason", "classification_previous", "exclude_entire_person_previous", "people_<delivery>", "prescriptions_<delivery>"),
  meaning = c(
  "The ATC the register assigns. Not the codebook Detaljerad kod, which disagrees with it.",
  "The register product name, exactly as delivered. One row per raw name.",
  "The lookup key: produkt_raw reduced to its lowercase ASCII letters. Several raw names may share one key. Every row sharing a key MUST share a classification.",
  "The register dosage form, every distinct value seen for this name.",
  "The route the form describes. Coarse on purpose: how the drug entered, not the package.",
  "A codebook subgroup, or notmht. This is what the drug IS, never whether the study keeps it.",
  "What that code means, in English. OUR translation of the codebook Specifikation and Administrationssatt cells: the codebook itself carries no English.",
  "TRUE where this study removes the WHOLE PERSON, not the prescription. Separate from classification, because a reversed exclusion must not lose the category.",
  "The classification this product carried in the previous table. Empty unless it changed.",
  "The exclusion flag it carried in the previous table. Empty unless it changed.",
  "What decided the classification. An exact cleaned-name match, never a prefix: a prefix says only that another product looks similar.",
  "REQUIRED where this row disagrees with another product of the same ATC and route. Says why.",
  "Why the person is removed. Two distinct reasons exist, and a sensitivity analysis may want only one.",
  "People holding the product per delivery. Not women: the query was sex-agnostic, so the H1 rows are men. Context only.",
  "Dispensed prescriptions per delivery: register rows, not people. A column absent from a sheet was never measured for it."))

wb <- openxlsx::createWorkbook()
for (s in unique(tab$sheet)) {
  z <- tab[sheet == s][, sheet := NULL]
  # Drop a count column this sheet never measured. A column of blanks cannot be
  # told apart from a column of zeroes, and the `other` ledger was scanned for
  # 2026 prescriptions only.
  empty <- names(z)[vapply(z, function(v) all(is.na(v)), logical(1))]
  if (length(empty) > 0L) z[, (empty) := NULL]
  openxlsx::addWorksheet(wb, s)
  openxlsx::writeData(wb, s, z)
  openxlsx::freezePane(wb, s, firstActiveRow = 2)
  openxlsx::setColWidths(wb, s, 1:ncol(z), widths = "auto")
  cat(s, ": ", nrow(z), " rows, ", uniqueN(z$produkt_clean), " distinct keys\n", sep = "")
}
openxlsx::addWorksheet(wb, "README")
openxlsx::writeData(wb, "README", readme)
openxlsx::setColWidths(wb, "README", 1:2, widths = c(24, 120))
openxlsx::saveWorkbook(wb, out, overwrite = TRUE)
cat("wrote ", out, "\n", sep = "")
