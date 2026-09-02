#!/usr/bin/env Rscript
# Count every product in one LMED delivery: prescriptions and people.
#
# Run:  Rscript dev/product-table/scan-delivery.R <label> <lmed.txt> <out.qs2>
#
# The two deliveries do NOT share a field layout. 2021 carries ATC at field 5
# and produkt at 24; 2026 carries them at 2 and 21. So the layout is resolved
# from the header by NAME, and the script stops if a name is missing. A
# hardcoded position that is silently wrong reads a different column and
# reports counts for something else entirely.
#
# The counting runs in awk, not R. The 2026 file is 55.8 GB and must never be
# read whole into memory.

suppressMessages({library(data.table)})
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L) stop("usage: scan-delivery.R <label> <lmed.txt> <out.qs2>", call. = FALSE)
label <- args[1]; path <- args[2]; out <- args[3]
if (!file.exists(path)) stop("no such delivery file: ", path, call. = FALSE)

header <- strsplit(readLines(path, n = 1L), "\t", fixed = TRUE)[[1]]
field <- function(pattern, what) {
  i <- grep(pattern, header, ignore.case = TRUE)
  if (length(i) == 0L) {
    stop("the header of ", basename(path), " names no ", what,
         " column. Header: ", paste(header, collapse = ", "), call. = FALSE)
  }
  return(i[1])
}
f_id      <- field("^(P[0-9]+_)?LopNr", "person identifier")
f_atc     <- field("^ATC$", "ATC")
f_produkt <- field("^produkt$", "product name")
f_lform   <- field("^lform$", "dosage form")

cat("delivery ", label, ": id=", f_id, " atc=", f_atc, " produkt=", f_produkt,
    " lform=", f_lform, "\n", sep = "")

tmp <- tempfile(fileext = ".tsv")
prog <- sprintf('
  NR > 1 {
    k = $%d "\\t" $%d "\\t" $%d
    n[k]++
    pk = k SUBSEP $%d
    if (!(pk in seen)) { seen[pk] = 1; ppl[k]++ }
  }
  END { for (k in n) printf "%%s\\t%%d\\t%%d\\n", k, n[k], ppl[k] }',
  f_produkt, f_atc, f_lform, f_id)
cmd <- sprintf("LC_ALL=C awk -F'\\t' %s %s > %s",
               shQuote(prog), shQuote(path), shQuote(tmp))
status <- system(cmd)
if (status != 0L) stop("the awk pass failed with status ", status, call. = FALSE)

# quote = "" because register product names carry stray quote characters, and
# fread's default handling then misdetects the column count.
d <- fread(tmp, header = FALSE, sep = "\t", quote = "", encoding = "Latin-1",
           col.names = c("produkt", "atc", "lform", "prescriptions", "people"))
unlink(tmp)
d[, delivery := label]
setcolorder(d, c("delivery", "atc", "produkt", "lform", "prescriptions", "people"))
setorder(d, atc, produkt)

if (nrow(d) == 0L) stop("the scan of ", label, " returned no rows", call. = FALSE)
qs2::qs_save(d, out)
cat("wrote ", out, ": ", nrow(d), " (product, atc, form) rows | ",
    uniqueN(d$produkt), " product names | ", sum(d$prescriptions),
    " prescriptions\n", sep = "")
