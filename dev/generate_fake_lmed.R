# Generate the synthetic MHT fixtures shipped in data/.
#
# Run with:  Rscript dev/generate_fake_lmed.R
#
# Deterministic: set.seed() below, no I/O other than the three .rda writes.
# Running it twice MUST produce byte-identical data/*.rda.
#
# The fixture is built to exercise the real x2023_mht_add_lmed() /
# x2026_mht_add_lmed() code paths, not merely to be well-formed:
#
#   * the grid spans 2015-W02 .. 2020-W52 (312 ISO weeks, 6 ISO years) so the
#     3-year minimum-duration rule (last_session_on_mht < 3 * 52) is reachable;
#   * every product category REACHABLE from the fcase ladder gets at least one
#     prescription (see the coverage block below);
#   * prescription gaps of <= 4 weeks (bridged by replace_false_runs) and
#     > 4 weeks (not bridged) both occur;
#   * one person has overlapping clashing prescriptions;
#   * one person has a negative fddd, which the 2026 code drops with a warning.

library(data.table)

set.seed(42)

base <- as.Date("2015-01-05") # Monday of ISO week 2015-02
grid_dates <- seq(base, as.Date("2020-12-27"), by = "week")

# --- prescriptions -----------------------------------------------------------
# Helper: one person's repeated dispensings of one product.
rx <- function(id, produkt, day_offsets, fddd) {
  data.table(
    lopnr = as.integer(id),
    produkt = produkt,
    edatum = base + day_offsets,
    fddd = as.numeric(fddd)
  )
}

parts <- list(
  # 1 long episode (~193 weeks >= 156) then stop -> "previous" survives
  rx(1, "Divigel", 84 * (0:15), 90),

  # 2 short episode (~73 weeks < 156) then stop -> "previous" becomes "exclude"
  rx(2, "Divigel", 84 * (0:5), 90),

  # 3 gap of 3 weeks -> bridged by replace_false_runs (run of FALSE <= 4)
  rx(3, "Divigel", c(0, 28, 56, 105, 133, 161), 28),

  # 4 gap of 10 weeks -> not bridged
  rx(4, "Divigel", c(0, 28, 56, 154, 182, 210), 28),

  # 5 clashing prescriptions: A1 transdermal + A2 peroral, concurrent
  rx(5, "Divigel", 84 * (0:3), 90),
  rx(5, "Progynon", 84 * (0:3), 90),

  # 6 long episode, > 4 week gap, then re-initiation on another route
  #   single -> "exclude" from re-initiation; multiple -> re-enters
  rx(6, "Divigel", 84 * (0:15), 90),
  rx(6, "Progynon", 1500 + 84 * (0:3), 90),

  # 7 local-only oestrogen throughout -> never leaves local_or_none_mht
  rx(7, "Vagifem", 84 * (0:9), 90),

  # 8-10 category coverage, one product per reachable category,
  #      spaced 12 weeks apart with fddd 28 so runs do not overlap
  rx(8, "Divigel", 0, 28),
  rx(8, "Progynon", 84, 28),
  rx(8, "Vagifem", 168, 28),
  rx(8, "Blissel", 252, 28),
  rx(8, "Oestriolaspen", 336, 28),
  rx(8, "Premarina", 420, 28),
  rx(8, "Neofollin", 504, 28),
  rx(9, "Estalis", 0, 28),
  rx(9, "Activelle", 84, 28),
  rx(9, "Indivina", 168, 28),
  rx(9, "Femostonconti", 252, 28),
  rx(9, "Climodien", 336, 28),
  rx(9, "Angemin", 420, 28),
  rx(9, "Sequidot", 504, 28),
  rx(9, "Trisekvens", 588, 28),
  rx(9, "Trivina", 672, 28),
  rx(9, "Femoston", 756, 28),
  rx(10, "Utrogestan", 0, 28),
  rx(10, "Cerazette", 336, 28),
  rx(10, "Provera", 672, 28),
  rx(10, "Livial", 1008, 28),
  rx(10, "Duavive", 1092, 28),
  rx(10, "Nebido", 1176, 28),
  rx(10, "MiniPe", 1260, 28),
  rx(10, "Exlutena", 1344, 28),

  # 11-14 the long-acting products, each alone: the code overrides fddd to
  #       1680 days for D3/E1 and 1008 for Jaydess, which swamps a shared person
  rx(11, "Nexplanon", 0, 28),
  rx(12, "Jadelle", 0, 28),
  rx(13, "Mirena", 0, 28),
  rx(14, "Jaydess", 0, 28),

  # 15-16 oestrogen + progestogen combinations -> approach 3 sub-levels
  rx(15, "Divigel", 84 * (0:5), 90),
  rx(15, "Utrogestan", 84 * (0:5), 90),
  rx(16, "Divigel", 84 * (0:5), 90),
  rx(16, "Cerazette", 84 * (0:5), 90),

  # 17 negative fddd -> dropped by the 2026 interval filter, with a warning
  rx(17, "Divigel", 0, -30),
  rx(17, "Divigel", 84, 90),

  # 18 product name absent from the ladder -> product_category stays NA
  rx(18, "Paracetamol", 0, 28)
)

fake_lmed_2026 <- rbindlist(parts, use.names = TRUE)

# Deterministic jitter, so the fixture is not a perfect lattice. Bounded to
# +/- 3 days and never applied to the long-episode people (1, 2, 6), whose
# episode lengths must stay above / below the 156-week threshold on purpose.
jitter_days <- sample(-3:3, nrow(fake_lmed_2026), replace = TRUE)
jitter_days[fake_lmed_2026$lopnr %in% c(1L, 2L, 6L)] <- 0L
fake_lmed_2026[, edatum := edatum + jitter_days]

setorder(fake_lmed_2026, lopnr, edatum, produkt)
setcolorder(fake_lmed_2026, c("lopnr", "produkt", "edatum", "fddd"))

# The 2023 entry point reads the id column under its registry name.
fake_lmed_2023 <- copy(fake_lmed_2026)
setnames(fake_lmed_2023, "lopnr", "p1163_lopnr_personnr")

# --- skeleton ----------------------------------------------------------------
fake_skeleton_mht <- CJ(
  id = sort(unique(fake_lmed_2026$lopnr)),
  isoyearweek = cstime::date_to_isoyearweek_c(grid_dates),
  unique = TRUE
)
setorder(fake_skeleton_mht, id, isoyearweek)
setDF(fake_skeleton_mht)
setDT(fake_skeleton_mht)

# --- write -------------------------------------------------------------------
# xz, not gzip: the gzip container records an mtime, which would make repeated
# runs differ byte-for-byte even from identical objects.
save(
  fake_lmed_2023,
  file = "data/fake_lmed_2023.rda",
  version = 3,
  compress = "xz"
)
save(
  fake_lmed_2026,
  file = "data/fake_lmed_2026.rda",
  version = 3,
  compress = "xz"
)
save(
  fake_skeleton_mht,
  file = "data/fake_skeleton_mht.rda",
  version = 3,
  compress = "xz"
)

cat("lmed rows:", nrow(fake_lmed_2026), "\n")
cat("skeleton rows:", nrow(fake_skeleton_mht), "\n")
cat(
  "isoyearweek span:",
  min(fake_skeleton_mht$isoyearweek),
  "..",
  max(fake_skeleton_mht$isoyearweek),
  "\n"
)
cat("distinct isoyearweeks:", uniqueN(fake_skeleton_mht$isoyearweek), "\n")
