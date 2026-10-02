# add_lmed_v20261002() adds approach 5 to the 2026-09-24 answer. Approach 5
# pools the oestrogen route of approach 4 and gives the hormonal IUD (E1) its
# own level. Every other output must equal the 2026-09-24 answer, and one call
# must open only the two 2026-10-02 workbooks.
#
# Tests 1 to 12 each run one case of CASES below. Grid week 1 starts on Monday
# 2016-01-04, and every dispensing falls on a Monday. A supply of d days from
# grid week k then covers grid weeks k to k + d / 7 - 1.
#
# Each dispensing sets its fddd, so no minimum-dose rule drops it. Durations
# follow the MHT_groups sheet. Utrogestan 100 mg covers floor(fddd / 28) * 28
# days. Provera covers floor(fddd / 4) * 28 days, and needs 3 months. Mirena
# covers 1680 days whatever its fddd, which is past the end of the grid. Every
# other product used here covers fddd days.
#
# Each test asserts the category columns first, then `approach5`, then
# `rd_approach5_single`. A failure on `approach5` with green category
# assertions is then a fault of the approach 5 rules, not of the input.

UTROGESTAN_LNMN <- "Utrogestan, kapsel, mjuk 100 mg"
GRID_FIRST <- as.Date("2016-01-04")
GRID_WEEKS <- 26L

# The Monday that starts grid week `k`.
monday <- function(k) {
  return(GRID_FIRST + 7L * (as.integer(k) - 1L))
}

# The ISO week of grid week `k`.
grid_week <- function(k) {
  return(cstime::date_to_isoyearweek_c(monday(k)))
}

fixture <- function() {
  skeleton <- data.table::copy(mht::fake_skeleton_mht)
  lmed <- data.table::setDT(data.table::copy(mht::fake_lmed_2026))
  lmed[, lnmn := NA_character_]
  lmed[produkt == "Utrogestan", lnmn := UTROGESTAN_LNMN]
  # `Paracetamol` is a fixture-only name. The table is built from the real
  # deliveries, which write branded spellings instead.
  lmed <- lmed[produkt != "Paracetamol"]
  return(list(skeleton = skeleton, lmed = lmed))
}

# A dropped prescription warns. The fixture holds one negative-duration row.
# Muffle only that warning, so any other warning still reaches the reporter.
muffle_dropped <- function(expr) {
  return(withCallingHandlers(expr, warning = function(w) {
    if (grepl("prescriptions dropped", conditionMessage(w), fixed = TRUE)) {
      invokeRestart("muffleWarning")
    }
  }))
}

# Consecutive ISO weeks, the same run for every id, from the Monday `first`.
weekly_grid <- function(ids, first = "2016-01-04", n = GRID_WEEKS) {
  weeks <- cstime::date_to_isoyearweek_c(
    seq(as.Date(first), by = 7, length.out = n)
  )
  return(data.table::data.table(
    id = rep(ids, each = n),
    isoyearweek = rep(weeks, times = length(ids))
  ))
}

# One dispensing per element of `produkt`, all for person 1. Utrogestan needs
# `lnmn` for its strength. Every other product used here needs none, so its
# `lnmn` is NA.
dispensings <- function(produkt, week, fddd, lopnr = 1L) {
  lnmn <- ifelse(produkt == "Utrogestan", UTROGESTAN_LNMN, NA_character_)
  return(data.table::data.table(
    lopnr = lopnr,
    produkt = produkt,
    edatum = monday(week),
    fddd = fddd,
    lnmn = lnmn
  ))
}

# The input of each known-answer case. Unless the test says otherwise, every
# supply starts in grid week 2 and covers weeks 2 to 9.
CASES <- list(
  divigel_mirena = dispensings(c("Divigel", "Mirena"), c(2, 2), c(56, 1)),
  progynon_mirena = dispensings(c("Progynon", "Mirena"), c(2, 2), c(56, 1)),
  # Provera fddd 12 covers 84 days, weeks 2 to 13.
  progynon_provera = dispensings(c("Progynon", "Provera"), c(2, 2), c(56, 12)),
  divigel_minipe = dispensings(c("Divigel", "Mini-Pe"), c(2, 2), c(56, 56)),
  activelle = dispensings("Activelle", 2, 56),
  estalis = dispensings("Estalis", 2, 56),
  divigel_utrogestan = dispensings(
    c("Divigel", "Utrogestan"),
    c(2, 2),
    c(56, 56)
  ),
  femoston = dispensings("Femoston", 2, 56),
  mirena = dispensings("Mirena", 2, 1),
  divigel_prolutex = dispensings(c("Divigel", "Prolutex"), c(2, 2), c(56, 56)),
  # Divigel fddd 182 covers 182 days, weeks 1 to 26.
  divigel_then_mirena = dispensings(c("Divigel", "Mirena"), c(1, 5), c(182, 1)),
  # Divigel fddd 84 and Provera fddd 12 each cover 84 days, weeks 1 to 12.
  mirena_provera_divigel = dispensings(
    c("Mirena", "Provera", "Divigel"),
    c(1, 1, 1),
    c(1, 12, 84)
  )
)

# One call of the new entry point on the grid, for every person.
run5 <- function(lmed) {
  skeleton <- weekly_grid(sort(unique(lmed$lopnr)))
  add_lmed_v20261002(skeleton, lmed, verbose = FALSE)
  return(skeleton)
}

# Column `col` of one person in grid weeks `weeks`.
in_weeks <- function(out, person, weeks, col) {
  return(out[id == person][match(grid_week(weeks), isoyearweek)][[col]])
}

expect_level <- function(out, person, weeks, level, col = "approach5") {
  expect_identical(
    in_weeks(out, person, weeks, col),
    rep(level, length(weeks)),
    info = paste("id", person, col)
  )
}

# The category columns of one person: TRUE in `weeks`, FALSE in every other
# grid week.
expect_category <- function(out, person, category, weeks) {
  expect_level(out, person, weeks, TRUE, col = category)
  rest <- setdiff(seq_len(GRID_WEEKS), weeks)
  expect_level(out, person, rest, FALSE, col = category)
}

# One treated episode in grid weeks 2 to 9. `approach5` holds `level` in those
# weeks and `local_or_none_mht` around them. Eight treated weeks do not reach
# 156, so `rd_approach5_single` holds `exclude` after the stop.
expect_episode <- function(out, person, level) {
  expect_level(out, person, 1, "local_or_none_mht")
  expect_level(out, person, 2:9, level)
  expect_level(out, person, 10:GRID_WEEKS, "local_or_none_mht")
  rd <- "rd_approach5_single"
  expect_level(out, person, 1, "local_or_none_mht", col = rd)
  expect_level(out, person, 2:9, level, col = rd)
  expect_level(out, person, 10:GRID_WEEKS, "exclude", col = rd)
}

# No week lights a level, so both columns hold `local_or_none_mht` throughout.
expect_untreated <- function(out, person) {
  weeks <- seq_len(GRID_WEEKS)
  expect_level(out, person, weeks, "local_or_none_mht")
  rd <- "rd_approach5_single"
  expect_level(out, person, weeks, "local_or_none_mht", col = rd)
}

test_that("1. Divigel + Mirena is estrogen_iud", {
  out <- run5(CASES$divigel_mirena)
  expect_category(out, 1L, "A1", 2:9)
  expect_category(out, 1L, "E1", 2:GRID_WEEKS)
  expect_episode(out, 1L, "estrogen_iud")
})

test_that("2. Progynon + Mirena is estrogen_iud", {
  out <- run5(CASES$progynon_mirena)
  expect_category(out, 1L, "A2", 2:9)
  expect_category(out, 1L, "E1", 2:GRID_WEEKS)
  expect_episode(out, 1L, "estrogen_iud")
})

test_that("3. Progynon + Provera is estrogen_other_progestogen", {
  out <- run5(CASES$progynon_provera)
  expect_category(out, 1L, "A2", 2:9)
  expect_category(out, 1L, "C4", 2:13)
  # Provera alone in weeks 10 to 13 lights no level.
  expect_episode(out, 1L, "estrogen_other_progestogen")
})

test_that("4. Divigel + Mini-Pe is estrogen_other_progestogen", {
  out <- run5(CASES$divigel_minipe)
  expect_category(out, 1L, "A1", 2:9)
  expect_category(out, 1L, "I1", 2:9)
  expect_episode(out, 1L, "estrogen_other_progestogen")
})

test_that("5. Activelle alone is estrogen_other_progestogen", {
  out <- run5(CASES$activelle)
  expect_category(out, 1L, "B2", 2:9)
  expect_episode(out, 1L, "estrogen_other_progestogen")
})

test_that("6. Estalis alone is estrogen_other_progestogen", {
  out <- run5(CASES$estalis)
  expect_category(out, 1L, "B1", 2:9)
  expect_episode(out, 1L, "estrogen_other_progestogen")
})

test_that("7. Divigel + Utrogestan is estrogen_progesterone", {
  out <- run5(CASES$divigel_utrogestan)
  expect_category(out, 1L, "A1", 2:9)
  expect_category(out, 1L, "C1", 2:9)
  expect_episode(out, 1L, "estrogen_progesterone")
})

test_that("8. Femoston alone is estrogen_progesterone", {
  out <- run5(CASES$femoston)
  expect_category(out, 1L, "B11", 2:9)
  expect_episode(out, 1L, "estrogen_progesterone")
})

test_that("9. Mirena alone is local_or_none_mht", {
  out <- run5(CASES$mirena)
  # The product covers the weeks, so the level is not an absent row.
  expect_category(out, 1L, "E1", 2:GRID_WEEKS)
  expect_untreated(out, 1L)
})

test_that("10. Divigel + Prolutex is flagged and local_or_none_mht", {
  out <- run5(CASES$divigel_prolutex)
  expect_category(out, 1L, "A1", 2:9)
  expect_category(out, 1L, "C2", 2:9)
  expect_true(all(out[id == 1L]$ri_mht_excluded_product))
  expect_untreated(out, 1L)
})

test_that("11. Divigel, then Mirena from week 5, moves to estrogen_iud", {
  out <- run5(CASES$divigel_then_mirena)
  expect_category(out, 1L, "A1", 1:GRID_WEEKS)
  expect_category(out, 1L, "E1", 5:GRID_WEEKS)
  expect_level(out, 1L, 1:4, "estrogen_only")
  expect_level(out, 1L, 5:GRID_WEEKS, "estrogen_iud")
  # One episode with no stop, so the exposure columns copy approach5.
  rd <- "rd_approach5_single"
  expect_level(out, 1L, 1:4, "estrogen_only", col = rd)
  expect_level(out, 1L, 5:GRID_WEEKS, "estrogen_iud", col = rd)
})

test_that("12. Mirena + Provera + Divigel in one week is a clash", {
  # Divigel lights estrogen_iud with Mirena and estrogen_other_progestogen with
  # Provera, both in week 1. The clash lasts to the end of the episode.
  out <- run5(CASES$mirena_provera_divigel)
  expect_category(out, 1L, "A1", 1:12)
  expect_category(out, 1L, "C4", 1:12)
  expect_category(out, 1L, "E1", 1:GRID_WEEKS)
  expect_level(out, 1L, 1:12, "clashingprescriptions")
  expect_level(out, 1L, 13:GRID_WEEKS, "local_or_none_mht")
  rd <- "rd_approach5_single"
  expect_level(out, 1L, 1:12, "clashingprescriptions", col = rd)
  expect_level(out, 1L, 13:GRID_WEEKS, "exclude", col = rd)
})

# Every case of CASES in one input, one person per case.
mixed_cohort <- function() {
  lmed <- data.table::rbindlist(CASES, idcol = "case")
  lmed[, lopnr := match(case, names(CASES))]
  lmed[, case := NULL]
  return(list(skeleton = weekly_grid(seq_along(CASES)), lmed = lmed))
}

test_that("13. every 2026-09-24 column is unchanged, and three are added", {
  inputs <- list(fixture = fixture, mixed = mixed_cohort)
  for (input in names(inputs)) {
    f_new <- inputs[[input]]()
    new <- muffle_dropped(
      add_lmed_v20261002(f_new$skeleton, f_new$lmed, verbose = FALSE)
    )
    f_old <- inputs[[input]]()
    old <- muffle_dropped(
      add_lmed_v20260924(f_old$skeleton, f_old$lmed, verbose = FALSE)
    )
    added <- setdiff(names(new), names(old))
    expect_length(added, 3L)
    expect_setequal(
      added,
      c("approach5", "rd_approach5_single", "rd_approach5_multiple")
    )
    expect_identical(setdiff(names(new), added), names(old), info = input)
    for (k in names(old)) {
      expect_identical(new[[k]], old[[k]], info = paste(input, k))
    }
  }
})

test_that("14. one call opens only the two 2026-10-02 workbooks", {
  lmed <- dispensings(c("Divigel", "Mirena"), c(2, 2), c(56, 1))
  skeleton <- weekly_grid(1L)
  seen <- new.env()
  seen$files <- character(0)
  record <- bquote(assign(
    "files",
    c(get("files", envir = .(seen)), basename(path)),
    envir = .(seen)
  ))
  ns <- asNamespace("readxl")
  for (fun in c("read_excel", "excel_sheets")) {
    suppressMessages(trace(fun, tracer = record, where = ns, print = FALSE))
  }
  withr::defer(for (fun in c("read_excel", "excel_sheets")) {
    suppressMessages(untrace(fun, where = ns))
  })
  add_lmed_v20261002(skeleton, lmed, verbose = FALSE)
  expect_identical(
    sort(unique(seen$files)),
    c("dataDictionary20261002.xlsx", "product_table_20261002.xlsx")
  )
})
