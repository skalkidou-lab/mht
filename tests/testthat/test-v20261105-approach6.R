# add_lmed_v20261105() adds approach 6 to the 2026-10-02 answer. Approach 6 is
# approach 1 with local treatment (A3, A4, A5) as the treatment `local_mht`.
# Its reference level is `no_local_or_systemic_mht`. Every other output must
# equal the 2026-10-02 answer, and one call must open only the two 2026-11-05
# workbooks.
#
# Grid week 1 starts on Monday 2016-01-04, and every dispensing falls on a
# Monday. A supply of d days from grid week k covers grid weeks k to
# k + d / 7 - 1. No product used here takes a minimum-dose rule or a fixed
# duration, so each covers fddd days.
#
# A known-answer test asserts the category columns first, then `approach6`,
# then `rd_approach6_single`. A failure on `approach6` with green category
# assertions is then a fault of the approach 6 layer, not of the input.

UTROGESTAN_LNMN <- "Utrogestan, kapsel, mjuk 100 mg"
GRID_FIRST <- as.Date("2016-01-04")
GRID_WEEKS <- 26L
NONE6 <- "no_local_or_systemic_mht"

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
weekly_grid <- function(ids, n = GRID_WEEKS) {
  weeks <- grid_week(seq_len(n))
  return(data.table::data.table(
    id = rep(ids, each = n),
    isoyearweek = rep(weeks, times = length(ids))
  ))
}

# One dispensing per element of `produkt`, all for person `lopnr`. Utrogestan
# needs `lnmn` for its strength. Every other product used here needs none, so
# its `lnmn` is NA.
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

# The input of each known-answer case. Unless the case says otherwise, every
# supply starts in grid week 2 and covers weeks 2 to 9.
CASES <- list(
  vagifem = dispensings("Vagifem", 2, 56),
  ovesterin = dispensings("Ovesterin", 2, 56),
  intrarosa = dispensings("Intrarosa", 2, 56),
  oestriol = dispensings("Oestriol Aspen", 2, 56),
  vagifem_divigel = dispensings(c("Vagifem", "Divigel"), c(2, 2), c(56, 56)),
  divigel = dispensings("Divigel", 2, 56),
  livial = dispensings("Livial", 2, 56),
  vagifem_livial = dispensings(c("Vagifem", "Livial"), c(2, 2), c(56, 56)),
  # Vagifem covers weeks 2 to 13, and Divigel weeks 2 to 9.
  divigel_then_vagifem = dispensings(
    c("Vagifem", "Divigel"),
    c(2, 2),
    c(84, 56)
  ),
  # Vagifem covers weeks 2 to 9, then weeks 15 to 18, after a 5-week gap.
  vagifem_restart = dispensings(c("Vagifem", "Vagifem"), c(2, 15), c(56, 28))
)

# One call of the new entry point on the grid. `ids` defaults to every person
# of `lmed`.
run6 <- function(lmed, ids = sort(unique(lmed$lopnr)), n = GRID_WEEKS) {
  skeleton <- weekly_grid(ids, n)
  add_lmed_v20261105(skeleton, lmed, verbose = FALSE)
  return(skeleton)
}

# Column `col` of one person in grid weeks `weeks`.
in_weeks <- function(out, person, weeks, col) {
  return(out[id == person][match(grid_week(weeks), isoyearweek)][[col]])
}

expect_level <- function(out, person, weeks, level, col = "approach6") {
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

# One treated episode in grid weeks 2 to 9. `approach6` holds `level` in those
# weeks and the reference level around them. Eight treated weeks do not reach
# 156, so `rd_approach6_single` holds `exclude` after the stop.
expect_episode <- function(out, person, level) {
  expect_level(out, person, 1, NONE6)
  expect_level(out, person, 2:9, level)
  expect_level(out, person, 10:GRID_WEEKS, NONE6)
  rd <- "rd_approach6_single"
  expect_level(out, person, 1, NONE6, col = rd)
  expect_level(out, person, 2:9, level, col = rd)
  expect_level(out, person, 10:GRID_WEEKS, "exclude", col = rd)
}

# Approach 1 reads a local treatment as untreated, in every grid week.
expect_untreated_in_approach1 <- function(out, person) {
  weeks <- seq_len(GRID_WEEKS)
  expect_level(out, person, weeks, "local_or_none_mht", col = "approach1")
  rd <- "rd_approach1_single"
  expect_level(out, person, weeks, "local_or_none_mht", col = rd)
}

test_that("1. Vagifem (A3, vaginal estradiol) alone is local_mht", {
  out <- run6(CASES$vagifem)
  expect_category(out, 1L, "A3", 2:9)
  expect_episode(out, 1L, "local_mht")
  expect_untreated_in_approach1(out, 1L)
})

test_that("2. Ovesterin and Intrarosa (A4) alone are local_mht", {
  for (case in c("ovesterin", "intrarosa")) {
    out <- run6(CASES[[case]])
    expect_category(out, 1L, "A4", 2:9)
    expect_episode(out, 1L, "local_mht")
    expect_untreated_in_approach1(out, 1L)
  }
})

test_that("3. Oestriol Aspen (A5, oral estriol) alone is local_mht", {
  out <- run6(CASES$oestriol)
  expect_category(out, 1L, "A5", 2:9)
  expect_episode(out, 1L, "local_mht")
  expect_untreated_in_approach1(out, 1L)
})

test_that("4. a woman with no product is no_local_or_systemic_mht", {
  # Person 2 is in the skeleton and holds no dispensing.
  out <- run6(CASES$vagifem, ids = 1:2)
  weeks <- seq_len(GRID_WEEKS)
  for (category in c("A1", "A2", "A3", "A4", "A5", "A6", "F1")) {
    expect_level(out, 2L, weeks, FALSE, col = category)
  }
  for (col in c("approach6", "rd_approach6_single", "rd_approach6_multiple")) {
    expect_level(out, 2L, weeks, NONE6, col = col)
  }
  for (col in c(paste0("approach", 1:5), "rd_approach1_single")) {
    expect_level(out, 2L, weeks, "local_or_none_mht", col = col)
  }
})

test_that("5. Vagifem + Divigel in one week is systemic_mht", {
  out <- run6(CASES$vagifem_divigel)
  expect_category(out, 1L, "A3", 2:9)
  expect_category(out, 1L, "A1", 2:9)
  expect_episode(out, 1L, "systemic_mht")
  # Divigel alone gives the same approach 6 columns.
  alone <- run6(CASES$divigel)
  for (col in c("approach6", "rd_approach6_single", "rd_approach6_multiple")) {
    expect_identical(out[[col]], alone[[col]], info = col)
  }
})

test_that("6. Livial (F1, tibolone) alone is tibolone", {
  out <- run6(CASES$livial)
  expect_category(out, 1L, "F1", 2:9)
  expect_episode(out, 1L, "tibolone")
})

test_that("7. Vagifem + Livial in one week is tibolone", {
  out <- run6(CASES$vagifem_livial)
  expect_category(out, 1L, "A3", 2:9)
  expect_category(out, 1L, "F1", 2:9)
  expect_episode(out, 1L, "tibolone")
})

test_that("8. Vagifem that outlasts Divigel moves to local_mht, not a stop", {
  out <- run6(CASES$divigel_then_vagifem)
  expect_category(out, 1L, "A3", 2:13)
  expect_category(out, 1L, "A1", 2:9)
  expect_level(out, 1L, 1, NONE6)
  expect_level(out, 1L, 2:9, "systemic_mht")
  expect_level(out, 1L, 10:13, "local_mht")
  expect_level(out, 1L, 14:GRID_WEEKS, NONE6)
  # A move between two active levels gives no `previous` week.
  rd <- "rd_approach6_single"
  expect_level(out, 1L, 2:9, "systemic_mht", col = rd)
  expect_level(out, 1L, 10:13, "local_mht", col = rd)
  expect_level(out, 1L, 14:GRID_WEEKS, "exclude", col = rd)
  # Approach 1 stops in week 9.
  expect_level(out, 1L, 10:GRID_WEEKS, "exclude", col = "rd_approach1_single")
})

test_that("9. a restart of local_mht is exclude in single, local_mht in multiple", {
  out <- run6(CASES$vagifem_restart)
  expect_category(out, 1L, "A3", c(2:9, 15:18))
  expect_level(out, 1L, 2:9, "local_mht")
  expect_level(out, 1L, 10:14, NONE6)
  expect_level(out, 1L, 15:18, "local_mht")
  single <- "rd_approach6_single"
  expect_level(out, 1L, 2:9, "local_mht", col = single)
  expect_level(out, 1L, 10:GRID_WEEKS, "exclude", col = single)
  multiple <- "rd_approach6_multiple"
  expect_level(out, 1L, 2:9, "local_mht", col = multiple)
  expect_level(out, 1L, 10:14, "exclude", col = multiple)
  expect_level(out, 1L, 15:18, "local_mht", col = multiple)
  expect_level(out, 1L, 19:GRID_WEEKS, "exclude", col = multiple)
})

# One supply of `produkt` that starts in grid week 2 and covers `weeks` weeks,
# then nothing, on a grid of 170 weeks.
long_run <- function(produkt, weeks) {
  return(run6(dispensings(produkt, 2, 7 * weeks), n = 170L))
}

test_that("10. rd_approach6_multiple: 156 weeks of local_mht, then a stop, is previous", {
  out <- long_run("Vagifem", 156)
  col <- "rd_approach6_multiple"
  expect_identical(
    out[[col]],
    rep(c(NONE6, "local_mht", "previous"), c(1L, 156L, 13L))
  )
})

test_that("11. rd_approach6_multiple: 155 weeks of local_mht, then a stop, is exclude", {
  out <- long_run("Vagifem", 155)
  col <- "rd_approach6_multiple"
  expect_identical(
    out[[col]],
    rep(c(NONE6, "local_mht", "exclude"), c(1L, 155L, 14L))
  )
})

test_that("12. the 156-week rule of local_mht is that of systemic_mht", {
  for (weeks in c(155, 156)) {
    local <- long_run("Vagifem", weeks)
    systemic <- long_run("Divigel", weeks)
    for (col in c("rd_approach6_single", "rd_approach6_multiple")) {
      expect_identical(
        sub("^local_mht$", "systemic_mht", local[[col]]),
        systemic[[col]],
        info = paste(weeks, col)
      )
    }
  }
})

# Every case of CASES in one input, one person per case, and one person with
# no dispensing.
mixed_cohort <- function() {
  lmed <- data.table::rbindlist(CASES, idcol = "case")
  lmed[, lopnr := match(case, names(CASES))]
  lmed[, case := NULL]
  ids <- seq_len(length(CASES) + 1L)
  return(list(skeleton = weekly_grid(ids), lmed = lmed))
}

test_that("13. every 2026-10-02 column is unchanged, and three are added", {
  inputs <- list(fixture = fixture, mixed = mixed_cohort)
  for (input in names(inputs)) {
    f_new <- inputs[[input]]()
    new <- muffle_dropped(
      add_lmed_v20261105(f_new$skeleton, f_new$lmed, verbose = FALSE)
    )
    f_old <- inputs[[input]]()
    old <- muffle_dropped(
      add_lmed_v20261002(f_old$skeleton, f_old$lmed, verbose = FALSE)
    )
    added <- setdiff(names(new), names(old))
    expect_length(added, 3L)
    expect_setequal(
      added,
      c("approach6", "rd_approach6_single", "rd_approach6_multiple")
    )
    expect_identical(setdiff(names(new), added), names(old), info = input)
    for (k in names(old)) {
      expect_identical(new[[k]], old[[k]], info = paste(input, k))
    }
    expect_identical(new[, names(old), with = FALSE], old, info = input)
    # The input exercises approach 6: every level and both reference levels.
    expect_true(
      all(c("local_mht", "systemic_mht", "tibolone", NONE6) %in% new$approach6),
      info = input
    )
  }
})

test_that("14. one call opens only the two 2026-11-05 workbooks", {
  lmed <- dispensings(c("Vagifem", "Livial"), c(2, 2), c(56, 56))
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
  withr::defer(
    for (fun in c("read_excel", "excel_sheets")) {
      suppressMessages(untrace(fun, where = ns))
    }
  )
  add_lmed_v20261105(skeleton, lmed, verbose = FALSE)
  expect_identical(
    sort(unique(seen$files)),
    c("dataDictionary20261105.xlsx", "product_table_20261105.xlsx")
  )
})
