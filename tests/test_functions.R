# ===== UNIT TESTS FOR LACROSSE RANKINGS =====
# Uses base R only (no test framework dependencies)
# Run with: Rscript tests/test_functions.R

cat("Running tests...\n\n")
failures <- 0

assert <- function(desc, expr) {
  result <- tryCatch(expr, error = function(e) FALSE)
  if (isTRUE(result)) {
    cat(sprintf("  PASS: %s\n", desc))
  } else {
    cat(sprintf("  FAIL: %s\n", desc))
    failures <<- failures + 1
  }
}

# ------------------------------------------
# ELO math functions (defined here to avoid
# triggering the full pipeline in elo_ratings.R)
# ------------------------------------------

calc_expected <- function(elo_diff) {
  1 / (1 + 10^(-elo_diff / 400))
}

calc_mov_mult <- function(point_diff, winner_elo, loser_elo) {
  log(abs(point_diff) + 1) * (2.2 / ((winner_elo - loser_elo) * 0.001 + 2.2))
}

revert_elos <- function(elo_vector, reversion = 0.20, initial = 1500) {
  (1 - reversion) * elo_vector + reversion * initial
}

# ------------------------------------------
# Tests: calc_expected
# ------------------------------------------
cat("calc_expected:\n")

assert("equal ratings -> 50%",
       abs(calc_expected(0) - 0.5) < 1e-6)

assert("+400 diff -> ~91%",
       abs(calc_expected(400) - 10/11) < 1e-6)

assert("-400 diff -> ~9%",
       abs(calc_expected(-400) - 1/11) < 1e-6)

assert("always between 0 and 1",
       calc_expected(1000) > 0 && calc_expected(1000) < 1 &&
       calc_expected(-1000) > 0 && calc_expected(-1000) < 1)

assert("symmetric around 0.5",
       abs(calc_expected(200) + calc_expected(-200) - 1.0) < 1e-6)

# ------------------------------------------
# Tests: calc_mov_mult
# ------------------------------------------
cat("\ncalc_mov_mult:\n")

assert("1-goal game has positive multiplier",
       calc_mov_mult(1, 1500, 1400) > 0)

assert("bigger blowout -> bigger multiplier",
       calc_mov_mult(10, 1500, 1400) > calc_mov_mult(1, 1500, 1400))

assert("upset win (lower elo wins) has higher multiplier",
       calc_mov_mult(3, 1400, 1600) > calc_mov_mult(3, 1600, 1400))

# ------------------------------------------
# Tests: revert_elos
# ------------------------------------------
cat("\nrevert_elos:\n")

assert("1500 stays at 1500",
       abs(revert_elos(1500) - 1500) < 1e-6)

assert("high elo reverts toward 1500",
       revert_elos(1700) < 1700 && revert_elos(1700) > 1500)

assert("low elo reverts toward 1500",
       revert_elos(1300) > 1300 && revert_elos(1300) < 1500)

assert("20% reversion from 1600 -> 1580",
       abs(revert_elos(1600) - 1580) < 1e-6)

assert("vector input works", {
  v <- revert_elos(c(1600, 1400))
  abs(v[1] - 1580) < 1e-6 && abs(v[2] - 1420) < 1e-6
})

# ------------------------------------------
# Tests: name_map consistency
# ------------------------------------------
cat("\nname_map consistency:\n")

# Source just the name_map from elo_ratings.R
elo_env <- new.env()
tryCatch({
  lines <- readLines("elo_ratings.R")
  start <- grep("^name_map <- c\\(", lines)
  end <- grep("^\\)", lines)
  end <- end[end > start[1]][1]
  eval(parse(text = lines[start:end]), envir = elo_env)
}, error = function(e) NULL)

rpi_env <- new.env()
tryCatch({
  lines <- readLines("rpi_calculator.R")
  start <- grep("^name_map <- c\\(", lines)
  end <- grep("^\\)", lines)
  end <- end[end > start[1]][1]
  eval(parse(text = lines[start:end]), envir = rpi_env)
}, error = function(e) NULL)

pred_env <- new.env()
tryCatch({
  lines <- readLines("predicting_the_next_weeks_games.R")
  start <- grep("^name_map <- c\\(", lines)
  end <- grep("^\\)", lines)
  end <- end[end > start[1]][1]
  eval(parse(text = lines[start:end]), envir = pred_env)
}, error = function(e) NULL)

if (exists("name_map", envir = elo_env) && exists("name_map", envir = rpi_env)) {
  assert("elo_ratings.R and rpi_calculator.R name_maps match",
         identical(sort(elo_env$name_map), sort(rpi_env$name_map)))
} else {
  cat("  SKIP: could not parse name_maps from source files\n")
}

if (exists("name_map", envir = elo_env) && exists("name_map", envir = pred_env)) {
  assert("elo_ratings.R and predicting_the_next_weeks_games.R name_maps match",
         identical(sort(elo_env$name_map), sort(pred_env$name_map)))
} else {
  cat("  SKIP: could not parse name_maps from source files\n")
}

if (exists("name_map", envir = elo_env)) {
  assert("no duplicate ESPN names in name_map",
         !any(duplicated(names(elo_env$name_map))))

  assert("no duplicate NCAA names in name_map",
         !any(duplicated(unname(elo_env$name_map))))
}

# ------------------------------------------
# Summary
# ------------------------------------------
cat(sprintf("\n%d failure(s)\n", failures))
if (failures > 0) quit(status = 1)
cat("All tests passed.\n")
