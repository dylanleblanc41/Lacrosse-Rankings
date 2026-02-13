# ===== NCAA LACROSSE ELO RATING SYSTEM =====
# Based on FiveThirtyEight's NFL ELO (https://github.com/fivethirtyeight/nfl-elo-game)
# and nfelo (https://github.com/greerreNFL/nfelo)
source("utils.R")

# ============================================
# PARAMETERS
# ============================================
K_FACTOR <- 30
REVERSION <- 0.20
YEARS <- c(2023, 2024, 2025, 2026)

all_games <- list()
for (y in YEARS) {
  all_games[[as.character(y)]] <- load_games(y)
}

# ============================================
# ELO FUNCTIONS
# ============================================

calc_expected <- function(elo_diff) {
  1 / (1 + 10^(-elo_diff / 400))
}

calc_mov_mult <- function(point_diff, winner_elo, loser_elo) {
  log(abs(point_diff) + 1) * (2.2 / ((winner_elo - loser_elo) * 0.001 + 2.2))
}

revert_elos <- function(elo_vector) {
  (1 - REVERSION) * elo_vector + REVERSION * INITIAL_ELO
}

calculate_elo <- function(games, initial_elos = NULL) {
  
  if (nrow(games) == 0) return(list(ratings = data.frame(), elo_vector = NULL))
  
  games <- games[order(games$date), ]
  teams <- unique(c(games$away_team, games$home_team))
  
  if (is.null(initial_elos)) {
    elo <- setNames(rep(INITIAL_ELO, length(teams)), teams)
  } else {
    elo <- initial_elos
    new_teams <- setdiff(teams, names(elo))
    if (length(new_teams) > 0) elo[new_teams] <- INITIAL_ELO
  }
  
  for (i in 1:nrow(games)) {
    g <- games[i, ]
    if (is.na(g$away_score) || is.na(g$home_score)) next
    
    away <- g$away_team
    home <- g$home_team
    
    elo_diff <- elo[home] + HFA - elo[away]
    home_expected <- calc_expected(elo_diff)
    
    if (g$home_score > g$away_score) {
      home_actual <- 1
      winner_elo <- elo[home]
      loser_elo <- elo[away]
    } else {
      home_actual <- 0
      winner_elo <- elo[away]
      loser_elo <- elo[home]
    }
    
    point_diff <- abs(g$home_score - g$away_score)
    mov <- calc_mov_mult(point_diff, winner_elo, loser_elo)
    shift <- K_FACTOR * mov * (home_actual - home_expected)
    
    elo[home] <- elo[home] + shift
    elo[away] <- elo[away] - shift
  }
  
  results <- data.frame(
    team = names(elo),
    elo = round(as.numeric(elo), 1),
    stringsAsFactors = FALSE
  )
  results <- results[order(-results$elo), ]
  results$rank <- 1:nrow(results)
  results <- results[, c("rank", "team", "elo")]
  rownames(results) <- NULL
  
  return(list(ratings = results, elo_vector = elo))
}

evaluate_predictions <- function(games, initial_elos = NULL) {
  
  if (nrow(games) == 0) return(NULL)
  
  games <- games[order(games$date), ]
  teams <- unique(c(games$away_team, games$home_team))
  
  if (is.null(initial_elos)) {
    elo <- setNames(rep(INITIAL_ELO, length(teams)), teams)
  } else {
    elo <- initial_elos
    new_teams <- setdiff(teams, names(elo))
    if (length(new_teams) > 0) elo[new_teams] <- INITIAL_ELO
  }
  
  correct <- 0
  total <- 0
  
  for (i in 1:nrow(games)) {
    g <- games[i, ]
    if (is.na(g$away_score) || is.na(g$home_score)) next
    
    away <- g$away_team
    home <- g$home_team
    
    elo_diff <- elo[home] + HFA - elo[away]
    home_expected <- calc_expected(elo_diff)
    home_favored <- home_expected > 0.5
    home_won <- g$home_score > g$away_score
    
    if (home_favored == home_won) correct <- correct + 1
    total <- total + 1
    
    if (g$home_score > g$away_score) {
      home_actual <- 1
      winner_elo <- elo[home]
      loser_elo <- elo[away]
    } else {
      home_actual <- 0
      winner_elo <- elo[away]
      loser_elo <- elo[home]
    }
    
    point_diff <- abs(g$home_score - g$away_score)
    mov <- calc_mov_mult(point_diff, winner_elo, loser_elo)
    shift <- K_FACTOR * mov * (home_actual - home_expected)
    
    elo[home] <- elo[home] + shift
    elo[away] <- elo[away] - shift
  }
  
  return(list(correct = correct, total = total, accuracy = correct / total))
}

# ============================================
# FILTER FOR PRINTING ONLY
# ============================================

get_eligible_teams <- function(games, min_games = 8) {
  team_games <- table(c(games$away_team, games$home_team))
  eligible <- names(team_games[team_games >= min_games])
  if (length(eligible) < 25) {
    eligible <- names(team_games[team_games >= 1])
  }
  return(eligible)
}

print_elo <- function(elo_result, games, year) {
  if (nrow(elo_result$ratings) == 0) {
    cat(sprintf("\n%d: No completed games yet\n", year))
    return()
  }
  
  eligible <- get_eligible_teams(games)
  ratings <- elo_result$ratings[elo_result$ratings$team %in% eligible, ]
  ratings$rank <- 1:nrow(ratings)
  
  cat(sprintf("\n========================================\n"))
  cat(sprintf("%d ELO RANKINGS - TOP 25\n", year))
  cat(sprintf("========================================\n\n"))
  
  cat(sprintf("%-4s %-28s %s\n", "Rank", "Team", "ELO"))
  cat(paste(rep("-", 45), collapse = ""), "\n")
  
  for (i in 1:min(25, nrow(ratings))) {
    r <- ratings[i, ]
    cat(sprintf("%-4d %-28s %.1f\n", r$rank, r$team, r$elo))
  }
}

# ============================================
# RUN ELO FOR ALL YEARS
# ============================================

cat("\nCalculating ELO ratings...\n")

elo_results <- list()
elo_entering <- list()
evals <- list()

for (i in seq_along(YEARS)) {
  y <- as.character(YEARS[i])
  
  if (i == 1) {
    elo_results[[y]] <- calculate_elo(all_games[[y]])
    evals[[y]] <- evaluate_predictions(all_games[[y]])
  } else {
    elo_entering[[y]] <- revert_elos(elo_results[[as.character(YEARS[i-1])]]$elo_vector)
    elo_results[[y]] <- calculate_elo(all_games[[y]], initial_elos = elo_entering[[y]])
    evals[[y]] <- evaluate_predictions(all_games[[y]], elo_entering[[y]])
  }
  
  print_elo(elo_results[[y]], all_games[[y]], YEARS[i])
}

# ============================================
# PREDICTION ACCURACY
# ============================================

cat("\n========================================\n")
cat("PREDICTION ACCURACY\n")
cat("========================================\n")

for (y in as.character(YEARS)) {
  if (!is.null(evals[[y]])) {
    cat(sprintf("%s: %.1f%% (%d/%d)\n", y, evals[[y]]$accuracy * 100, evals[[y]]$correct, evals[[y]]$total))
  }
}

# ============================================
# SAVE (ALL teams, no filter - filtering is for display only)
# ============================================

for (y in as.character(YEARS)) {
  if (nrow(elo_results[[y]]$ratings) > 0) {
    write.csv(elo_results[[y]]$ratings, paste0("elo_", y, ".csv"), row.names = FALSE)
  }
}

cat("\nSaved ELO ratings for all years.\n")