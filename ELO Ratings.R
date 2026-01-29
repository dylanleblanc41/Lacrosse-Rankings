# ===== NCAA LACROSSE ELO RATING SYSTEM =====
# Based on FiveThirtyEight's NFL ELO (https://github.com/fivethirtyeight/nfl-elo-game)
# and nfelo (https://github.com/greerreNFL/nfelo)

# ============================================
# PARAMETERS
# ============================================
K_FACTOR <- 20          # How reactive ratings are 538 uses 20
HFA <- 55               # Home field advantage in Elo points 538 uses 65, but home field mens more in football than lacrosse
INITIAL_ELO <- 1500     # Starting rating
REVERSION <- 1/3        # Revert to mean between seasons

# ============================================
# LOAD DATA
# ============================================
games_2023 <- read.csv("ncaa_lax_2023.csv", stringsAsFactors = FALSE)
games_2024 <- read.csv("ncaa_lax_2024.csv", stringsAsFactors = FALSE)
games_2025 <- read.csv("ncaa_lax_2025.csv", stringsAsFactors = FALSE)
games_2026 <- tryCatch(
  read.csv("ncaa_lax_2026.csv", stringsAsFactors = FALSE),
  error = function(e) data.frame()
)

games_2023$date <- as.Date(games_2023$date)
games_2024$date <- as.Date(games_2024$date)
games_2025$date <- as.Date(games_2025$date)
if (nrow(games_2026) > 0) games_2026$date <- as.Date(games_2026$date)

cat(sprintf("2023: %d games\n", nrow(games_2023)))
cat(sprintf("2024: %d games\n", nrow(games_2024)))
cat(sprintf("2025: %d games\n", nrow(games_2025)))
cat(sprintf("2026: %d games\n", nrow(games_2026)))

# ============================================
# ELO FUNCTIONS
# ============================================

# Win probability from Elo difference
calc_expected <- function(elo_diff) {
  1 / (1 + 10^(-elo_diff / 400))
}

# Margin of victory multiplier (538 formula)
calc_mov_mult <- function(point_diff, winner_elo, loser_elo) {
  log(abs(point_diff) + 1) * (2.2 / ((winner_elo - loser_elo) * 0.001 + 2.2))
}

# Main ELO calculation
calculate_elo <- function(games, initial_elos = NULL) {
  
  if (nrow(games) == 0) return(list(ratings = data.frame(), elo_vector = NULL))
  
  games <- games[order(games$date), ]
  teams <- unique(c(games$away_team, games$home_team))
  
  # Initialize
  if (is.null(initial_elos)) {
    elo <- setNames(rep(INITIAL_ELO, length(teams)), teams)
  } else {
    elo <- initial_elos
    new_teams <- setdiff(teams, names(elo))
    if (length(new_teams) > 0) elo[new_teams] <- INITIAL_ELO
  }
  
  # Process games
  for (i in 1:nrow(games)) {
    g <- games[i, ]
    if (is.na(g$away_score) || is.na(g$home_score)) next
    
    away <- g$away_team
    home <- g$home_team
    
    # Elo diff with home advantage
    elo_diff <- elo[home] + HFA - elo[away]
    home_expected <- calc_expected(elo_diff)
    
    # Result
    if (g$home_score > g$away_score) {
      home_actual <- 1
      winner_elo <- elo[home]
      loser_elo <- elo[away]
    } else {
      home_actual <- 0
      winner_elo <- elo[away]
      loser_elo <- elo[home]
    }
    
    # MOV multiplier and shift
    point_diff <- abs(g$home_score - g$away_score)
    mov <- calc_mov_mult(point_diff, winner_elo, loser_elo)
    shift <- K_FACTOR * mov * (home_actual - home_expected)
    
    # Update (zero-sum)
    elo[home] <- elo[home] + shift
    elo[away] <- elo[away] - shift
  }
  
  # Build rankings
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

# Season reversion
revert_elos <- function(elo_vector) {
  (1 - REVERSION) * elo_vector + REVERSION * INITIAL_ELO
}

# ============================================
# RUN ELO
# ============================================

cat("\nCalculating ELO ratings...\n")

# 2023
elo_2023 <- calculate_elo(games_2023)

# 2024 (starting with reverted 2023)
elos_entering_2024 <- revert_elos(elo_2023$elo_vector)
elo_2024 <- calculate_elo(games_2024, initial_elos = elos_entering_2024)

# 2025 (starting with reverted 2024)
elos_entering_2025 <- revert_elos(elo_2024$elo_vector)
elo_2025 <- calculate_elo(games_2025, initial_elos = elos_entering_2025)

# 2026 (starting with reverted 2025, may be empty)
elos_entering_2026 <- revert_elos(elo_2025$elo_vector)
elo_2026 <- calculate_elo(games_2026, initial_elos = elos_entering_2026)

# ============================================
# PRINT TOP 25
# ============================================

print_elo <- function(elo_result, year) {
  if (nrow(elo_result$ratings) == 0) {
    cat(sprintf("\n%d: No games yet\n", year))
    return()
  }
  
  cat(sprintf("\n========================================\n"))
  cat(sprintf("%d ELO RANKINGS - TOP 25\n", year))
  cat(sprintf("========================================\n\n"))
  
  cat(sprintf("%-4s %-28s %s\n", "Rank", "Team", "ELO"))
  cat(paste(rep("-", 45), collapse = ""), "\n")
  
  for (i in 1:min(25, nrow(elo_result$ratings))) {
    r <- elo_result$ratings[i, ]
    cat(sprintf("%-4d %-28s %.1f\n", r$rank, r$team, r$elo))
  }
}

print_elo(elo_2023, 2023)
print_elo(elo_2024, 2024)
print_elo(elo_2025, 2025)
print_elo(elo_2026, 2026)

# ============================================
# PREDICTION ACCURACY
# ============================================

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
    
    # Pre-game prediction
    elo_diff <- elo[home] + HFA - elo[away]
    home_expected <- calc_expected(elo_diff)
    home_favored <- home_expected > 0.5
    home_won <- g$home_score > g$away_score
    
    if (home_favored == home_won) correct <- correct + 1
    total <- total + 1
    
    # Update elo for next game
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

cat("\n========================================\n")
cat("PREDICTION ACCURACY\n")
cat("========================================\n")

eval_2023 <- evaluate_predictions(games_2023)
eval_2024 <- evaluate_predictions(games_2024, elos_entering_2024)
eval_2025 <- evaluate_predictions(games_2025, elos_entering_2025)
eval_2026 <- evaluate_predictions(games_2026, elos_entering_2026)

if (!is.null(eval_2023)) cat(sprintf("2023: %.1f%% (%d/%d)\n", eval_2023$accuracy * 100, eval_2023$correct, eval_2023$total))
if (!is.null(eval_2024)) cat(sprintf("2024: %.1f%% (%d/%d)\n", eval_2024$accuracy * 100, eval_2024$correct, eval_2024$total))
if (!is.null(eval_2025)) cat(sprintf("2025: %.1f%% (%d/%d)\n", eval_2025$accuracy * 100, eval_2025$correct, eval_2025$total))
if (!is.null(eval_2026)) cat(sprintf("2026: %.1f%% (%d/%d)\n", eval_2026$accuracy * 100, eval_2026$correct, eval_2026$total))

# ============================================
# SAVE
# ============================================

write.csv(elo_2023$ratings, "elo_2023.csv", row.names = FALSE)
write.csv(elo_2024$ratings, "elo_2024.csv", row.names = FALSE)
write.csv(elo_2025$ratings, "elo_2025.csv", row.names = FALSE)
if (nrow(elo_2026$ratings) > 0) write.csv(elo_2026$ratings, "elo_2026.csv", row.names = FALSE)

cat("\nSaved: elo_2023.csv, elo_2024.csv, elo_2025.csv")
if (nrow(elo_2026$ratings) > 0) cat(", elo_2026.csv")
cat("\n")