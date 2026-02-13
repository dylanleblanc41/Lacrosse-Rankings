# ===== NCAA LACROSSE RPI RANKINGS =====
source("utils.R")

# ============================================
# PARAMETERS
# ============================================
YEARS <- c(2023, 2024, 2025, 2026)

all_games <- list()
for (y in YEARS) {
  all_games[[as.character(y)]] <- load_games(y)
}

# ============================================
# RPI CALCULATION
# ============================================

calculate_rpi <- function(games, min_games = 8) {
  
  if (nrow(games) == 0) return(data.frame())
  
  teams <- unique(c(games$away_team, games$home_team))
  n <- length(teams)
  
  wins <- setNames(rep(0, n), teams)
  losses <- setNames(rep(0, n), teams)
  
  h2h_wins <- matrix(0, nrow = n, ncol = n, dimnames = list(teams, teams))
  
  for (i in 1:nrow(games)) {
    g <- games[i, ]
    if (is.na(g$away_score) || is.na(g$home_score)) next
    
    away <- g$away_team
    home <- g$home_team
    
    if (g$away_score > g$home_score) {
      wins[away] <- wins[away] + 1
      losses[home] <- losses[home] + 1
      h2h_wins[away, home] <- h2h_wins[away, home] + 1
    } else {
      wins[home] <- wins[home] + 1
      losses[away] <- losses[away] + 1
      h2h_wins[home, away] <- h2h_wins[home, away] + 1
    }
  }
  
  h2h_games <- h2h_wins + t(h2h_wins)
  total_games <- wins + losses
  wp <- ifelse(total_games > 0, wins / total_games, 0)
  
  owp <- sapply(teams, function(team) {
    opps <- teams[h2h_games[team, ] > 0]
    if (length(opps) == 0) return(0)
    
    opp_wps <- sapply(opps, function(opp) {
      opp_wins_adj <- wins[opp] - h2h_wins[opp, team]
      opp_losses_adj <- losses[opp] - h2h_wins[team, opp]
      opp_total_adj <- opp_wins_adj + opp_losses_adj
      if (opp_total_adj <= 0) return(0.5)
      opp_wins_adj / opp_total_adj
    })
    
    games_vs_opp <- h2h_games[team, opps]
    sum(opp_wps * games_vs_opp) / sum(games_vs_opp)
  })
  
  oowp <- sapply(teams, function(team) {
    opps <- teams[h2h_games[team, ] > 0]
    if (length(opps) == 0) return(0)
    mean(owp[opps])
  })
  
  rpi <- 0.25 * wp + 0.50 * owp + 0.25 * oowp
  
  results <- data.frame(
    team = teams,
    wins = as.integer(wins[teams]),
    losses = as.integer(losses[teams]),
    win_pct = round(wp[teams], 3),
    owp = round(owp, 3),
    oowp = round(oowp, 3),
    rpi = round(rpi, 4),
    stringsAsFactors = FALSE
  )
  
  results <- results[(results$wins + results$losses) >= min_games, ]
  
  results <- results[order(-results$rpi), ]
  results$rank <- 1:nrow(results)
  results <- results[, c("rank", "team", "wins", "losses", "win_pct", "owp", "oowp", "rpi")]
  rownames(results) <- NULL
  
  return(results)
}

# ============================================
# PRINT
# ============================================

print_rpi <- function(rpi, year) {
  if (nrow(rpi) == 0) {
    cat(sprintf("\n%d: Not enough games yet for RPI\n", year))
    return()
  }
  
  cat(sprintf("\n========================================\n"))
  cat(sprintf("%d RPI RANKINGS - TOP 25\n", year))
  cat(sprintf("========================================\n\n"))
  
  cat(sprintf("%-4s %-28s %5s %6s %6s %6s %7s\n",
              "Rank", "Team", "W-L", "WP", "OWP", "OOWP", "RPI"))
  cat(paste(rep("-", 72), collapse = ""), "\n")
  
  for (i in 1:min(25, nrow(rpi))) {
    r <- rpi[i, ]
    cat(sprintf("%-4d %-28s %2d-%-2d %5.3f %5.3f %5.3f %6.4f\n",
                r$rank,
                substr(r$team, 1, 28),
                r$wins, r$losses,
                r$win_pct,
                r$owp,
                r$oowp,
                r$rpi))
  }
}

# ============================================
# RUN + SAVE FOR ALL YEARS
# ============================================

cat("\nCalculating RPI...\n")

rpi_results <- list()

for (y in as.character(YEARS)) {
  # Use min_games = 1 for current season, 8 for completed seasons
  min_g <- ifelse(as.integer(y) >= 2026, 1, 8)
  rpi_results[[y]] <- calculate_rpi(all_games[[y]], min_games = min_g)
  print_rpi(rpi_results[[y]], as.integer(y))
}

# Save
for (y in as.character(YEARS)) {
  if (nrow(rpi_results[[y]]) > 0) {
    write.csv(rpi_results[[y]], paste0("rpi_", y, ".csv"), row.names = FALSE)
  }
}

cat("\nSaved RPI ratings for all years.\n")