# ===== NCAA LACROSSE RPI RANKINGS (FIXED) =====

# Load data
games_2024 <- read.csv("ncaa_lax_2024.csv", stringsAsFactors = FALSE)
games_2025 <- read.csv("ncaa_lax_2025.csv", stringsAsFactors = FALSE)
games_2024$date <- as.Date(games_2024$date)
games_2025$date <- as.Date(games_2025$date)

cat(sprintf("2024: %d games\n", nrow(games_2024)))
cat(sprintf("2025: %d games\n", nrow(games_2025)))

# ============================================
# RPI CALCULATION (FIXED)
# ============================================

calculate_rpi <- function(games) {
  
  teams <- unique(c(games$away_team, games$home_team))
  n <- length(teams)
  
  # Initialize
  wins <- setNames(rep(0, n), teams)
  losses <- setNames(rep(0, n), teams)
  
  # Head-to-head matrix: h2h[i,j] = wins by team i against team j
  h2h_wins <- matrix(0, nrow = n, ncol = n, dimnames = list(teams, teams))
  
  # Process all games
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
  
  # Games played against each opponent
  h2h_games <- h2h_wins + t(h2h_wins)
  
  # Total games
  total_games <- wins + losses
  
  # Winning Percentage
  wp <- ifelse(total_games > 0, wins / total_games, 0)
  
  # Opponents' Winning Percentage (OWP)
  # For each team, calculate average WP of opponents excluding games vs that team
  owp <- sapply(teams, function(team) {
    
    # Who did this team play?
    opps <- teams[h2h_games[team, ] > 0]
    
    if (length(opps) == 0) return(0)
    
    opp_wps <- sapply(opps, function(opp) {
      # Opponent's wins/losses excluding games against this team
      opp_wins_adj <- wins[opp] - h2h_wins[opp, team]
      opp_losses_adj <- losses[opp] - h2h_wins[team, opp]
      
      opp_total_adj <- opp_wins_adj + opp_losses_adj
      
      if (opp_total_adj <= 0) return(0.5)
      
      opp_wins_adj / opp_total_adj
    })
    
    # Weight by number of games against each opponent
    games_vs_opp <- h2h_games[team, opps]
    
    sum(opp_wps * games_vs_opp) / sum(games_vs_opp)
  })
  
  # Opponents' Opponents' Winning Percentage (OOWP)
  oowp <- sapply(teams, function(team) {
    opps <- teams[h2h_games[team, ] > 0]
    
    if (length(opps) == 0) return(0)
    
    mean(owp[opps])
  })
  
  # RPI = 0.25*WP + 0.50*OWP + 0.25*OOWP
  rpi <- 0.25 * wp + 0.50 * owp + 0.25 * oowp
  
  # Build results
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
  
  results <- results[order(-results$rpi), ]
  results$rank <- 1:nrow(results)
  results <- results[, c("rank", "team", "wins", "losses", "win_pct", "owp", "oowp", "rpi")]
  rownames(results) <- NULL
  
  return(results)
}

# ============================================
# RUN
# ============================================

cat("\nCalculating RPI...\n")
rpi_2024 <- calculate_rpi(games_2024)
rpi_2025 <- calculate_rpi(games_2025)

# ============================================
# PRINT TOP 25
# ============================================

print_rpi <- function(rpi, year) {
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

print_rpi(rpi_2024, 2024)
print_rpi(rpi_2025, 2025)

# ============================================
# SAVE
# ============================================

write.csv(rpi_2024, "rpi_2024.csv", row.names = FALSE)
write.csv(rpi_2025, "rpi_2025.csv", row.names = FALSE)

cat("\n\nSaved: rpi_2024.csv, rpi_2025.csv\n")
