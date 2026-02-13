# ===== FINAL RANKINGS MULTI YEAR =====
# RPI Top 20 Filter → Dynamic RPI/ELO Combined

# ================================
# PARAMETERS
# ================================
YEARS <- c(2023, 2024, 2025, 2026)

# ================================
# LOAD DATA
# ================================

load_ratings <- function(year) {
  rpi <- tryCatch(
    read.csv(paste0("rpi_", year, ".csv"), stringsAsFactors = FALSE),
    error = function(e) data.frame()
  )
  elo <- tryCatch(
    read.csv(paste0("elo_", year, ".csv"), stringsAsFactors = FALSE),
    error = function(e) data.frame()
  )
  return(list(rpi = rpi, elo = elo))
}

all_ratings <- list()
for (y in YEARS) {
  all_ratings[[as.character(y)]] <- load_ratings(y)
}

# ================================
# FUNCTION
# ================================
run_rankings <- function(rpi, elo, year, top_n = 25) {
  
  if (nrow(rpi) == 0 || nrow(elo) == 0) {
    cat(sprintf("\n%d: Not enough data for combined rankings\n", year))
    return()
  }
  
  rpi$rpi <- as.numeric(rpi$rpi)
  
  # Calculate average games played to set dynamic weights
  avg_games <- mean(rpi$wins + rpi$losses)
  
  # Dynamic weights: RPI barely counts until ~5 avg games, reaches 60% by ~15 games
  rpi_weight <- min(0.60, max(0, (avg_games - 3) * 0.05))
  elo_weight <- 1 - rpi_weight
  
  cat(sprintf("\nAvg games played: %.1f | Weights: %.0f%% RPI / %.0f%% ELO\n",
              avg_games, rpi_weight * 100, elo_weight * 100))
  
  # Start with ALL teams from ELO
  rankings <- elo[, c("team", "elo")]
  
  # Merge in RPI data where available
  rankings <- merge(rankings,
                    rpi[, c("team", "wins", "losses", "rpi")],
                    by = "team", all.x = TRUE)
  
  # Fill in teams with no games yet
  rankings$wins[is.na(rankings$wins)] <- 0
  rankings$losses[is.na(rankings$losses)] <- 0
  rankings$rpi[is.na(rankings$rpi)] <- NA
  
  rankings$games_played <- rankings$wins + rankings$losses
  
  # Normalize ELO
  rankings$elo_norm <- (rankings$elo - min(rankings$elo)) /
    (max(rankings$elo) - min(rankings$elo))
  
  # For teams with RPI, normalize it
  has_rpi <- !is.na(rankings$rpi)
  
  if (sum(has_rpi) > 1 && max(rankings$rpi[has_rpi]) != min(rankings$rpi[has_rpi])) {
    rpi_min <- min(rankings$rpi[has_rpi])
    rpi_max <- max(rankings$rpi[has_rpi])
    rankings$rpi_norm <- ifelse(has_rpi,
                                (rankings$rpi - rpi_min) / (rpi_max - rpi_min),
                                NA)
  } else {
    rankings$rpi_norm <- NA
  }
  
  # Calculate combined score
  rankings$score <- ifelse(!is.na(rankings$rpi_norm),
                           rpi_weight * rankings$rpi_norm + elo_weight * rankings$elo_norm,
                           elo_weight * rankings$elo_norm)
  
  # Sort and take top N
  rankings <- rankings[order(-rankings$score), ]
  rownames(rankings) <- NULL
  
  available <- min(top_n, nrow(rankings))
  rankings <- rankings[1:available, ]
  
  # ================================
  # PRINT
  # ================================
  
  cat("\n=============================================\n")
  cat(year, "FINAL RANKINGS - TOP", available, "\n")
  cat("=============================================\n\n")
  
  cat(sprintf("%2s  %-25s  %5s  %6s  %5s  %5s\n",
              "#", "Team", "W-L", "RPI", "ELO", "Score"))
  cat(paste(rep("-", 65), collapse = ""), "\n")
  
  for (i in 1:available) {
    r <- rankings[i, ]
    rpi_str <- ifelse(is.na(r$rpi), "  N/A", sprintf("%.4f", r$rpi))
    cat(sprintf("%2d  %-25s  %2d-%-2d  %s  %.0f  %.3f\n",
                i,
                substr(r$team, 1, 25),
                r$wins, r$losses,
                rpi_str,
                r$elo,
                r$score))
  }
  
  write.csv(rankings[, c("team","wins","losses","rpi","elo","score")],
            paste0("final_top25_", year, ".csv"),
            row.names = FALSE)
}

# ================================
# RUN ALL YEARS
# ================================
for (y in YEARS) {
  r <- all_ratings[[as.character(y)]]
  run_rankings(r$rpi, r$elo, y)
}