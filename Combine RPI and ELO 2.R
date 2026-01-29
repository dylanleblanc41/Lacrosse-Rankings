# ===== FINAL RANKINGS MULTI YEAR =====
# RPI Top 20 Filter → 60% RPI + 40% ELO Combined


# ================================
# LOAD DATA
# ================================

rpi_2023 <- read.csv("rpi_2023.csv", stringsAsFactors = FALSE)
elo_2023 <- read.csv("elo_2023.csv", stringsAsFactors = FALSE)

rpi_2024 <- read.csv("rpi_2024.csv", stringsAsFactors = FALSE)
elo_2024 <- read.csv("elo_2024.csv", stringsAsFactors = FALSE)

rpi_2025 <- read.csv("rpi_2025.csv", stringsAsFactors = FALSE)
elo_2025 <- read.csv("elo_2025.csv", stringsAsFactors = FALSE)


# ================================
# FUNCTION
# ================================

run_rankings <- function(rpi, elo, year) {
  
  # make sure rpi is numeric
  rpi$rpi <- as.numeric(rpi$rpi)
  
  # RPI cutoff
  rpi$rpi_rank <- rank(-rpi$rpi)
  eligible <- rpi$team[rpi$rpi_rank <= 20]
  
  rpi <- rpi[rpi$team %in% eligible, ]
  elo <- elo[elo$team %in% eligible, ]
  
  # Merge
  rankings <- merge(
    rpi[, c("team", "wins", "losses", "rpi")],
    elo[, c("team", "elo")],
    by = "team"
  )
  
  # Normalize
  rankings$rpi_norm <- (rankings$rpi - min(rankings$rpi)) /
    (max(rankings$rpi) - min(rankings$rpi))
  
  rankings$elo_norm <- (rankings$elo - min(rankings$elo)) /
    (max(rankings$elo) - min(rankings$elo))
  
  # Combined score
  rankings$score <- 0.60 * rankings$rpi_norm + 0.40 * rankings$elo_norm
  
  # Sort
  rankings <- rankings[order(-rankings$score), ]
  rownames(rankings) <- NULL
  
  
  # ================================
  # PRINT TOP 20
  # ================================
  
  cat("\n=============================================\n")
  cat(year, "FINAL RANKINGS - TOP 20\n")
  cat("=============================================\n\n")
  
  for (i in 1:20) {
    r <- rankings[i, ]
    cat(sprintf("%2d  %-25s  %2d-%-2d  %.4f  %.0f  %.3f\n",
                i,
                substr(r$team, 1, 25),
                r$wins, r$losses,
                r$rpi,
                r$elo,
                r$score))
  }
  
  # Save
  write.csv(rankings[, c("team","wins","losses","rpi","elo","score")],
            paste0("final_top20_", year, ".csv"),
            row.names = FALSE)
}


# ================================
# RUN ALL YEARS
# ================================

run_rankings(rpi_2023, elo_2023, 2023)
run_rankings(rpi_2024, elo_2024, 2024)
run_rankings(rpi_2025, elo_2025, 2025)
