# ===== NCAA LACROSSE SCRAPER - HANDLES FLAKY API =====
# The NCAA API returns inconsistent results (sometimes 404 for valid dates)
# This scraper retries aggressively to get all the data

library(jsonlite)
library(httr)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x[1]))) y else x

# Fetch with aggressive retries
fetch_games_for_date <- function(date_str, max_attempts = 5) {
  
  url <- paste0(
    "https://data.ncaa.com/casablanca/scoreboard/lacrosse-men/d1/",
    date_str,
    "/scoreboard.json"
  )
  
  for (attempt in 1:max_attempts) {
    Sys.sleep(0.2 * attempt)  # Increasing delay between attempts
    
    resp <- try(GET(url, timeout(30)), silent = TRUE)
    
    if (inherits(resp, "try-error")) next
    
    if (status_code(resp) == 200) {
      json <- try(fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE), silent = TRUE)
      if (!inherits(json, "try-error") && !is.null(json$games)) {
        return(list(success = TRUE, games = json$games))
      }
    }
    
    # If we got 404, try again - the API is inconsistent
    if (status_code(resp) == 404 && attempt < max_attempts) {
      next
    }
  }
  
  return(list(success = FALSE, games = list()))
}

# Main scraper
scrape_year <- function(year) {
  
  cat(sprintf("\n========================================\n"))
  cat(sprintf("SCRAPING %d (with retry logic)\n", year))
  cat(sprintf("========================================\n"))
  
  dates <- seq(as.Date(sprintf("%d-02-01", year)), 
               as.Date(sprintf("%d-05-31", year)), 
               by = "day")
  
  all_games <- list()
  dates_success <- 0
  dates_failed <- 0
  
  for (i in seq_along(dates)) {
    d <- dates[i]
    date_str <- format(d, "%Y/%m/%d")
    
    result <- fetch_games_for_date(date_str, max_attempts = 5)
    
    if (result$success && length(result$games) > 0) {
      dates_success <- dates_success + 1
      
      for (g in result$games) {
        game <- g$game
        if (is.null(game)) next
        
        away <- game$away
        home <- game$home
        if (is.null(away) || is.null(home)) next
        
        away_name <- away$names$full %||% away$names$seo %||% away$names$short %||% NA
        home_name <- home$names$full %||% home$names$seo %||% home$names$short %||% NA
        
        if (is.na(away_name) || is.na(home_name)) next
        
        all_games[[length(all_games) + 1]] <- data.frame(
          date = as.character(d),
          game_id = game$gameID %||% NA,
          away_team = away_name,
          home_team = home_name,
          away_score = as.integer(away$score %||% NA),
          home_score = as.integer(home$score %||% NA),
          stringsAsFactors = FALSE
        )
      }
    }
    
    if (i %% 10 == 0) {
      cat(sprintf("  %d/%d dates | %d games\n", i, length(dates), length(all_games)))
    }
  }
  
  if (length(all_games) == 0) return(data.frame())
  
  df <- do.call(rbind, all_games)
  df$date <- as.Date(df$date)
  
  cat(sprintf("\n%d FINAL: %d games from %s to %s\n", 
              year, nrow(df), min(df$date), max(df$date)))
  
  return(df)
}

# Run for both years
games_2024 <- scrape_year(2024)
games_2025 <- scrape_year(2025)
# games_2026 <- scrape_year(2026)

# Summary
cat("\n========================================\n")
cat("FINAL RESULTS\n")
cat("========================================\n")
cat(sprintf("2024: %d games\n", nrow(games_2024)))
cat(sprintf("2025: %d games\n", nrow(games_2025)))
# cat(sprintf("2026: %d games\n", nrow(games_2026)))


cat("\n2024 by month:\n")
print(table(format(games_2024$date, "%B")))

cat("\n2025 by month:\n")
print(table(format(games_2025$date, "%B")))

#cat("\n2026 by month:\n")
#print(table(format(games_2026$date, "%B")))

write.csv(games_2024, "ncaa_lax_2024.csv", row.names = FALSE)
write.csv(games_2025, "ncaa_lax_2025.csv", row.names = FALSE)
#write.csv(games_2026, "ncaa_lax_2025.csv", row.names = FALSE)
cat("\nSaved to ncaa_lax_2024.csv and ncaa_lax_2025.csv\n")