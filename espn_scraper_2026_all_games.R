# ===== NCAA LACROSSE SCRAPER - ESPN API =====
library(jsonlite)
library(httr)

fetch_espn_games <- function(date_str, max_attempts = 3) {
  url <- paste0(
    "https://site.api.espn.com/apis/site/v2/sports/lacrosse/mens-college-lacrosse/scoreboard?dates=",
    date_str
  )
  
  for (attempt in 1:max_attempts) {
    Sys.sleep(0.3 * attempt)
    resp <- try(GET(url, timeout(30)), silent = TRUE)
    
    if (inherits(resp, "try-error")) next
    
    if (status_code(resp) == 200) {
      json <- try(fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE), silent = TRUE)
      if (!inherits(json, "try-error") && !is.null(json$events)) {
        return(list(success = TRUE, events = json$events))
      }
    }
  }
  
  return(list(success = FALSE, events = list()))
}

scrape_espn_year <- function(year) {
  
  cat(sprintf("\n========================================\n"))
  cat(sprintf("SCRAPING %d (ESPN API)\n", year))
  cat(sprintf("========================================\n"))
  
  dates <- seq(as.Date(sprintf("%d-01-01", year)),
               as.Date(sprintf("%d-06-01", year)),
               by = "day")
  
  all_games <- list()
  
  for (i in seq_along(dates)) {
    d <- dates[i]
    date_str <- format(d, "%Y%m%d")
    
    result <- fetch_espn_games(date_str)
    
    if (result$success && length(result$events) > 0) {
      for (event in result$events) {
        comp <- event$competitions[[1]]
        if (is.null(comp)) next
        
        teams <- comp$competitors
        if (is.null(teams) || length(teams) < 2) next
        
        home <- NULL
        away <- NULL
        for (t in teams) {
          if (t$homeAway == "home") home <- t
          if (t$homeAway == "away") away <- t
        }
        
        if (is.null(home) || is.null(away)) next
        
        all_games[[length(all_games) + 1]] <- data.frame(
          date = as.character(d),
          game_id = event$id,
          away_team = away$team$displayName,
          home_team = home$team$displayName,
          away_score = as.integer(away$score),
          home_score = as.integer(home$score),
          status = comp$status$type$name,
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

# Run for 2026
games_2026 <- scrape_espn_year(2026)

cat("\n========================================\n")
cat("FINAL RESULTS\n")
cat("========================================\n")
cat(sprintf("2026: %d games\n", nrow(games_2026)))

cat("\n2026 by month:\n")
print(table(format(games_2026$date, "%B")))

write.csv(games_2026, "ncaa_lax_2026.csv", row.names = FALSE)
cat("\nSaved to ncaa_lax_2026.csv\n")
