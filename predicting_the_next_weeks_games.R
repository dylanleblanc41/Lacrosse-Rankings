# ===== NCAA LACROSSE WEEKLY PREDICTIONS =====
library(httr)
library(jsonlite)
source("utils.R")

# ============================================
# LOAD ELO RATINGS
# ============================================

elo_2026 <- read.csv("elo_2026.csv", stringsAsFactors = FALSE)
elo_vector <- setNames(elo_2026$elo, elo_2026$team)

cat("Loaded", length(elo_vector), "team ratings\n\n")

# ============================================
# FETCH THIS WEEK'S GAMES FROM ESPN
# ============================================

fetch_espn_games <- function(date_str) {
  url <- paste0(
    "https://site.api.espn.com/apis/site/v2/sports/lacrosse/mens-college-lacrosse/scoreboard?dates=",
    date_str
  )

  resp <- try(GET(url, timeout(30)), silent = TRUE)
  if (inherits(resp, "try-error")) return(NULL)
  if (status_code(resp) != 200) return(NULL)

  json <- fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  if (is.null(json$events) || length(json$events) == 0) return(NULL)

  games <- lapply(json$events, function(event) {
    comp <- event$competitions[[1]]
    teams <- comp$competitors

    home <- NULL
    away <- NULL
    for (t in teams) {
      if (t$homeAway == "home") home <- t
      if (t$homeAway == "away") away <- t
    }

    if (is.null(home) || is.null(away)) return(NULL)

    data.frame(
      date = substr(event$date, 1, 10),
      time = comp$status$type$shortDetail,
      away_espn = away$team$displayName,
      home_espn = home$team$displayName,
      away_team = convert_name(away$team$displayName),
      home_team = convert_name(home$team$displayName),
      away_score = as.integer(away$score),
      home_score = as.integer(home$score),
      status = comp$status$type$name,
      stringsAsFactors = FALSE
    )
  })

  games <- games[!sapply(games, is.null)]
  if (length(games) == 0) return(NULL)
  do.call(rbind, games)
}

cat("========================================\n")
cat("FETCHING THIS WEEK'S GAMES\n")
cat("========================================\n\n")

# Fetch next 7 days
week_dates <- seq(Sys.Date(), Sys.Date() + 6, by = "day")
all_games <- list()

for (d in week_dates) {
  date_str <- format(as.Date(d, origin = "1970-01-01"), "%Y%m%d")
  games <- fetch_espn_games(date_str)
  if (!is.null(games) && nrow(games) > 0) {
    all_games[[length(all_games) + 1]] <- games
  }
  Sys.sleep(0.3)
}

if (length(all_games) == 0) {
  cat("No games found this week.\n")
} else {
  schedule <- do.call(rbind, all_games)
  schedule <- unique(schedule)

  # Split into upcoming and completed
  upcoming <- schedule[schedule$status == "STATUS_SCHEDULED", ]
  completed <- schedule[schedule$status == "STATUS_FINAL", ]

  # ============================================
  # PREDICT UPCOMING GAMES
  # ============================================

  if (nrow(upcoming) > 0) {
    cat("========================================\n")
    cat("PREDICTIONS - UPCOMING GAMES\n")
    cat("========================================\n\n")

    cat(sprintf("%-10s  %-22s  %-22s  %6s %6s  %-25s\n",
                "Date", "Away", "Home", "Away%", "Home%", "Pick"))
    cat(paste(rep("-", 95), collapse = ""), "\n")

    predictions <- data.frame()

    for (i in 1:nrow(upcoming)) {
      g <- upcoming[i, ]

      home_elo <- ifelse(g$home_team %in% names(elo_vector), elo_vector[g$home_team], INITIAL_ELO)
      away_elo <- ifelse(g$away_team %in% names(elo_vector), elo_vector[g$away_team], INITIAL_ELO)

      elo_diff <- home_elo + HFA - away_elo
      home_prob <- calc_expected(elo_diff)
      away_prob <- 1 - home_prob

      if (home_prob > away_prob) {
        pick <- g$home_team
        pick_prob <- round(home_prob * 100, 1)
      } else {
        pick <- g$away_team
        pick_prob <- round(away_prob * 100, 1)
      }

      cat(sprintf("%-10s  %-22s  %-22s  %5.1f%% %5.1f%%  %s (%s%%)\n",
                  g$date,
                  substr(g$away_espn, 1, 22),
                  substr(g$home_espn, 1, 22),
                  round(away_prob * 100, 1),
                  round(home_prob * 100, 1),
                  substr(pick, 1, 25),
                  pick_prob))

      predictions <- rbind(predictions, data.frame(
        date = g$date,
        away_team = g$away_team,
        home_team = g$home_team,
        away_elo = round(away_elo, 0),
        home_elo = round(home_elo, 0),
        away_prob = round(away_prob * 100, 1),
        home_prob = round(home_prob * 100, 1),
        pick = pick,
        pick_prob = pick_prob,
        stringsAsFactors = FALSE
      ))
    }

    write.csv(predictions, paste0("predictions_week_", format(Sys.Date(), "%Y%m%d"), ".csv"), row.names = FALSE)
    cat(sprintf("\nSaved %d predictions to predictions_week_%s.csv\n", nrow(predictions), format(Sys.Date(), "%Y%m%d")))
  }

  # ============================================
  # SHOW COMPLETED RESULTS + ACCURACY
  # ============================================

  if (nrow(completed) > 0) {
    cat("\n========================================\n")
    cat("COMPLETED GAMES - RESULTS VS PREDICTIONS\n")
    cat("========================================\n\n")

    correct <- 0
    total <- 0

    cat(sprintf("%-10s  %-22s  %-22s  %5s  %s\n",
                "Date", "Away", "Home", "Score", "Prediction"))
    cat(paste(rep("-", 85), collapse = ""), "\n")

    for (i in 1:nrow(completed)) {
      g <- completed[i, ]

      home_elo <- ifelse(g$home_team %in% names(elo_vector), elo_vector[g$home_team], INITIAL_ELO)
      away_elo <- ifelse(g$away_team %in% names(elo_vector), elo_vector[g$away_team], INITIAL_ELO)

      elo_diff <- home_elo + HFA - away_elo
      home_prob <- calc_expected(elo_diff)

      predicted_home <- home_prob > 0.5
      actual_home_win <- g$home_score > g$away_score

      hit <- predicted_home == actual_home_win
      if (hit) correct <- correct + 1
      total <- total + 1

      result <- ifelse(hit, "HIT", "MISS")

      cat(sprintf("%-10s  %-22s  %-22s  %2d-%-2d  %s\n",
                  g$date,
                  substr(g$away_espn, 1, 22),
                  substr(g$home_espn, 1, 22),
                  g$away_score, g$home_score,
                  result))
    }

    cat(sprintf("\nAccuracy: %d/%d (%.1f%%)\n", correct, total, correct/total * 100))
  }
}
