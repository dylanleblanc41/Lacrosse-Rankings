# ===== NCAA LACROSSE RPI RANKINGS =====

# ============================================
# ESPN TO NCAA NAME MAPPING
# ============================================
name_map <- c(
  "Air Force Falcons" = "U.S. Air Force Academy",
  "Army Black Knights" = "U.S. Military Academy",
  "Bellarmine Knights" = "Bellarmine University",
  "Boston University Terriers" = "Boston University",
  "Bryant Bulldogs" = "Bryant University",
  "Bucknell Bison" = "Bucknell University",
  "Canisius Golden Griffins" = "Canisius University",
  "Cleveland State Vikings" = "Cleveland State University",
  "Colgate Raiders" = "Colgate University",
  "Delaware Blue Hens" = "University of Delaware",
  "Denver Pioneers" = "University of Denver",
  "Detroit Mercy Titans" = "University of Detroit Mercy",
  "Drexel Dragons" = "Drexel University",
  "Duke Blue Devils" = "Duke University",
  "Fairfield Stags" = "Fairfield University",
  "Hampton Pirates" = "Hampton University",
  "High Point Panthers" = "High Point University",
  "Hobart College Statesmen" = "Hobart College",
  "Hofstra Pride" = "Hofstra University",
  "Holy Cross Crusaders" = "College of the Holy Cross",
  "Iona Gaels" = "Iona University",
  "Jacksonville Dolphins" = "Jacksonville University",
  "Johns Hopkins University Blue Jays" = "Johns Hopkins University",
  "Lafayette Leopards" = "Lafayette College",
  "Le Moyne Dolphins" = "Le Moyne College",
  "Lehigh Mountain Hawks" = "Lehigh University",
  "Long Island University Sharks" = "Long Island University",
  "Loyola Maryland Greyhounds" = "Loyola University Maryland",
  "Manhattan Jaspers" = "Manhattan University",
  "Marist Red Foxes" = "Marist University",
  "Marquette Golden Eagles" = "Marquette University",
  "Maryland Terrapins" = "University of Maryland, College Park",
  "Massachusetts Minutemen" = "University of Massachusetts, Amherst",
  "Mercer Bears" = "Mercer University",
  "Mercyhurst Lakers" = "Mercyhurst University",
  "Merrimack Warriors" = "Merrimack College",
  "Michigan Wolverines" = "University of Michigan",
  "Monmouth Hawks" = "Monmouth University",
  "Mount St. Mary's Mountaineers" = "Mount St. Mary's University",
  "Navy Midshipmen" = "U.S. Naval Academy",
  "NJIT Highlanders" = "New Jersey Institute of Technology",
  "North Carolina Tar Heels" = "University of North Carolina, Chapel Hill",
  "Ohio State Buckeyes" = "The Ohio State University",
  "Penn State Nittany Lions" = "Pennsylvania State University",
  "Providence Friars" = "Providence College",
  "Queens University Royals" = "Queens University of Charlotte",
  "Quinnipiac Bobcats" = "Quinnipiac University",
  "Richmond Spiders" = "University of Richmond",
  "Robert Morris Colonials" = "Robert Morris University",
  "Rutgers Scarlet Knights" = "Rutgers, The State University of New Jersey, New Brunswick",
  "Sacred Heart Pioneers" = "Sacred Heart University",
  "Saint Joseph's Hawks" = "Saint Joseph's University",
  "Siena Saints" = "Siena College",
  "St. Bonaventure Bonnies" = "St. Bonaventure University",
  "St. John's Red Storm" = "St. John's University (New York)",
  "Stony Brook Seawolves" = "Stony Brook University",
  "Syracuse Orange" = "Syracuse University",
  "UMass Lowell River Hawks" = "University of Massachusetts Lowell",
  "Utah Utes" = "University of Utah",
  "Villanova Wildcats" = "Villanova University",
  "Virginia Cavaliers" = "University of Virginia",
  "VMI Keydets" = "Virginia Military Institute"
)

# ============================================
# PARAMETERS
# ============================================
YEARS <- c(2023, 2024, 2025, 2026)

# ============================================
# LOAD DATA
# ============================================

load_games <- function(year) {
  file <- paste0("ncaa_lax_", year, ".csv")
  games <- tryCatch(
    read.csv(file, stringsAsFactors = FALSE),
    error = function(e) data.frame()
  )
  
  if (nrow(games) == 0) return(games)
  
  games$date <- as.Date(games$date)
  
  if (year >= 2026) {
    for (col in c("away_team", "home_team")) {
      matched <- name_map[games[[col]]]
      games[[col]] <- ifelse(!is.na(matched), matched, games[[col]])
    }
    games <- games[games$status == "STATUS_FINAL", ]
  }
  
  cat(sprintf("%d: %d games\n", year, nrow(games)))
  return(games)
}

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