# ===== NCAA LACROSSE ELO RATING SYSTEM =====
# Based on FiveThirtyEight's NFL ELO (https://github.com/fivethirtyeight/nfl-elo-game)
# and nfelo (https://github.com/greerreNFL/nfelo)

# ============================================
# PARAMETERS
# ============================================
K_FACTOR <- 30
HFA <- 20
INITIAL_ELO <- 1500
REVERSION <- 0.20
YEARS <- c(2023, 2024, 2025, 2026)

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