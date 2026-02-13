# ===== SHARED UTILITIES =====
# Source this file from other scripts: source("utils.R")

# ============================================
# CONSTANTS
# ============================================
INITIAL_ELO <- 1500
HFA <- 20

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
# SHARED FUNCTIONS
# ============================================

convert_name <- function(espn_name) {
  if (espn_name %in% names(name_map)) return(name_map[espn_name])
  return(espn_name)
}

calc_expected <- function(elo_diff) {
  1 / (1 + 10^(-elo_diff / 400))
}

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
